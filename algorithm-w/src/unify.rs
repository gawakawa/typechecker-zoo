use std::collections::HashMap;

use crate::{
    ast::Type,
    error::{InferenceError, Result},
};

use super::{InferenceTree, Subst, TyVar, TypeInference};

impl TypeInference {
    pub(crate) fn unify(t1: &Type, t2: &Type) -> Result<(Subst, InferenceTree)> {
        let input = format!("{} - {}", t1, t2);

        match (t1, t2) {
            (Type::Int, Type::Int) | (Type::Bool, Type::Bool) => Self::unify_lit(&input),
            (Type::Var(v), ty) | (ty, Type::Var(v)) => Self::unify_var(v, ty, &input),
            (Type::Arrow(a1, a2), Type::Arrow(b1, b2)) => Self::unify_arrow(a1, a2, b1, b2, &input),
            (Type::Tuple(ts1), Type::Tuple(ts2)) => Self::unify_tuple(ts1, ts2, &input),
            _ => Err(InferenceError::UnificationFailure {
                expected: t1.clone(),
                actual: t2.clone(),
            }),
        }
    }

    // ───────────────────── (U-Int, U-Bool)
    // unify(τ, τ) = ∅
    fn unify_lit(input: &str) -> Result<(Subst, InferenceTree)> {
        let tree = InferenceTree::new("Unify-Base", input, "{}", vec![]);
        Ok((HashMap::new(), tree))
    }

    // α ∉ ftv(τ)
    // ────────────────────── (U-VarL, U-VarR)
    // unify(α, τ) = [α ↦ τ]
    fn unify_var(v: &TyVar, ty: &Type, input: &str) -> Result<(Subst, InferenceTree)> {
        if ty == &Type::Var(v.clone()) {
            let tree = InferenceTree::new("Unify-Var-Same", input, "{}", vec![]);
            Ok((HashMap::new(), tree))
        } else if Self::occurs_check(v, ty) {
            Err(InferenceError::OccursCheck {
                var: v.clone(),
                ty: ty.clone(),
            })
        } else {
            let mut subst = HashMap::new();
            subst.insert(v.clone(), ty.clone());
            let output = format!("{{{}/{}}}", ty, v);
            let tree = InferenceTree::new("Unify-Var", input, &output, vec![]);
            Ok((subst, tree))
        }
    }

    // S₁ = unify(τ₁, τ₃)    S₂ = unify(S₁(τ₂), S₁(τ₄))
    // ───────────────────────────────────────────────── (U-Arrow)
    // unify(τ₁ → τ₂, τ₃ → τ₄) = S₂ ∘ S₁
    fn unify_arrow(
        a1: &Type,
        a2: &Type,
        b1: &Type,
        b2: &Type,
        input: &str,
    ) -> Result<(Subst, InferenceTree)> {
        let (s1, tree1) = Self::unify(a1, b1)?;
        let a2_subst = Self::apply_subst(&s1, a2);
        let b2_subst = Self::apply_subst(&s1, b2);
        let (s2, tree2) = Self::unify(&a2_subst, &b2_subst)?;
        let final_subst = Self::compose_subst(&s2, &s1);
        let output = Self::pretty_subst(&final_subst);
        let tree = InferenceTree::new("Unify-Arrow", input, &output, vec![tree1, tree2]);
        Ok((final_subst, tree))
    }

    // S₁ = unify(τ₁, τ₃)    S₂ = unify(S₁(τ₂), S₁(τ₄))
    // ───────────────────────────────────────────────── (U-Tuple)
    // unify((τ₁, τ₂), (τ₃, τ₄)) = S₂ ∘ S₁
    fn unify_tuple(ts1: &[Type], ts2: &[Type], input: &str) -> Result<(Subst, InferenceTree)> {
        if ts1.len() != ts2.len() {
            return Err(InferenceError::TupleLengthMismatch {
                left_len: ts1.len(),
                right_len: ts2.len(),
            });
        }

        let mut subst = HashMap::new();
        let mut trees = Vec::new();

        for (t1, t2) in ts1.iter().zip(ts2.iter()) {
            let t1_subst = Self::apply_subst(&subst, t1);
            let t2_subst = Self::apply_subst(&subst, t2);
            let (s, tree) = Self::unify(&t1_subst, &t2_subst)?;
            subst = Self::compose_subst(&s, &subst);
            trees.push(tree);
        }

        let output = Self::pretty_subst(&subst);
        let tree = InferenceTree::new("Unify-Tuple", input, &output, trees);
        Ok((subst, tree))
    }

    pub(crate) fn occurs_check(var: &TyVar, ty: &Type) -> bool {
        match ty {
            Type::Var(name) => name == var,
            Type::Arrow(t1, t2) => Self::occurs_check(var, t1) || Self::occurs_check(var, t2),
            Type::Tuple(types) => types.iter().any(|t| Self::occurs_check(var, t)),
            Type::Int | Type::Bool => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tyvar(name: &str) -> Type {
        Type::Var(name.to_string())
    }

    fn arrow(t1: Type, t2: Type) -> Type {
        Type::Arrow(Box::new(t1), Box::new(t2))
    }

    fn tuple(types: Vec<Type>) -> Type {
        Type::Tuple(types)
    }

    mod unify {
        use super::*;

        #[test]
        fn base_type_int() {
            // unify(Int, Int) = {}
            let (actual, _) = TypeInference::unify(&Type::Int, &Type::Int).unwrap();
            let expected: Subst = HashMap::new();
            assert_eq!(actual, expected);
        }

        #[test]
        fn base_type_bool() {
            // unify(Bool, Bool) = {}
            let (actual, _) = TypeInference::unify(&Type::Bool, &Type::Bool).unwrap();
            let expected: Subst = HashMap::new();
            assert_eq!(actual, expected);
        }

        #[test]
        fn same_tyvar() {
            // unify(t0, t0) = {}
            let (actual, _) = TypeInference::unify(&tyvar("t0"), &tyvar("t0")).unwrap();
            let expected: Subst = HashMap::new();
            assert_eq!(actual, expected);
        }

        #[test]
        fn tyvar_with_concrete_type() {
            // unify(t0, Int) = {Int/t0}
            let (actual, _) = TypeInference::unify(&tyvar("t0"), &Type::Int).unwrap();
            let expected: Subst = HashMap::from([("t0".to_string(), Type::Int)]);
            assert_eq!(actual, expected);
        }

        #[test]
        fn concrete_type_with_tyvar() {
            // unify(Int, t0) = {Int/t0}
            let (actual, _) = TypeInference::unify(&Type::Int, &tyvar("t0")).unwrap();
            let expected: Subst = HashMap::from([("t0".to_string(), Type::Int)]);
            assert_eq!(actual, expected);
        }

        #[test]
        fn arrow_simple() {
            // unify(t0 -> t1, Int -> Bool) = {Int/t0, Bool/t1}
            let t1 = arrow(tyvar("t0"), tyvar("t1"));
            let t2 = arrow(Type::Int, Type::Bool);
            let (actual, _) = TypeInference::unify(&t1, &t2).unwrap();
            let expected: Subst = HashMap::from([
                ("t0".to_string(), Type::Int),
                ("t1".to_string(), Type::Bool),
            ]);
            assert_eq!(actual, expected);
        }

        #[test]
        fn arrow_with_substitution_propagation() {
            // unify(t0 -> t0, Int -> t1) = {Int/t0, Int/t1}
            let t1 = arrow(tyvar("t0"), tyvar("t0"));
            let t2 = arrow(Type::Int, tyvar("t1"));
            let (actual, _) = TypeInference::unify(&t1, &t2).unwrap();
            let expected: Subst =
                HashMap::from([("t0".to_string(), Type::Int), ("t1".to_string(), Type::Int)]);
            assert_eq!(actual, expected);
        }

        #[test]
        fn tuple_simple() {
            // unify((t0, t1), (Int, Bool)) = {Int/t0, Bool/t1}
            let t1 = tuple(vec![tyvar("t0"), tyvar("t1")]);
            let t2 = tuple(vec![Type::Int, Type::Bool]);
            let (actual, _) = TypeInference::unify(&t1, &t2).unwrap();
            let expected: Subst = HashMap::from([
                ("t0".to_string(), Type::Int),
                ("t1".to_string(), Type::Bool),
            ]);
            assert_eq!(actual, expected);
        }

        #[test]
        fn nested_arrow() {
            // unify((t0 -> t1) -> t2, (Int -> Bool) -> Int) = {Int/t0, Bool/t1, Int/t2}
            let t1 = arrow(arrow(tyvar("t0"), tyvar("t1")), tyvar("t2"));
            let t2 = arrow(arrow(Type::Int, Type::Bool), Type::Int);
            let (actual, _) = TypeInference::unify(&t1, &t2).unwrap();
            let expected: Subst = HashMap::from([
                ("t0".to_string(), Type::Int),
                ("t1".to_string(), Type::Bool),
                ("t2".to_string(), Type::Int),
            ]);
            assert_eq!(actual, expected);
        }

        #[test]
        fn occurs_check() {
            // unify(t0, t0 -> Int) should fail with OccursCheck
            let t1 = tyvar("t0");
            let t2 = arrow(tyvar("t0"), Type::Int);
            let actual = TypeInference::unify(&t1, &t2);

            assert!(matches!(
                actual,
                Err(InferenceError::OccursCheck { var, ty: _ }) if var == "t0"
            ));
        }

        #[test]
        fn type_mismatch() {
            // unify(Int, Bool) should fail with UnificationFailure
            let actual = TypeInference::unify(&Type::Int, &Type::Bool);

            assert!(matches!(
                actual,
                Err(InferenceError::UnificationFailure {
                    expected: Type::Int,
                    actual: Type::Bool
                })
            ));
        }

        #[test]
        fn tuple_length_mismatch() {
            // unify((Int, Bool), (Int,)) should fail with TupleLengthMismatch
            let t1 = tuple(vec![Type::Int, Type::Bool]);
            let t2 = tuple(vec![Type::Int]);
            let actual = TypeInference::unify(&t1, &t2);

            assert!(matches!(
                actual,
                Err(InferenceError::TupleLengthMismatch {
                    left_len: 2,
                    right_len: 1
                })
            ));
        }
    }
}
