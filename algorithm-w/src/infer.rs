use std::collections::{BTreeMap, HashMap, HashSet};

use crate::{
    ast::{Expr, Lit, Scheme, Type},
    error::{InferenceError, Result},
};

pub type TyVar = String;
pub type TmVar = String;
pub type Env = BTreeMap<TmVar, Scheme>;
pub type Subst = HashMap<TyVar, Type>;

#[derive(Debug)]
pub struct InferenceTree {
    pub rules: String,
    pub input: String,
    pub output: String,
    pub children: Vec<InferenceTree>,
}

impl InferenceTree {
    fn new(rule: &str, input: &str, output: &str, children: Vec<InferenceTree>) -> Self {
        Self {
            rules: rule.to_string(),
            input: input.to_string(),
            output: output.to_string(),
            children,
        }
    }
}

pub struct TypeInference {
    counter: usize,
}

impl TypeInference {
    fn fresh_tyvar(&mut self) -> TyVar {
        let var = format!("t{}", self.counter);
        self.counter += 1;
        var
    }

    fn pretty_env(env: &Env) -> String {
        if env.is_empty() {
            "{}".to_string()
        } else {
            let entries: Vec<String> = env.iter().map(|(k, v)| format!("{}: {}", k, v)).collect();
            format!("{{{}}}", entries.join(", "))
        }
    }

    fn pretty_subst(subst: &Subst) -> String {
        if subst.is_empty() {
            "{}".to_string()
        } else {
            let entries: Vec<String> = subst.iter().map(|(k, v)| format!("{}/{}", v, k)).collect();
            format!("{{{}}}", entries.join(", "))
        }
    }

    fn apply_subst(subst: &Subst, ty: &Type) -> Type {
        match ty {
            Type::Var(name) => subst.get(name).cloned().unwrap_or_else(|| ty.clone()),
            Type::Arrow(t1, t2) => Type::Arrow(
                Box::new(Self::apply_subst(subst, t1)),
                Box::new(Self::apply_subst(subst, t2)),
            ),
            Type::Tuple(types) => {
                Type::Tuple(types.iter().map(|t| Self::apply_subst(subst, t)).collect())
            }
            Type::Int | Type::Bool => ty.clone(),
        }
    }

    fn compose_subst(s1: &Subst, s2: &Subst) -> Subst {
        let mut result = s1.clone();
        for (k, v) in s2 {
            result.insert(k.clone(), Self::apply_subst(s1, v));
        }
        result
    }

    fn apply_subst_scheme(subst: &Subst, scheme: &Scheme) -> Scheme {
        let mut filtered_subst = subst.clone();
        for var in &scheme.vars {
            filtered_subst.remove(var);
        }
        Scheme {
            vars: scheme.vars.clone(),
            ty: Self::apply_subst(&filtered_subst, &scheme.ty),
        }
    }

    fn apply_subst_env(subst: &Subst, env: &Env) -> Env {
        env.iter()
            .map(|(k, v)| (k.clone(), Self::apply_subst_scheme(subst, v)))
            .collect()
    }

    fn unify(t1: &Type, t2: &Type) -> Result<(Subst, InferenceTree)> {
        let input = format!("{} - {}", t1, t2);

        match (t1, t2) {
            // ───────────────────── (U-Int, U-Bool)
            // unify(τ, τ) = ∅
            (Type::Int, Type::Int) | (Type::Bool, Type::Bool) => {
                let tree = InferenceTree::new("Unify-Base", &input, "{}", vec![]);
                Ok((HashMap::new(), tree))
            }

            // α ∉ ftv(τ)
            // ────────────────────── (U-VarL, U-VarR)
            // unify(α, τ) = [α ↦ τ]
            (Type::Var(v), ty) | (ty, Type::Var(v)) => {
                if ty == &Type::Var(v.clone()) {
                    let tree = InferenceTree::new("Unify-Var-Same", &input, "{}", vec![]);
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
                    let tree = InferenceTree::new("Unify-Var", &input, &output, vec![]);
                    Ok((subst, tree))
                }
            }

            // S₁ = unify(τ₁, τ₃)    S₂ = unify(S₁(τ₂), S₁(τ₄))
            // ───────────────────────────────────────────────── (U-Arrow)
            // unify(τ₁ → τ₂, τ₃ → τ₄) = S₂ ∘ S₁
            (Type::Arrow(a1, a2), Type::Arrow(b1, b2)) => {
                let (s1, tree1) = Self::unify(a1, b1)?;
                let a2_subst = Self::apply_subst(&s1, a2);
                let b2_subst = Self::apply_subst(&s1, b2);
                let (s2, tree2) = Self::unify(&a2_subst, &b2_subst)?;
                let final_subst = Self::compose_subst(&s2, &s1);
                let output = Self::pretty_subst(&final_subst);
                let tree = InferenceTree::new("Unify-Arrow", &input, &output, vec![tree1, tree2]);
                Ok((final_subst, tree))
            }

            // S₁ = unify(τ₁, τ₃)    S₂ = unify(S₁(τ₂), S₁(τ₄))
            // ───────────────────────────────────────────────── (U-Tuple)
            // unify((τ₁, τ₂), (τ₃, τ₄)) = S₂ ∘ S₁
            (Type::Tuple(ts1), Type::Tuple(ts2)) => {
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
                let tree = InferenceTree::new("Unify-Tuple", &input, &output, trees);
                Ok((subst, tree))
            }
            _ => Err(InferenceError::UnificationFailure {
                expected: t1.clone(),
                actual: t2.clone(),
            }),
        }
    }

    fn occurs_check(var: &TyVar, ty: &Type) -> bool {
        match ty {
            Type::Var(name) => name == var,
            Type::Arrow(t1, t2) => Self::occurs_check(var, t1) || Self::occurs_check(var, t2),
            Type::Tuple(types) => types.iter().any(|t| Self::occurs_check(var, t)),
            Type::Int | Type::Bool => false,
        }
    }

    pub fn infer(&mut self, env: &Env, expr: &Expr) -> Result<(Subst, Type, InferenceTree)> {
        match expr {
            Expr::Lit(Lit::Int(_)) => Self::infer_lit_int(env, expr),
            Expr::Lit(Lit::Bool(_)) => Self::infer_lit_bool(env, expr),
            Expr::Var(name) => self.infer_var(env, expr, name),
            Expr::Abs(param, body) => self.infer_abs(env, expr, param, body),
            Expr::App(func, arg) => self.infer_app(env, expr, func, arg),
            Expr::Let(var, value, body) => self.infer_let(env, expr, var, value, body),
            Expr::Tuple(exprs) => self.infer_tuple(env, expr, exprs),
        }
    }

    // x : σ ∈ Γ    τ = inst(σ)
    // ───────────────────────── (T-Var)
    //        Γ ⊢ x : τ
    fn infer_var(
        &mut self,
        env: &Env,
        expr: &Expr,
        name: &str,
    ) -> Result<(Subst, Type, InferenceTree)> {
        let input = format!("{} ⊢ {} ⇒", Self::pretty_env(env), expr);

        match env.get(name) {
            Some(scheme) => {
                let instantiated = self.instantiate(scheme);
                let output = format!("{}", instantiated);
                let tree = InferenceTree::new("T-Var", &input, &output, vec![]);
                Ok((HashMap::new(), instantiated, tree))
            }
            None => Err(InferenceError::UnboundVariable {
                name: name.to_string(),
            }),
        }
    }

    // Γ, x : α ⊢ e : τ    α fresh
    // ───────────────────────────── (T-Lam)
    //    Γ ⊢ λx. e : α → τ
    fn infer_abs(
        &mut self,
        env: &Env,
        expr: &Expr,
        param: &str,
        body: &Expr,
    ) -> Result<(Subst, Type, InferenceTree)> {
        let input = format!("{} ⊢ {} ⇒", Self::pretty_env(env), expr);

        let param_type = Type::Var(self.fresh_tyvar());
        let mut new_env = env.clone();
        let param_scheme = Scheme {
            vars: vec![],
            ty: param_type.clone(),
        };
        new_env.insert(param.to_string(), param_scheme);

        let (s1, body_type, tree1) = self.infer(&new_env, body)?;
        let param_type_subst = Self::apply_subst(&s1, &param_type);
        let result_type = Type::Arrow(Box::new(param_type_subst), Box::new(body_type));

        let output = format!("{}", result_type);
        let tree = InferenceTree::new("T-Abs", &input, &output, vec![tree1]);
        Ok((s1, result_type, tree))
    }

    // Γ ⊢ e₁ : τ₁    Γ ⊢ e₂ : τ₂    α fresh    S = unify(τ₁, τ₂ → α)
    // ────────────────────────────────────────────────────────────── (T-App)
    //                     Γ ⊢ e₁ e₂ : S(α)
    fn infer_app(
        &mut self,
        env: &Env,
        expr: &Expr,
        func: &Expr,
        arg: &Expr,
    ) -> Result<(Subst, Type, InferenceTree)> {
        let input = format!("{} ⊢ {} ⇒", Self::pretty_env(env), expr);
        let result_type = Type::Var(self.fresh_tyvar());

        let (s1, func_type, tree1) = self.infer(env, func)?;
        let env_subst = Self::apply_subst_env(&s1, env);
        let (s2, arg_type, tree2) = self.infer(&env_subst, arg)?;

        let func_type_subst = Self::apply_subst(&s2, &func_type);
        let expected_func_type = Type::Arrow(Box::new(arg_type), Box::new(result_type.clone()));

        let (s3, tree3) = Self::unify(&func_type_subst, &expected_func_type)?;

        let final_subst = Self::compose_subst(&s3, &Self::compose_subst(&s2, &s1));
        let final_type = Self::apply_subst(&s3, &result_type);

        let output = format!("{}", final_type);
        let tree = InferenceTree::new("T-App", &input, &output, vec![tree1, tree2, tree3]);
        Ok((final_subst, final_type, tree))
    }

    // Γ ⊢ e₁ : τ₁    σ = gen(Γ, τ₁)    Γ, x : σ ⊢ e₂ : τ₂
    // ────────────────────────────────────────────────────── (T-Let)
    //          Γ ⊢ let x = e₁ in e₂ : τ₂
    fn infer_let(
        &mut self,
        env: &Env,
        expr: &Expr,
        var: &str,
        value: &Expr,
        body: &Expr,
    ) -> Result<(Subst, Type, InferenceTree)> {
        let input = format!("{} ⊢ {} ⇒", Self::pretty_env(env), expr);

        let (s1, value_type, tree1) = self.infer(env, value)?;
        let env_subst = Self::apply_subst_env(&s1, env);
        let generalized_type = self.generalize(&env_subst, &value_type);

        let mut new_env = env_subst;
        new_env.insert(var.to_string(), generalized_type);

        let (s2, body_type, tree2) = self.infer(&new_env, body)?;

        let final_subst = Self::compose_subst(&s2, &s1);
        let output = format!("{}", body_type);
        let tree = InferenceTree::new("T-Let", &input, &output, vec![tree1, tree2]);
        Ok((final_subst, body_type, tree))
    }

    // Γ ⊢ e₁ : τ₁    ...    Γ ⊢ eₙ : τₙ
    // ─────────────────────────────────── (T-Tuple)
    //    Γ ⊢ (e₁, ..., eₙ) : (τ₁, ..., τₙ)
    fn infer_tuple(
        &mut self,
        env: &Env,
        expr: &Expr,
        exprs: &[Expr],
    ) -> Result<(Subst, Type, InferenceTree)> {
        let input = format!("{} ⊢ {} ⇒", Self::pretty_env(env), expr);

        let mut subst = HashMap::new();
        let mut types = Vec::new();
        let mut trees = Vec::new();
        let mut current_env = env.clone();

        for expr in exprs {
            let (s, ty, tree) = self.infer(&current_env, expr)?;
            subst = Self::compose_subst(&s, &subst);
            current_env = Self::apply_subst_env(&s, &current_env);
            types.push(ty);
            trees.push(tree);
        }

        let result_type = Type::Tuple(types);
        let output = format!("{}", result_type);
        let tree = InferenceTree::new("T-Tuple", &input, &output, trees);
        Ok((subst, result_type, tree))
    }

    // ───────────────── (T-LitInt)
    //    Γ ⊢ n : Int
    fn infer_lit_int(env: &Env, expr: &Expr) -> Result<(Subst, Type, InferenceTree)> {
        let input = format!("{} ⊢ {} ⇒", Self::pretty_env(env), expr);
        let tree = InferenceTree::new("T-Int", &input, "Int", vec![]);
        Ok((HashMap::new(), Type::Int, tree))
    }

    // ────────────────── (T-LitBool)
    //    Γ ⊢ b : Bool
    fn infer_lit_bool(env: &Env, expr: &Expr) -> Result<(Subst, Type, InferenceTree)> {
        let input = format!("{} ⊢ {} ⇒", Self::pretty_env(env), expr);
        let tree = InferenceTree::new("T-Bool", &input, "Bool", vec![]);
        Ok((HashMap::new(), Type::Bool, tree))
    }

    fn instantiate(&mut self, scheme: &Scheme) -> Type {
        let mut subst = HashMap::new();
        for var in &scheme.vars {
            let fresh = self.fresh_tyvar();
            subst.insert(var.clone(), Type::Var(fresh));
        }

        Self::apply_subst(&subst, &scheme.ty)
    }

    fn generalize(&self, env: &Env, ty: &Type) -> Scheme {
        let type_vars = self.free_type_vars(ty);
        let env_vars = self.free_type_vars_env(env);
        let mut free_vars: Vec<_> = type_vars.difference(&env_vars).cloned().collect();
        // For deterministic behavior
        free_vars.sort();

        Scheme {
            vars: free_vars,
            ty: ty.clone(),
        }
    }

    fn free_type_vars(&self, ty: &Type) -> HashSet<TyVar> {
        match ty {
            Type::Var(name) => {
                let mut set = HashSet::new();
                set.insert(name.clone());
                set
            }
            Type::Arrow(t1, t2) => {
                let mut set = self.free_type_vars(t1);
                set.extend(self.free_type_vars(t2));
                set
            }
            Type::Tuple(types) => {
                let mut set = HashSet::new();
                for t in types {
                    set.extend(self.free_type_vars(t));
                }
                set
            }
            Type::Int | Type::Bool => HashSet::new(),
        }
    }

    fn free_type_vars_scheme(&self, scheme: &Scheme) -> HashSet<TyVar> {
        let mut set = self.free_type_vars(&scheme.ty);
        // Remove quantified variables
        for var in &scheme.vars {
            set.remove(var);
        }
        set
    }

    fn free_type_vars_env(&self, env: &Env) -> HashSet<TyVar> {
        let mut set = HashSet::new();
        for scheme in env.values() {
            set.extend(self.free_type_vars_scheme(scheme));
        }
        set
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

    mod occurs_check {
        use super::*;

        #[test]
        fn not_in_base_type() {
            // occurs_check("t0", Int) = false
            let actual = TypeInference::occurs_check(&"t0".to_string(), &Type::Int);
            assert!(!actual);
        }

        #[test]
        fn same_var() {
            // occurs_check("t0", t0) = true
            let actual = TypeInference::occurs_check(&"t0".to_string(), &tyvar("t0"));
            assert!(actual);
        }

        #[test]
        fn different_var() {
            // occurs_check("t0", t1) = false
            let actual = TypeInference::occurs_check(&"t0".to_string(), &tyvar("t1"));
            assert!(!actual);
        }

        #[test]
        fn in_arrow_left() {
            // occurs_check("t0", t0 -> t1) = true
            let actual =
                TypeInference::occurs_check(&"t0".to_string(), &arrow(tyvar("t0"), tyvar("t1")));
            assert!(actual);
        }

        #[test]
        fn in_arrow_right() {
            // occurs_check("t0", t1 -> t0) = true
            let actual =
                TypeInference::occurs_check(&"t0".to_string(), &arrow(tyvar("t1"), tyvar("t0")));
            assert!(actual);
        }

        #[test]
        fn not_in_arrow() {
            // occurs_check("t0", t1 -> t2) = false
            let actual =
                TypeInference::occurs_check(&"t0".to_string(), &arrow(tyvar("t1"), tyvar("t2")));
            assert!(!actual);
        }

        #[test]
        fn in_tuple_first() {
            // occurs_check("t0", (t0, t1)) = true
            let actual = TypeInference::occurs_check(
                &"t0".to_string(),
                &tuple(vec![tyvar("t0"), tyvar("t1")]),
            );
            assert!(actual);
        }

        #[test]
        fn in_tuple_second() {
            // occurs_check("t0", (t1, t0)) = true
            let actual = TypeInference::occurs_check(
                &"t0".to_string(),
                &tuple(vec![tyvar("t1"), tyvar("t0")]),
            );
            assert!(actual);
        }

        #[test]
        fn not_in_tuple() {
            // occurs_check("t0", (t1, t2)) = false
            let actual = TypeInference::occurs_check(
                &"t0".to_string(),
                &tuple(vec![tyvar("t1"), tyvar("t2")]),
            );
            assert!(!actual);
        }
    }

    mod generalize {
        use super::*;

        #[test]
        fn concrete_type_empty_env() {
            // generalize({}, Int) = Int
            let ti = TypeInference { counter: 0 };
            let env: Env = BTreeMap::new();
            let actual = ti.generalize(&env, &Type::Int);
            let expected = Scheme {
                vars: vec![],
                ty: Type::Int,
            };
            assert_eq!(actual, expected);
        }

        #[test]
        fn single_tyvar_empty_env() {
            // generalize({}, t0) = ∀t0. t0
            let ti = TypeInference { counter: 0 };
            let env: Env = BTreeMap::new();
            let actual = ti.generalize(&env, &tyvar("t0"));
            let expected = Scheme {
                vars: vec!["t0".to_string()],
                ty: tyvar("t0"),
            };
            assert_eq!(actual, expected);
        }

        #[test]
        fn arrow_type_empty_env() {
            // generalize({}, t0 -> t1) = ∀t0 t1. t0 -> t1
            let ti = TypeInference { counter: 0 };
            let env: Env = BTreeMap::new();
            let actual = ti.generalize(&env, &arrow(tyvar("t0"), tyvar("t1")));
            let expected = Scheme {
                vars: vec!["t0".to_string(), "t1".to_string()],
                ty: arrow(tyvar("t0"), tyvar("t1")),
            };
            assert_eq!(actual, expected);
        }

        #[test]
        fn tyvar_bound_in_env() {
            // generalize({x: t0}, t0 -> t1) = ∀t1. t0 -> t1
            let ti = TypeInference { counter: 0 };
            let env: Env = BTreeMap::from([(
                "x".to_string(),
                Scheme {
                    vars: vec![],
                    ty: tyvar("t0"),
                },
            )]);
            let actual = ti.generalize(&env, &arrow(tyvar("t0"), tyvar("t1")));
            let expected = Scheme {
                vars: vec!["t1".to_string()],
                ty: arrow(tyvar("t0"), tyvar("t1")),
            };
            assert_eq!(actual, expected);
        }

        #[test]
        fn all_tyvars_bound_in_env() {
            // generalize({x: t0, y: t1}, t0 -> t1) = t0 -> t1
            let ti = TypeInference { counter: 0 };
            let env: Env = BTreeMap::from([
                (
                    "x".to_string(),
                    Scheme {
                        vars: vec![],
                        ty: tyvar("t0"),
                    },
                ),
                (
                    "y".to_string(),
                    Scheme {
                        vars: vec![],
                        ty: tyvar("t1"),
                    },
                ),
            ]);
            let actual = ti.generalize(&env, &arrow(tyvar("t0"), tyvar("t1")));
            let expected = Scheme {
                vars: vec![],
                ty: arrow(tyvar("t0"), tyvar("t1")),
            };
            assert_eq!(actual, expected);
        }
    }

    mod instantiate {
        use super::*;

        #[test]
        fn monomorphic_type() {
            // instantiate(Int) = Int
            let mut ti = TypeInference { counter: 0 };
            let scheme = Scheme {
                vars: vec![],
                ty: Type::Int,
            };
            let actual = ti.instantiate(&scheme);
            assert_eq!(actual, Type::Int);
        }

        #[test]
        fn multiple_tyvars_get_distinct_fresh_vars() {
            // instantiate(∀a b. (a, b, a)) = (t0, t1, t0)
            let mut ti = TypeInference { counter: 0 };
            let scheme = Scheme {
                vars: vec!["a".to_string(), "b".to_string()],
                ty: tuple(vec![tyvar("a"), tyvar("b"), tyvar("a")]),
            };
            let actual = ti.instantiate(&scheme);
            assert_eq!(actual, tuple(vec![tyvar("t0"), tyvar("t1"), tyvar("t0")]));
        }
    }
}
