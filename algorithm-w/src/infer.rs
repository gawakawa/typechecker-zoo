use std::collections::{BTreeMap, HashMap};

use crate::{
    ast::{Scheme, Type},
    error::InferenceError,
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

    fn unify(t1: &Type, t2: &Type) -> Result<(Subst, InferenceTree), InferenceError> {
        let input = format!("{} - {}", t1, t2);

        match (t1, t2) {
            (Type::Int, Type::Int) | (Type::Bool, Type::Bool) => {
                let tree = InferenceTree::new("Unify-Base", &input, "{}", vec![]);
                Ok((HashMap::new(), tree))
            }
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
}
