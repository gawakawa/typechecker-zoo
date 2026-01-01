use std::collections::{HashMap, HashSet};

use crate::ast::{Scheme, Type};

use super::{Env, TyVar, TypeInference};

impl TypeInference {
    pub(super) fn instantiate(&mut self, scheme: &Scheme) -> Type {
        let mut subst = HashMap::new();
        for var in &scheme.vars {
            let fresh = self.fresh_tyvar();
            subst.insert(var.clone(), Type::Var(fresh));
        }

        Self::apply_subst(&subst, &scheme.ty)
    }

    pub(super) fn generalize(&self, env: &Env, ty: &Type) -> Scheme {
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
    use std::collections::BTreeMap;

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
