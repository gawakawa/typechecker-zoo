use std::collections::{BTreeMap, HashMap};

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Var(String),
    Abs(String, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Let(String, Box<Expr>, Box<Expr>),
    Lit(Lit),
    Tuple(Vec<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Int(i64),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Var(String),
    Arrow(Box<Type>, Box<Type>),
    Int,
    Bool,
    Tuple(Vec<Type>),
}

pub type TyVar = String;
pub type TmVar = String;
pub type Env = BTreeMap<TmVar, Scheme>;
pub type Subst = HashMap<TyVar, Type>;

#[derive(Debug, Clone, PartialEq)]
pub struct Scheme {
    pub vars: Vec<String>,
    pub ty: Type,
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
}
