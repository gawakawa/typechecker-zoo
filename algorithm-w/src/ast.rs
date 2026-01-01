use itertools::Itertools;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Var(String),
    Abs(String, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Let(String, Box<Expr>, Box<Expr>),
    Lit(Lit),
    Tuple(Vec<Expr>),
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Var(name) => write!(f, "{}", name),
            Expr::Lit(Lit::Int(n)) => write!(f, "{}", n),
            Expr::Lit(Lit::Bool(b)) => write!(f, "{}", b),
            Expr::Abs(param, body) => write!(f, "λ{}.{}", param, body),
            Expr::App(func, arg) => match (func.as_ref(), arg.as_ref()) {
                (Expr::Abs(_, _), _) => write!(f, "({}) {}", func, arg),
                (_, Expr::App(_, _)) => write!(f, "{} ({})", func, arg),
                (_, Expr::Abs(_, _)) => write!(f, "{} ({})", func, arg),
                _ => write!(f, "{} {}", func, arg),
            },
            Expr::Let(var, value, body) => write!(f, "let {} = {} in {}", var, value, body),
            Expr::Tuple(exprs) => write!(f, "({})", exprs.iter().format(", ")),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Int(i64),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Var(String),
    Arrow(Box<Type>, Box<Type>),
    Int,
    Bool,
    Tuple(Vec<Type>),
}

impl Type {
    pub fn alpha_eq(&self, target: &Type) -> bool {
        use std::collections::HashMap;
        self.bijects_to(target, &mut HashMap::new(), &mut HashMap::new())
    }

    fn bijects_to(
        &self,
        target: &Type,
        map: &mut std::collections::HashMap<String, String>,
        inv: &mut std::collections::HashMap<String, String>,
    ) -> bool {
        match (self, target) {
            (Type::Var(a), Type::Var(b)) => match (map.get(a), inv.get(b)) {
                (None, None) => {
                    map.insert(a.clone(), b.clone());
                    inv.insert(b.clone(), a.clone());
                    true
                }
                (Some(map_a), Some(inv_b)) => map_a == b && inv_b == a,
                _ => false,
            },
            (Type::Arrow(a1, a2), Type::Arrow(b1, b2)) => {
                a1.bijects_to(b1, map, inv) && a2.bijects_to(b2, map, inv)
            }
            (Type::Tuple(ts1), Type::Tuple(ts2)) => {
                ts1.len() == ts2.len()
                    && ts1.iter().zip(ts2).all(|(a, b)| a.bijects_to(b, map, inv))
            }
            (Type::Int, Type::Int) | (Type::Bool, Type::Bool) => true,
            _ => false,
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Var(name) => write!(f, "{}", name),
            Type::Int => write!(f, "Int"),
            Type::Bool => write!(f, "Bool"),
            Type::Arrow(t1, t2) => match t1.as_ref() {
                Type::Arrow(_, _) => write!(f, "({}) -> {}", t1, t2),
                _ => write!(f, "{} -> {}", t1, t2),
            },
            Type::Tuple(types) => write!(f, "({})", types.iter().format(", ")),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Scheme {
    pub vars: Vec<String>,
    pub ty: Type,
}

impl std::fmt::Display for Scheme {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.vars.is_empty() {
            write!(f, "{}", self.ty)
        } else {
            write!(f, "forall {}. {}", self.vars.join(" "), self.ty)
        }
    }
}
