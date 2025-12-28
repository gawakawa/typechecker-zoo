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

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Var(String),
    Arrow(Box<Type>, Box<Type>),
    Int,
    Bool,
    Tuple(Vec<Type>),
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
