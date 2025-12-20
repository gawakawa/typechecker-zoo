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

#[derive(Debug, Clone, PartialEq)]
pub struct Scheme {
    pub vars: Vec<String>,
    pub ty: Type,
}
