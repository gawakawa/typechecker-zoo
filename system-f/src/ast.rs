#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Var(String),                                 // Variable: x
    App(Box<Expr>, Box<Expr>),                   // Application: e₁ e₂
    Abs(String, Box<Type>, Box<Expr>),           // Lambda abstraction: λx: T. e
    TApp(Box<Expr>, Box<Type>),                  // Type application: e [T]
    TAbs(String, Box<Expr>),                     // Type abstraction: Λα. e
    Ann(Box<Expr>, Box<Type>),                   // Type annotation: e : T
    LitInt(i64),                                 // Integer literal
    LitBool(bool),                               // Boolean literal
    Let(String, Box<Expr>, Box<Expr>),           // Let binding: let x = e₁ in e₂
    IfThenElse(Box<Expr>, Box<Expr>, Box<Expr>), // Conditional: if e₁ then e₂ else e₃
    BinOp(BinOp, Box<Expr>, Box<Expr>),          // Binary operation: e₁ op e₂
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    And, // &&
    Or,  // ||
    Eq,  // ==
    Ne,  // !=
    Lt,  // <
    Le,  // <=
    Gt,  // >
    Ge,  // >=
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Var(String),                 // α (ordinary type variable)
    ETVar(String),               // ^α (existential type variable)
    Arrow(Box<Type>, Box<Type>), // A → B
    Forall(String, Box<Type>),   // ∀α. A
    Int,                         // Int
    Bool,                        // Bool
}
