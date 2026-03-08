use crate::ast::Type;

#[derive(Debug)]
pub enum TypeError {
    UnboundVariable { name: String, expr: Option<String> },

    ApplicationTypeError { actual: Type, expr: Option<String> },

    TypeApplicationError { actual: Type, expr: Option<String> },
}

pub type TypeResult<T> = Result<T, TypeError>;
