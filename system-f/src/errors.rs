#[derive(Debug)]
pub enum TypeError {
    UnboundVariable { name: String, expr: Option<String> },
}

pub type TypeResult<T> = Result<T, TypeError>;
