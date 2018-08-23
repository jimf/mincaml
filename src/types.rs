#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Unit,
    Bool,
    Int,
    Float,
    Fun(Vec<Box<Type>>, Box<Type>),
    Tuple(Vec<Box<Type>>),
    Array(Box<Type>),
    Var(Option<Box<Type>>),
}
