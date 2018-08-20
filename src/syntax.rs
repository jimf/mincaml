use types::Type;

#[derive(Debug, PartialEq)]
pub enum Syntax {
    Unit,
    Bool(bool),
    Int(i64),
    Float(f64),
    Not(Box<Syntax>),
    Neg(Box<Syntax>),
    Add(Box<Syntax>, Box<Syntax>),
    Sub(Box<Syntax>, Box<Syntax>),
    FNeg(Box<Syntax>),
    FAdd(Box<Syntax>, Box<Syntax>),
    FSub(Box<Syntax>, Box<Syntax>),
    FMul(Box<Syntax>, Box<Syntax>),
    FDiv(Box<Syntax>, Box<Syntax>),
    Equal(Box<Syntax>, Box<Syntax>),
    LE(Box<Syntax>, Box<Syntax>),
    If(Box<Syntax>, Box<Syntax>, Box<Syntax>),
    Let(String, Type, Box<Syntax>, Box<Syntax>),
    Var(String),
    LetRec(FunDef, Box<Syntax>),
    App(Box<Syntax>, Vec<Box<Syntax>>),
    // | Tuple of t list
    LetTuple(Vec<(String, Type)>, Box<Syntax>, Box<Syntax>),
    // | Array of t * t
    Get(Box<Syntax>, Box<Syntax>),
    // | Put of t * t * t
}

#[derive(Debug, PartialEq)]
pub struct FunDef {
    pub name: (String, Type),
    pub args: Vec<(String, Type)>,
    pub body: Box<Syntax>,
}
