use id::Id;
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
    Let(Id, Type, Box<Syntax>, Box<Syntax>),
    Var(Id),
    LetRec(FunDef, Box<Syntax>),
    App(Box<Syntax>, Vec<Box<Syntax>>),
    Tuple(Vec<Box<Syntax>>),
    LetTuple(Vec<(Id, Type)>, Box<Syntax>, Box<Syntax>),
    Array(Box<Syntax>, Box<Syntax>),
    Get(Box<Syntax>, Box<Syntax>),
    Put(Box<Syntax>, Box<Syntax>, Box<Syntax>),
}

#[derive(Debug, PartialEq)]
pub struct FunDef {
    pub name: (Id, Type),
    pub args: Vec<(Id, Type)>,
    pub body: Box<Syntax>,
}
