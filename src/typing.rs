use id::Id;
use syntax::{Syntax, FunDef};
use types::Type;

use std::collections::HashMap;

fn unify(t1: &Type, t2: &Type) -> Result<(), String> {
    match (t1, t2) {
        (Type::Unit, Type::Unit) | (Type::Bool, Type::Bool) | (Type::Int, Type::Int) | (Type::Float, Type::Float) => Ok(()),
        _ => Err(format!("Type mismatch: expected {:?}; found {:?}", t1, t2).to_string())
    }
}

pub struct Typing<'a> {
    env: HashMap<Id, &'a Type>,
    extenv: HashMap<Id, &'a Type>,
}

impl<'a> Typing<'a> {
    pub fn new() -> Typing<'a> {
        let env = HashMap::new();
        let extenv = HashMap::new();
        Typing { env, extenv }
    }

    pub fn get_type_of(&mut self, exp: &'a Syntax) -> Result<Type, String> {
        match exp {
            Syntax::Unit => Ok(Type::Unit),
            Syntax::Bool(_) => Ok(Type::Bool),
            Syntax::Int(_) => Ok(Type::Int),
            Syntax::Float(_) => Ok(Type::Float),
            Syntax::Not(exp2) => {
                let t2 = self.get_type_of(exp2)?;
                unify(&Type::Bool, &t2)?;
                Ok(Type::Bool)
            },
            Syntax::Neg(exp2) => {
                let t2 = self.get_type_of(exp2)?;
                unify(&Type::Int, &t2)?;
                Ok(Type::Int)
            },
            Syntax::Add(e1, e2) | Syntax::Sub(e1, e2) => {
                let t1 = self.get_type_of(e1)?;
                let t2 = self.get_type_of(e2)?;
                unify(&Type::Int, &t1)?;
                unify(&Type::Int, &t2)?;
                Ok(Type::Int)
            },
            Syntax::FNeg(exp2) => {
                let t2 = self.get_type_of(exp2)?;
                unify(&Type::Float, &t2)?;
                Ok(Type::Float)
            },
            Syntax::FAdd(e1, e2) | Syntax::FSub(e1, e2) | Syntax::FMul(e1, e2) | Syntax::FDiv(e1, e2) => {
                let t1 = self.get_type_of(e1)?;
                let t2 = self.get_type_of(e2)?;
                unify(&Type::Float, &t1)?;
                unify(&Type::Float, &t2)?;
                Ok(Type::Float)
            },
            Syntax::Equal(e1, e2) | Syntax::LE(e1, e2) => {
                let t1 = self.get_type_of(e1)?;
                let t2 = self.get_type_of(e2)?;
                unify(&t1, &t2)?;
                Ok(Type::Bool)
            },
            Syntax::If(e1, e2, e3) => {
                let t1 = self.get_type_of(e1)?;
                let t2 = self.get_type_of(e2)?;
                let t3 = self.get_type_of(e3)?;
                unify(&Type::Bool, &t1)?;
                unify(&t2, &t3)?;
                Ok(Type::Bool)
            },
            Syntax::Let(id, t, e1, e2) => {
                let t1 = self.get_type_of(e1)?;
                unify(&t, &t1)?;
                self.env.insert(id.to_string(), t);
                self.get_type_of(e2)
            },
            Syntax::Var(id) => {
                if self.env.contains_key(id) {
                    let t = *self.env.get(id).unwrap();
                    Ok(t.clone())
                } else {
                    let t = *self.extenv.entry(id.to_string()).or_insert(&Type::Var(None));
                    Ok(t.clone())
                }
            },
            Syntax::LetRec(FunDef { name: (id, t), args, body: e1 }, e2) => {
                self.env.insert(id.to_string(), t);
                let mut yts = Vec::new();
                for (yid, yt) in args {
                    yts.push(Box::new(yt.clone()));
                    self.env.insert(yid.to_string(), yt);
                }
                let t1 = self.get_type_of(e1)?;
                unify(t, &Type::Fun(yts, Box::new(t1)))?;
                self.get_type_of(e2)
            },
            _ => Err("err".to_string())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use parser::parse;

    fn run_tests(cases: Vec<(&str, Result<Type, String>)>) {
        for (input, expected) in cases.iter() {
            let parsed = parse(input).unwrap();
            let mut typing = Typing::new();
            let actual = typing.get_type_of(&parsed);
            assert_eq!(actual, *expected);
        }
    }

    #[test]
    fn should_infer_type_for_primitives() {
        run_tests(vec![
            ("()", Ok(Type::Unit)),
            ("true", Ok(Type::Bool)),
            ("42", Ok(Type::Int)),
            ("3.14", Ok(Type::Float)),
        ]);
    }

    #[test]
    fn should_infer_type_for_negation() {
        run_tests(vec![
            ("not true", Ok(Type::Bool)),
            ("-1", Ok(Type::Int)),
            ("-.1.0", Ok(Type::Float)),
        ]);
    }

    #[test]
    fn should_infer_type_for_arithmetic() {
        run_tests(vec![
            ("4 + 2", Ok(Type::Int)),
            ("4 - 2", Ok(Type::Int)),
            ("4.0 +. 2.0", Ok(Type::Float)),
            ("4.0 -. 2.0", Ok(Type::Float)),
            ("4.0 *. 2.0", Ok(Type::Float)),
            ("4.0 /. 2.0", Ok(Type::Float)),
        ]);
    }

    #[test]
    fn should_infer_type_for_comparison() {
        run_tests(vec![
            ("4 = 2", Ok(Type::Bool)),
            ("4 <> 2", Ok(Type::Bool)),
        ]);
    }

    #[test]
    fn should_infer_type_for_conditionals() {
        run_tests(vec![
            ("if true then 1 else 0", Ok(Type::Bool)),
        ]);
    }

    #[test]
    #[ignore] // FIXME: come back once vars are working fully
    fn should_infer_type_for_let_expressions() {
        run_tests(vec![
            ("let var1 = 1 in var1", Ok(Type::Int)),
            ("let rec fact x = if x = 1.0 then 1.0 else x *. (fact (x -. 1.0)) in fib 5", Ok(Type::Float)),
        ]);
    }

    #[test]
    fn should_infer_type_for_vars() {
        run_tests(vec![
            ("foo", Ok(Type::Var(None))),
        ]);
    }
}
