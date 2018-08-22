use id::Id;
use syntax::Syntax;
use types::Type;

use std::collections::HashMap;

fn unify(t1: &Type, t2: &Type) -> Result<(), String> {
    match (t1, t2) {
        (Type::Unit, Type::Unit) | (Type::Bool, Type::Bool) | (Type::Int, Type::Int) | (Type::Float, Type::Float) => Ok(()),
        _ => Err("fixme".to_string())
    }
}

pub fn get_type_of(env: &HashMap<Id, Type>, exp: &Syntax) -> Result<Type, String> {
    match exp {
        Syntax::Unit => Ok(Type::Unit),
        Syntax::Bool(_) => Ok(Type::Bool),
        Syntax::Int(_) => Ok(Type::Int),
        Syntax::Float(_) => Ok(Type::Float),
        Syntax::Not(exp2) => {
            let t2 = &get_type_of(&env, exp2)?;
            unify(&Type::Bool, t2)?;
            Ok(Type::Bool)
        },
        Syntax::Neg(exp2) => {
            let t2 = &get_type_of(&env, exp2)?;
            unify(&Type::Int, t2)?;
            Ok(Type::Int)
        },
        Syntax::Add(e1, e2) | Syntax::Sub(e1, e2) => {
            let t1 = &get_type_of(&env, e1)?;
            let t2 = &get_type_of(&env, e2)?;
            unify(&Type::Int, t1)?;
            unify(&Type::Int, t2)?;
            Ok(Type::Int)
        },
        Syntax::FNeg(exp2) => {
            let t2 = &get_type_of(&env, exp2)?;
            unify(&Type::Float, t2)?;
            Ok(Type::Float)
        },
        Syntax::FAdd(e1, e2) | Syntax::FSub(e1, e2) | Syntax::FMul(e1, e2) | Syntax::FDiv(e1, e2) => {
            let t1 = &get_type_of(&env, e1)?;
            let t2 = &get_type_of(&env, e2)?;
            unify(&Type::Float, t1)?;
            unify(&Type::Float, t2)?;
            Ok(Type::Float)
        },
        Syntax::Equal(e1, e2) | Syntax::LE(e1, e2) => {
            let t1 = &get_type_of(&env, e1)?;
            let t2 = &get_type_of(&env, e2)?;
            unify(t1, t2)?;
            Ok(Type::Bool)
        },
        Syntax::If(e1, e2, e3) => {
            let t1 = &get_type_of(&env, e1)?;
            let t2 = &get_type_of(&env, e2)?;
            let t3 = &get_type_of(&env, e3)?;
            unify(&Type::Bool, t1)?;
            unify(t2, t3)?;
            Ok(Type::Bool)
        },
        // TODO
        // Syntax::Let(id, t, e1, e2) => {
        //     let t1 = &get_type_of(&env, e1)?;
        //     unify(&t, t1)?;
        //     env.insert(id, t);
        //     get_type_of(&env, e2)
        // },
        _ => Err("err".to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_infer_type_for_primitives() {
        let env: HashMap<Id, Type> = HashMap::new();
        let cases = [
            (Syntax::Unit, Ok(Type::Unit)),
            (Syntax::Bool(true), Ok(Type::Bool)),
            (Syntax::Int(42), Ok(Type::Int)),
            (Syntax::Float(3.14), Ok(Type::Float)),
        ];
        for (input, expected) in cases.iter() {
            let actual = get_type_of(&env, &input);
            assert_eq!(actual, *expected);
        }
    }

    #[test]
    fn should_infer_type_for_negation() {
        let env: HashMap<Id, Type> = HashMap::new();
        let cases = [
            (
                Syntax::Not(Box::new(Syntax::Bool(true))),
                Ok(Type::Bool)
            ),

            (
                Syntax::Neg(Box::new(Syntax::Int(1))),
                Ok(Type::Int)
            ),

            (
                Syntax::FNeg(Box::new(Syntax::Float(1.0))),
                Ok(Type::Float)
            ),
        ];
        for (input, expected) in cases.iter() {
            let actual = get_type_of(&env, &input);
            assert_eq!(actual, *expected);
        }
    }

    #[test]
    fn should_infer_type_for_arithmetic() {
        let env: HashMap<Id, Type> = HashMap::new();
        let cases = [
            (
                Syntax::Add(Box::new(Syntax::Int(4)), Box::new(Syntax::Int(2))),
                Ok(Type::Int)
            ),

            (
                Syntax::Sub(Box::new(Syntax::Int(4)), Box::new(Syntax::Int(2))),
                Ok(Type::Int)
            ),

            (
                Syntax::FAdd(Box::new(Syntax::Float(4.0)), Box::new(Syntax::Float(2.0))),
                Ok(Type::Float)
            ),

            (
                Syntax::FSub(Box::new(Syntax::Float(4.0)), Box::new(Syntax::Float(2.0))),
                Ok(Type::Float)
            ),

            (
                Syntax::FMul(Box::new(Syntax::Float(4.0)), Box::new(Syntax::Float(2.0))),
                Ok(Type::Float)
            ),

            (
                Syntax::FDiv(Box::new(Syntax::Float(4.0)), Box::new(Syntax::Float(2.0))),
                Ok(Type::Float)
            ),
        ];
        for (input, expected) in cases.iter() {
            let actual = get_type_of(&env, &input);
            assert_eq!(actual, *expected);
        }
    }

    #[test]
    fn should_infer_type_for_comparison() {
        let env: HashMap<Id, Type> = HashMap::new();
        let cases = [
            (
                Syntax::Equal(Box::new(Syntax::Int(4)), Box::new(Syntax::Int(2))),
                Ok(Type::Bool)
            ),

            (
                Syntax::LE(Box::new(Syntax::Int(4)), Box::new(Syntax::Int(2))),
                Ok(Type::Bool)
            ),
        ];
        for (input, expected) in cases.iter() {
            let actual = get_type_of(&env, &input);
            assert_eq!(actual, *expected);
        }
    }

    #[test]
    fn should_infer_type_for_conditionals() {
        let env: HashMap<Id, Type> = HashMap::new();
        let cases = [
            (
                Syntax::If(
                    Box::new(Syntax::Bool(true)),
                    Box::new(Syntax::Int(1)),
                    Box::new(Syntax::Int(0))
                ),
                Ok(Type::Bool)
            ),
        ];
        for (input, expected) in cases.iter() {
            let actual = get_type_of(&env, &input);
            assert_eq!(actual, *expected);
        }
    }

    // TODO
    // #[test]
    // fn should_infer_type_for_let_expressions() {
    //     let env: HashMap<Id, Type> = HashMap::new();
    //     let cases = [
    //         (
    //             Syntax::Let(
    //                 "var1".to_string(),
    //                 Type::Int,
    //                 Box::new(Syntax::Int(1)),
    //                 Box::new(Syntax::Int(0))
    //             ),
    //             Ok(Type::Int)
    //         ),
    //     ];
    //     for (input, expected) in cases.iter() {
    //         let actual = get_type_of(&env, &input);
    //         assert_eq!(actual, *expected);
    //     }
    // }
}
