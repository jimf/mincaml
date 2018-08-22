use id::Id;
use syntax::Syntax;
use types::Type;

use std::collections::HashMap;

fn unify(t1: &Type, t2: &Type) -> Result<(), String> {
    match (t1, t2) {
        (Type::Unit, Type::Unit) | (Type::Bool, Type::Bool) => Ok(()),
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
        ];
        for (input, expected) in cases.iter() {
            let actual = get_type_of(&env, &input);
            assert_eq!(actual, *expected);
        }
    }
}
