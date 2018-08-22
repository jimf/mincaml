use types::Type;

pub type Id = String;

fn type_to_id(t: Type) -> Id {
    match t {
        Type::Unit => "u".to_string(),
        Type::Bool => "b".to_string(),
        Type::Int => "i".to_string(),
        Type::Float => "d".to_string(),
        Type::Fun(_, _) => "f".to_string(),
        Type::Tuple(_) => "t".to_string(),
        Type::Array(_) => "a".to_string(),
        Type::Var(_) => {
            panic!("unexpected");
        },
    }
}

static mut COUNTER: i32 = 0;

pub fn gentmp(t: Type) -> Id {
    unsafe {
        COUNTER += 1;
        format!("T{}{}", type_to_id(t), COUNTER).to_string()
    }
}
