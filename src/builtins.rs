use crate::object::{Object, ObjectTrait};

pub fn len(args: &Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }
    let arg = &args[0];
    match arg {
        Object::String(v) => Object::Integer(v.len() as i64),
        _ => Object::Error(format!(
            "argument to `len` not supported, got {}",
            arg.type_string()
        )),
    }
}
