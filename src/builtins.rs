use crate::{
    evaluator,
    object::{Array, Object, ObjectTrait},
};

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
        Object::Array(v) => Object::Integer(v.elements.len() as i64),
        _ => Object::Error(format!(
            "argument to `len` not supported, got {}",
            arg.type_string()
        )),
    }
}

pub fn first(args: &Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }
    let arg = &args[0];
    match arg {
        Object::Array(v) => {
            if v.elements.len() > 0 {
                return v.elements[0].clone();
            } else {
                return evaluator::NULL;
            }
        }
        _ => Object::Error(format!(
            "argument to `len` not supported, got {}",
            arg.type_string()
        )),
    }
}

pub fn last(args: &Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }
    let arg = &args[0];
    match arg {
        Object::Array(v) => {
            if v.elements.len() > 0 {
                return v.elements[v.elements.len() - 1].clone();
            } else {
                return evaluator::NULL;
            }
        }
        _ => Object::Error(format!(
            "argument to `len` not supported, got {}",
            arg.type_string()
        )),
    }
}

pub fn rest(args: &Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }
    let arg = &args[0];
    match arg {
        Object::Array(v) => {
            let len = v.elements.len();
            if len > 0 {
                let mut r = Vec::new();
                for i in 1..len {
                    r.push(v.elements[i].clone());
                }
                Object::Array(Array { elements: r })
            } else {
                return evaluator::NULL;
            }
        }
        _ => Object::Error(format!(
            "argument to `len` not supported, got {}",
            arg.type_string()
        )),
    }
}

pub fn push(args: &Vec<Object>) -> Object {
    if args.len() != 2 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }
    let arg = &args[0];
    let val = &args[1];
    match arg {
        Object::Array(v) => {
            let mut r = Vec::new();
            for o in v.elements.iter() {
                r.push(o.clone());
            }
            r.push(val.clone());
            Object::Array(Array { elements: r })
        }
        _ => Object::Error(format!(
            "argument to `len` not supported, got {}",
            arg.type_string()
        )),
    }
}

pub fn print(args: &Vec<Object>) -> Object {
    for arg in args.iter() {
        println!("{}", arg.inspect());
    }
    return evaluator::NULL;
}
