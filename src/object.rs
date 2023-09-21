use crate::{
    ast::{BlockStatement, Identifier, Node},
    environment::Environment,
};

pub trait ObjectTrait {
    fn type_val(&self) -> ObjectType;
    fn type_string(&self) -> &'static str;
    fn inspect(&self) -> String;
}

type BuiltinFunction = fn(args: &Vec<Object>) -> Object;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Array {
    pub elements: Vec<Object>,
}

#[derive(PartialEq, Eq)]
pub enum ObjectType {
    Null,
    Integer,
    Boolean,
    Return,
    Error,
    Function,
    String,
    Builtin,
    Array,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    Null,
    Integer(i64),
    Boolean(bool),
    Return(std::boxed::Box<Object>),
    Error(String),
    Function(Function),
    String(std::sync::Arc<str>),
    Builtin(Builtin),
    Array(Array),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Function {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub env: Environment,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Builtin {
    pub func: BuiltinFunction,
}

impl ObjectTrait for Object {
    fn type_val(&self) -> ObjectType {
        match self {
            Self::Null => ObjectType::Null,
            Self::Integer(_) => ObjectType::Integer,
            Self::Boolean(_) => ObjectType::Boolean,
            Self::String(_) => ObjectType::String,
            Self::Return(_) => ObjectType::Return,
            Self::Error(_) => ObjectType::Error,
            Self::Function(_) => ObjectType::Function,
            Self::Builtin(_) => ObjectType::Builtin,
            Self::Array(_) => ObjectType::Array,
        }
    }
    fn type_string(&self) -> &'static str {
        match self {
            Self::Null => "NULL",
            Self::Integer(_) => "INTEGER",
            Self::Boolean(_) => "BOOLEAN",
            Self::String(_) => "STRING",
            Self::Return(_) => "RETURN",
            Self::Error(_) => "ERROR",
            Self::Function(_) => "FUNCTION",
            Self::Builtin(_) => "BUILTIN",
            Self::Array(_) => "ARRAY",
        }
    }

    fn inspect(&self) -> String {
        match self {
            Self::Null => "null".to_owned(),
            Self::Integer(val) => val.to_string(),
            Self::Boolean(val) => val.to_string(),
            Self::String(val) => val.to_string(),
            Self::Return(val) => val.inspect(),
            Self::Error(val) => "ERROR: ".to_owned() + &val,
            Self::Function(val) => {
                let mut res = String::new();
                res.push_str("fn(");
                for (i, param) in val.parameters.iter().enumerate() {
                    let s = param.string();
                    res.push_str(&s);
                    if i != val.parameters.len() - 1 {
                        res.push_str(", ");
                    }
                }
                res.push_str(") {\n");
                res.push_str(&val.body.string());
                res.push_str("\n}");
                res
            }
            Self::Builtin(_) => "builtin function".to_owned(),
            Self::Array(val) => {
                let mut res = String::new();
                res.push('[');
                for (i, el) in val.elements.iter().enumerate() {
                    res.push_str(&el.inspect());
                    if i != val.elements.len() - 1 {
                        res.push_str(", ");
                    }
                }
                res.push(']');
                res
            }
        }
    }
}
