pub trait ObjectTrait {
    fn type_val(&self) -> ObjectType;
    fn type_string(&self) -> &'static str;
    fn inspect(&self) -> String;
}

#[derive(PartialEq, Eq)]
pub enum ObjectType {
    Null,
    Integer,
    Boolean,
    Return,
    Error,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    Null,
    Integer(i64),
    Boolean(bool),
    Return(std::boxed::Box<Object>),
    Error(String),
}

impl ObjectTrait for Object {
    fn type_val(&self) -> ObjectType {
        match self {
            Self::Null => ObjectType::Null,
            Self::Integer(_) => ObjectType::Integer,
            Self::Boolean(_) => ObjectType::Boolean,
            Self::Return(_) => ObjectType::Return,
            Self::Error(_) => ObjectType::Error,
        }
    }
    fn type_string(&self) -> &'static str {
        match self {
            Self::Null => "NULL",
            Self::Integer(_) => "INTEGER",
            Self::Boolean(_) => "BOOLEAN",
            Self::Return(_) => "RETURN",
            Self::Error(_) => "ERROR",
        }
    }

    fn inspect(&self) -> String {
        match self {
            Self::Null => "null".to_owned(),
            Self::Integer(val) => val.to_string(),
            Self::Boolean(val) => val.to_string(),
            Self::Return(val) => val.inspect(),
            Self::Error(val) => "ERROR: ".to_owned() + &val,
        }
    }
}
