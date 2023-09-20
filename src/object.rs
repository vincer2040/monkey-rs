pub trait ObjectTrait {
    fn type_string(&self) -> &'static str;
    fn inspect(&self) -> String;
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    Null,
    Integer(i64),
    Boolean(bool),
    Return(std::boxed::Box<Object>),
}

impl ObjectTrait for Object {
    fn type_string(&self) -> &'static str {
        match self {
            Self::Null => "NULL",
            Self::Integer(_) => "INTEGER",
            Self::Boolean(_) => "BOOLEAN",
            Self::Return(_) => "RETURN",
        }
    }

    fn inspect(&self) -> String {
        match self {
            Self::Null => "null".to_owned(),
            Self::Integer(val) => val.to_string(),
            Self::Boolean(val) => val.to_string(),
            Self::Return(val) => val.inspect(),
        }
    }
}
