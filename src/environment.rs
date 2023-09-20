use crate::object::Object;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Environment {
    store: std::collections::HashMap<std::sync::Arc<str>, Object>,
    outer: Option<std::boxed::Box<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        let store = std::collections::HashMap::new();
        Self { store, outer: None }
    }

    pub fn new_enclosed_env(outer: &Environment) -> Self {
        let mut env = Environment::new();
        env.outer = Some(std::boxed::Box::new(outer.clone()));
        env
    }

    pub fn set(&mut self, name: std::sync::Arc<str>, val: Object) {
        self.store.insert(name, val);
    }

    pub fn get(&self, name: &std::sync::Arc<str>) -> Option<&Object> {
        match self.store.get(name) {
            Some(obj) => Some(obj),
            None => match &self.outer {
                Some(env) => env.get(name),
                None => None,
            },
        }
    }
}
