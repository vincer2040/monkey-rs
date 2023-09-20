use crate::object::Object;


pub struct Environment {
    store: std::collections::HashMap<std::sync::Arc<str>, Object>,
}

impl Environment {
    pub fn new() -> Self {
        let store = std::collections::HashMap::new();
        Self {
            store
        }
    }

    pub fn set(&mut self, name: std::sync::Arc<str>, val: Object) {
        self.store.insert(name, val);
    }

    pub fn get(&mut self, name: &std::sync::Arc<str>) -> Option<&Object> {
        self.store.get(name)
    }
}
