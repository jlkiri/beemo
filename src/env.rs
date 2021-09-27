use std::{borrow::BorrowMut, cell::RefCell, collections::HashMap, rc::Rc};

use crate::{error::BeemoError, Value};

/* pub struct Environment {
    enclosing: Option<Rc<Environment>>,
    // parent: Option<&'a Environment<'a>>,
    inner: HashMap<String, Value>,
} */

#[derive(Debug, Clone)]
pub struct Environment(Option<Rc<Node>>);

#[derive(Debug, Clone)]
struct Node {
    inner: RefCell<HashMap<String, Value>>,
    parent: Environment,
    len: usize,
}

impl Environment {
    pub fn new() -> Self {
        let node = Node {
            inner: RefCell::new(HashMap::new()),
            parent: Environment(None),
            len: 1,
        };
        Self(Some(Rc::new(node)))
    }

    pub fn child(&self) -> Environment {
        let node = Node {
            inner: RefCell::new(HashMap::new()),
            parent: self.clone(),
            len: self.len() + 1,
        };
        Environment(Some(Rc::new(node)))
    }

    pub fn len(&self) -> usize {
        self.0.as_ref().map_or(0, |n| n.len)
    }

    pub fn define(&self, ident: String, value: Value) {
        let inner = &self
            .0
            .as_ref()
            .ok_or(BeemoError::InternalError)
            .expect("Unwrapped empty environment")
            .inner;
        inner.borrow_mut().insert(ident, value);
    }

    fn node(&self) -> Option<&Rc<Node>> {
        self.0.as_ref()
    }

    fn parent(&self) -> Environment {
        self.0
            .as_ref()
            .map_or(Environment(None), |n| n.parent.clone())
    }

    pub fn get(&self, ident: &str) -> Option<Value> {
        let mut env = self.clone();
        while {
            let node = env
                .node()
                .ok_or(BeemoError::InternalError)
                .expect("Unwrapped empty environment");
            node.inner.borrow().get(ident).is_none()
        } {
            env = env.parent();
        }

        env.node()
            .and_then(|n| n.inner.borrow().get(ident).cloned())
    }
}
