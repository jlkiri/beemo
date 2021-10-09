use std::{borrow::BorrowMut, cell::RefCell, collections::HashMap, rc::Rc};

use crate::{error::BeemoError, Value};

/* pub struct Environment {
    enclosing: Option<Rc<Environment>>,
    // parent: Option<&'a Environment<'a>>,
    inner: HashMap<String, Value>,
} */

#[derive(Debug, Clone)]
pub struct Environment<'a>(Option<Rc<Node<'a>>>);

#[derive(Debug, Clone)]
struct Node<'a> {
    inner: RefCell<HashMap<String, Value>>,
    parent: &'a Environment<'a>,
    len: usize,
}

impl<'a> Environment<'a> {
    pub fn new() -> Self {
        let node = Node {
            inner: RefCell::new(HashMap::new()),
            parent: &Environment(None),
            len: 0,
        };
        Self(Some(Rc::new(node)))
    }

    pub fn child(&self) -> Environment {
        let node = Node {
            inner: RefCell::new(HashMap::new()),
            parent: self,
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
            .expect("Unwrapped empty environment (define)")
            .inner;
        inner.borrow_mut().insert(ident, value);
    }

    fn node(&self) -> Option<&Rc<Node>> {
        self.0.as_ref()
    }

    fn parent(&self) -> &Environment {
        self.0.as_ref().map_or(&Environment(None), |n| n.parent)
    }

    pub fn vec_push(&self, ident: &str, val: Value) -> Option<()> {
        let mut env = self;
        while {
            let node = env.node();
            node.is_some()
                && node
                    .and_then(|node| node.inner.borrow_mut().get_mut(ident).cloned())
                    .is_none()
        } {
            env = env.parent();
        }

        let mut mut_env = env.node().and_then(|n| Some(n.inner.borrow_mut())).unwrap();
        mut_env.get_mut(ident).and_then(|vec| {
            if let Value::Array(vec) = vec {
                match val {
                    Value::Number(num) => {
                        vec.push(num);
                        return Some(());
                    }
                    _ => unreachable!(),
                }
            }
            None
        })
    }

    pub fn get(&self, ident: &str) -> Option<Value> {
        let mut env = self;
        while {
            let node = env.node();
            node.is_some()
                && node
                    .and_then(|node| node.inner.borrow().get(ident).cloned())
                    .is_none()
        } {
            env = env.parent();
        }

        env.node()
            .and_then(|n| n.inner.borrow().get(ident).cloned())
    }
}
