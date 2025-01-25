use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Deref;
use std::rc::Rc;

use crate::interpret::RuntimeError;
use crate::tokenize::Token;
use crate::value::Value;

#[derive(Debug, Clone)]
pub struct Scope {
	parent: Option<Box<Scope>>,
	values: HashMap<String, Rc<RefCell<Option<Value>>>>,
}

impl Scope {
	pub(super) fn new() -> Self {
		Scope { parent: None, values: HashMap::new() }
	}

	pub(super) fn with_parent(parent: &Scope) -> Self {
		Scope { parent: Some(Box::new(parent.clone())), values: HashMap::new() }
	}

	pub(super) fn get(&self, name: &Token) -> Result<Value, RuntimeError> {
		if let Some(value) = self.values.get(&name.text) {
			if let Some(value) = value.borrow().deref() {
				return Ok(value.clone());
			}
			return Err(RuntimeError::UninitializedVariable(name.clone()));
		}

		if let Some(ref parent) = self.parent {
			return Scope::get(parent, name);
		}

		Err(RuntimeError::UndefinedVariable(name.clone()))
	}

	pub(super) fn define(&mut self, name: impl Into<String>, value: Option<Value>) {
		let name = name.into();
		if self.values.contains_key(&name) {
			*self.values.get_mut(&name).unwrap().borrow_mut() = value;
		} else {
			self.values.insert(name, Rc::new(RefCell::new(value)));
		}
	}

	pub(super) fn assign(&mut self, name: &Token, value: Value) -> Result<(), RuntimeError> {
		if let Some(variable) = self.values.get_mut(&name.text) {
			*variable.borrow_mut() = Some(value);
			return Ok(());
		}

		if let Some(ref mut parent) = self.parent {
			return Scope::assign(parent, name, value);
		}

		Err(RuntimeError::UndefinedVariable(name.clone()))
	}
}
