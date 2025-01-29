use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Deref;

use crate::interpret::RuntimeError;
use crate::tokenize::Token;
use crate::value::Value;

#[derive(Clone)]
pub struct Scope {
	parent: Option<Box<Scope>>,
	values: HashMap<String, RefCell<Option<Value>>>,
}

impl Default for Scope {
	fn default() -> Self {
		Self::new()
	}
}

impl Scope {
	pub fn new() -> Self {
		Scope { parent: None, values: HashMap::new() }
	}

	pub fn with_parent(parent: &Scope) -> Self {
		Scope { parent: Some(Box::new(parent.clone())), values: HashMap::new() }
	}

	pub fn get(&self, name: &Token) -> Result<Value, RuntimeError> {
		if let Some(value) = self.values.get(&name.text) {
			if let Some(value) = value.borrow().deref() {
				return Ok(value.clone());
			}
			return Err(RuntimeError::UninitializedValue(name.clone()));
		}

		if let Some(ref parent) = self.parent {
			return parent.get(name);
		}

		Err(RuntimeError::UndefinedValue(name.clone()))
	}

	pub fn define(&mut self, name: impl Into<String>, value: Option<Value>) {
		let name = name.into();
		if let Some(old_value) = self.values.get_mut(&name) {
			old_value.replace(value);
		} else {
			self.values.insert(name, RefCell::new(value));
		}
	}

	pub fn assign(&mut self, name: &Token, value: Value) -> Result<(), RuntimeError> {
		if let Some(variable) = self.values.get_mut(&name.text) {
			variable.replace(Some(value));
			return Ok(());
		}

		if let Some(ref mut parent) = self.parent {
			return parent.assign(name, value);
		}

		Err(RuntimeError::UndefinedValue(name.clone()))
	}
}
