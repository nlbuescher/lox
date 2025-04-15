use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Deref;

use crate::error::Error;
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

	pub fn with_parent(parent: Scope) -> Self {
		Scope { parent: Some(Box::new(parent)), values: HashMap::new() }
	}

	pub fn parent(&self) -> Option<&Scope> {
		self.parent.as_deref()
	}

	pub fn get_local(&self, name: &Token) -> Result<Value, Error> {
		if let Some(value) = self.values.get(&name.text) {
			if let Some(value) = value.borrow().deref() {
				return Ok(value.clone());
			}
			return Err(Error::uninitialized_value(name));
		}

		Err(Error::undefined_value(name))
	}

	pub fn get(&self, name: &Token) -> Result<Value, Error> {
		if let Some(value) = self.values.get(&name.text) {
			if let Some(value) = value.borrow().deref() {
				return Ok(value.clone());
			}
			return Err(Error::uninitialized_value(name));
		}

		if let Some(parent) = &self.parent {
			return parent.get(name);
		}

		Err(Error::undefined_value(name))
	}

	pub fn define(&mut self, name: impl Into<String>, value: Option<Value>) {
		let name = name.into();
		if let Some(old_value) = self.values.get_mut(&name) {
			old_value.replace(value);
		}
		else {
			self.values.insert(name, RefCell::new(value));
		}
	}

	pub fn assign(&mut self, name: &Token, value: Value) -> Result<(), Error> {
		if let Some(variable) = self.values.get_mut(&name.text) {
			variable.replace(Some(value));
			return Ok(());
		}

		if let Some(parent) = &mut self.parent {
			return parent.assign(name, value);
		}

		Err(Error::undefined_value(name))
	}
}
