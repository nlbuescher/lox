use std::collections::HashMap;
use std::fmt::{Display, Formatter};

use crate::interpret::{Callable, Environment, RuntimeError};
use crate::tokenize::Token;
use crate::value::Value;

#[derive(Debug, Clone)]
pub struct Class {
	pub name: String,
}

#[derive(Debug, Clone)]
pub struct Instance {
	pub class_name: String,
	fields: HashMap<String, Value>,
}

impl Class {
	pub fn new(name: String) -> Self {
		Class { name: name.clone() }
	}
}

impl Instance {
	fn new(class: &Class) -> Self {
		Instance { class_name: class.name.clone(), fields: HashMap::new() }
	}

	pub fn get(&self, name: &Token) -> Result<Value, RuntimeError> {
		self.fields
			.get(&name.text)
			.cloned()
			.ok_or_else(|| RuntimeError::UndefinedValue(name.clone()))
	}
}

impl Display for Class {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		write!(f, "{name}", name = self.name)
	}
}

impl PartialEq for Class {
	fn eq(&self, _other: &Self) -> bool {
		false
	}
}

impl Callable for Class {
	fn arity(&self) -> usize {
		0
	}

	fn call(&self, _: &mut Environment, _: &[Value]) -> Result<Value, RuntimeError> {
		Ok(Value::Instance(Instance::new(self)))
	}
}
