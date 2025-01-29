use std::fmt::{Display, Formatter};

use crate::interpret::{Callable, Environment, RuntimeError};
use crate::value::Value;

#[derive(Debug, Clone)]
pub struct Class {
	pub name: String,
}

#[derive(Debug, Clone)]
pub struct Instance {
	pub class_name: String,
}

impl Class {
	pub fn new(name: String) -> Self {
		Class { name: name.clone() }
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
		Ok(Value::Instance(Instance { class_name: self.name.clone() }))
	}
}
