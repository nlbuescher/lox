use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;

use crate::interpret::{Callable, Class, Instance};

#[derive(Clone)]
pub enum Value {
	Nil,
	Bool(bool),
	Number(f64),
	String(String),
	Function(Rc<dyn Callable>),
	Class(Class),
	Instance(Instance),
}

impl Debug for Value {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		write!(f, "{self}")
	}
}

impl Display for Value {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		match self {
			Value::Nil => write!(f, "nil"),
			Value::Bool(b) => write!(f, "{b}"),
			Value::Number(n) => write!(f, "{n}"),
			Value::String(s) => write!(f, "{s}"),
			Value::Function(function) => f.write_str(&function.to_string()),
			Value::Class(Class { name, .. }) => f.write_str(name),
			Value::Instance(Instance { class_name, .. }) => write!(f, "{class_name} instance"),
		}
	}
}

impl PartialEq for Value {
	fn eq(&self, other: &Self) -> bool {
		match (self, other) {
			(Value::Nil, Value::Nil) => true,
			(Value::Bool(left), Value::Bool(right)) => left == right,
			(Value::Number(left), Value::Number(right)) => left == right,
			(Value::String(left), Value::String(right)) => left == right,
			_ => false,
		}
	}
}
