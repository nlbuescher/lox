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
	Class(Rc<Class>),
	Instance(Box<Instance>),
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
			Value::Function(function) => write!(f, "{}", function.to_string()),
			Value::Class(class) => write!(f, "{}", class.name),
			Value::Instance(instance) => write!(f, "{} instance", instance.class.name),
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
