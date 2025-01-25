use std::fmt::{Display, Formatter};

use crate::interpret::TypeKind;
use crate::location::{Locatable, Location};
use crate::tokenize::Token;
use crate::value::Value;

pub enum Break {
	Return(Value),
	Error(RuntimeError),
}

impl From<RuntimeError> for Break {
	fn from(error: RuntimeError) -> Self {
		Break::Error(error)
	}
}

#[derive(Debug)]
pub enum RuntimeError {
	Io(std::io::Error),
	TypeError { location: Location, expected: TypeKind, actual: TypeKind },
	UndefinedVariable(Token),
	UninitializedVariable(Token),
	UnexpectedVoid(Location),
	NotCallable(Location),
	UnexpectedNumberOfArguments { location: Location, expected: usize, actual: usize },
}

impl From<std::io::Error> for RuntimeError {
	fn from(error: std::io::Error) -> Self {
		RuntimeError::Io(error)
	}
}

impl Display for RuntimeError {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		if f.alternate() {
			write!(f, "{location} ", location = self.location())?;
		}
		match self {
			RuntimeError::Io(inner) => Display::fmt(inner, f),

			RuntimeError::TypeError { expected, actual, .. } => {
				write!(f, "Expected {expected} but got {actual}")
			}

			RuntimeError::UndefinedVariable(name) => {
				write!(f, "Undefined variable '{name}'", name = name.text)
			}

			RuntimeError::UninitializedVariable(name) => {
				write!(f, "Uninitialized variable '{name}'", name = name.text)
			}

			RuntimeError::UnexpectedVoid(_) => write!(f, "Unexpected void"),

			RuntimeError::NotCallable(_) => write!(f, "Can only call functions and classes"),

			RuntimeError::UnexpectedNumberOfArguments { expected, actual, .. } => {
				write!(f, "Expected {expected} arguments but got {actual}")
			}
		}
	}
}

impl Locatable for RuntimeError {
	fn location(&self) -> &Location {
		match self {
			RuntimeError::Io(_) => &Location { line: 0, column: 0 },
			RuntimeError::TypeError { location, .. } => location,
			RuntimeError::UndefinedVariable(name) => name.location(),
			RuntimeError::UninitializedVariable(name) => name.location(),
			RuntimeError::UnexpectedVoid(location) => location,
			RuntimeError::NotCallable(location) => location,
			RuntimeError::UnexpectedNumberOfArguments { location, .. } => location,
		}
	}
}
