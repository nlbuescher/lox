use std::fmt::{Display, Formatter};

use crate::interpret::TypeKind;
use crate::location::{Locatable, Location};
use crate::tokenize::Token;
use crate::value::Value;

#[derive(Debug)]
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
	UndefinedValue(Token),
	UninitializedValue(Token),
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

			RuntimeError::UndefinedValue(name) => {
				write!(f, "'{name}' is undefined", name = name.text)
			}

			RuntimeError::UninitializedValue(name) => {
				write!(f, "'{name}' is uninitialized", name = name.text)
			}

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
			RuntimeError::UndefinedValue(name) => name.location(),
			RuntimeError::UninitializedValue(name) => name.location(),
			RuntimeError::NotCallable(location) => location,
			RuntimeError::UnexpectedNumberOfArguments { location, .. } => location,
		}
	}
}
