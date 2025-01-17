use std::fmt::{Display, Formatter};

use crate::interpret::RuntimeError;
use crate::location::{Locatable, Location};
use crate::parse::ParseError;

#[derive(Debug)]
pub enum Error {
	Io(std::io::Error),
	Parse(ParseError),
	Runtime(RuntimeError),
}

impl Display for Error {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		match self {
			Error::Io(inner) => inner.fmt(f),
			Error::Parse(inner) => inner.fmt(f),
			Error::Runtime(inner) => inner.fmt(f),
		}
	}
}

impl Locatable for Error {
	fn location(&self) -> &Location {
		match self {
			Error::Io(_) => &Location { line: 0, column: 0 },
			Error::Parse(error) => error.location(),
			Error::Runtime(error) => error.location(),
		}
	}
}

impl From<std::io::Error> for Error {
	fn from(value: std::io::Error) -> Self {
		Error::Io(value)
	}
}

impl From<ParseError> for Error {
	fn from(value: ParseError) -> Self {
		Error::Parse(value)
	}
}

impl From<RuntimeError> for Error {
	fn from(value: RuntimeError) -> Self {
		Error::Runtime(value)
	}
}
