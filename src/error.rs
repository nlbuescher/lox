use std::fmt::{Display, Formatter};

use crate::interpret::RuntimeError;
use crate::parse::ParseError;

#[derive(Debug)]
pub enum Error {
	BadUsage(Vec<String>),
	Io(std::io::Error),
	Parse(ParseError),
	Runtime(RuntimeError),
}

impl Display for Error {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		match self {
			Error::BadUsage(args) => write!(f, "Usage: {} <filename>", &args[0]),
			Error::Io(inner) => inner.fmt(f),
			Error::Parse(inner) => inner.fmt(f),
			Error::Runtime(inner) => inner.fmt(f),
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
