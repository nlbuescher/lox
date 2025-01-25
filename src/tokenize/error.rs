use std::fmt::{Display, Formatter};

use crate::location::{Locatable, Location};
use crate::tokenize::Token;

pub type Result = std::result::Result<Token, Error>;

#[derive(Debug, Clone, PartialEq)]
pub struct Error {
	location: Location,
	pub kind: ErrorKind,
	pub text: String,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ErrorKind {
	UnknownChar,
	UnterminatedString,
}

impl Error {
	pub(crate) fn new(location: Location, kind: ErrorKind, text: impl Into<String>) -> Error {
		Error { location, kind, text: text.into() }
	}
}

impl ErrorKind {
	fn as_str(&self) -> &'static str {
		match self {
			ErrorKind::UnknownChar => "unknown character",
			ErrorKind::UnterminatedString => "unterminated string",
		}
	}
}

impl Locatable for Error {
	fn location(&self) -> &Location {
		&self.location
	}
}

impl Display for Error {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		let Error { location, kind, text } = self;
		if f.alternate() {
			write!(f, "{location} ")?;
		}
		write!(f, "{kind} '{text}'")
	}
}

impl Display for ErrorKind {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		f.write_str(self.as_str())
	}
}
