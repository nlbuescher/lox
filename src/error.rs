use std::fmt::{Display, Formatter};

use crate::parse::Error as ParseError;
use crate::tokenize::Error as TokenError;

#[derive(Debug)]
pub enum Error {
	Usage(String),
	Io(std::io::Error),
	Tokenize(TokenError),
	Parse(ParseError),
}

impl From<std::io::Error> for Error {
	fn from(value: std::io::Error) -> Self {
		Error::Io(value)
	}
}

impl From<TokenError> for Error {
	fn from(value: TokenError) -> Self {
		Error::Tokenize(value)
	}
}

impl From<ParseError> for Error {
	fn from(value: ParseError) -> Self {
		Error::Parse(value)
	}
}

impl Display for Error {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		match self {
			Error::Usage(program) => write!(f, "Usage: {program} <filename>"),
			Error::Io(inner) => write!(f, "IO error: {inner}"),
			Error::Tokenize(inner) => write!(f, "{inner}"),
			Error::Parse(inner) => write!(f, "{inner}"),
		}
	}
}
