use crate::parse;
use crate::tokenize;
use std::fmt::Display;

#[derive(Debug)]
pub enum Error {
	Usage(String),
	Io(std::io::Error),
	Tokenize(tokenize::AnnotatedError),
	Parse(parse::Error),
}

impl From<std::io::Error> for Error {
	fn from(value: std::io::Error) -> Self {
		Error::Io(value)
	}
}

impl From<tokenize::AnnotatedError> for Error {
	fn from(value: tokenize::AnnotatedError) -> Self {
		Error::Tokenize(value)
	}
}

impl Display for Error {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Error::Usage(program) => write!(f, "Usage: {program} <filename>"),
			Error::Io(inner) => write!(f, "IO error: {inner}"),
			Error::Tokenize(inner) => write!(f, "{inner}"),
			Error::Parse(inner) => write!(f, "{inner}"),
		}
	}
}
