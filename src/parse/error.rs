use std::fmt::{Display, Formatter};

use crate::location::{Locatable, Location};
use crate::tokenize;

pub type Result<T> = std::result::Result<T, ParseError>;

#[derive(Debug)]
pub enum ParseError {
	UnexpectedToken { expected: &'static str, actual: tokenize::Result },
	InvalidAssignment { location: Location },
}

impl Display for ParseError {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		if f.alternate() {
			write!(f, "{location} ", location = self.location())?;
		}
		match self {
			ParseError::UnexpectedToken { expected, actual } => match actual {
				Ok(token) => write!(f, "Expected {expected} but got {token}"),
				Err(error) => write!(f, "Expected {expected} but got {error}"),
			},
			ParseError::InvalidAssignment { .. } => {
				write!(f, "Invalid assignment target")
			}
		}
	}
}

impl Locatable for ParseError {
	fn location(&self) -> &Location {
		match self {
			ParseError::UnexpectedToken { actual, .. } => match actual {
				Ok(token) => token.location(),
				Err(error) => error.location(),
			},
			ParseError::InvalidAssignment { location, .. } => location,
		}
	}
}
