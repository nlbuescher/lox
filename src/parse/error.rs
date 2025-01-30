use crate::error::Error;
use crate::location::{Locate, Location};
use crate::tokenize;

impl Error {
	pub(super) fn unexpected_token(expected: &str, actual: &tokenize::Result) -> Self {
		let location = match actual {
			Ok(token) => token.locate().clone(),
			Err(error) => error.locate().clone(),
		};
		let actual = match actual {
			Ok(token) => token.to_string(),
			Err(error) => error.to_string(),
		};
		Error::Lox(Box::new((location, format!("Expected {expected} but got {actual}"))))
	}

	pub(super) fn invalid_assignment(location: Location) -> Self {
		Error::Lox(Box::new((location, String::from("Invalid assignment target"))))
	}
}
