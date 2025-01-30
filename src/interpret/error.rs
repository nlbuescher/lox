use crate::error::Error;
use crate::interpret::TypeKind;
use crate::location::{Locate, Location};
use crate::tokenize::Token;
use crate::value::Value;

#[derive(Debug)]
pub enum Break {
	Return(Value),
	Error(Error),
}

impl From<Error> for Break {
	fn from(error: Error) -> Self {
		Break::Error(error)
	}
}

impl Error {
	pub(super) fn type_error(location: &Location, expected: TypeKind, actual: TypeKind) -> Self {
		Error::Lox(Box::new((location.clone(), format!("Expected {expected} but got {actual}"))))
	}

	pub(super) fn undefined_value(token: &Token) -> Self {
		Error::Lox(Box::new((token.locate().clone(), format!("{} is undefined", token.text))))
	}

	pub(super) fn uninitialized_value(token: &Token) -> Self {
		Error::Lox(Box::new((token.locate().clone(), format!("{} is uninitialized", token.text))))
	}

	pub(super) fn not_callable(location: &Location) -> Self {
		Error::Lox(Box::new((location.clone(), String::from("Value is not callable"))))
	}

	pub(super) fn unexpected_number_of_arguments(location: &Location, expected: usize, actual: usize) -> Self {
		Error::Lox(Box::new((
			location.clone(),
			format!("Expected {expected} arguments but got {actual}"),
		)))
	}
}
