use std::fmt::{Debug, Display, Formatter};

use crate::location::Location;

pub type Result<T> = std::result::Result<T, Error>;

pub enum Error {
	Io(std::io::Error),
	Lox(Box<(Location, String)>),
}

impl Display for Error {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		match self {
			Error::Io(inner) => Display::fmt(inner, f),
			Error::Lox(data) => {
				if f.alternate() {
					write!(f, "{location} ", location = data.0)?;
				}
				write!(f, "{message}", message = data.1)
			}
		}
	}
}

impl Debug for Error {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		write!(f, "{self:#}")
	}
}

impl From<std::io::Error> for Error {
	fn from(value: std::io::Error) -> Self {
		Error::Io(value)
	}
}
