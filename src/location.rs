use std::fmt::{Display, Formatter};

pub trait Locatable {
	fn location(&self) -> &Location;
}

impl<T: Locatable, E: Locatable> Locatable for Result<T, E> {
	fn location(&self) -> &Location {
		match self {
			Ok(value) => value.location(),
			Err(error) => error.location(),
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct Location {
	pub line: usize,
	pub column: usize,
}

impl Location {
	pub fn new(line: usize, column: usize) -> Location {
		Location { line, column }
	}
}

impl Display for Location {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		let Location { line, column } = self;
		let width = f.width().unwrap_or(0);
		write!(f, "{:width$} ", format!("[{line}:{column}]"))
	}
}
