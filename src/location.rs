use std::fmt::{Display, Formatter};

pub trait Locate {
	fn locate(&self) -> &Location;
}

#[derive(Debug, Clone, PartialEq)]
pub struct Location {
	pub line: u32,
	pub column: u32,
}

impl Location {
	pub fn new(line: u32, column: u32) -> Location {
		Location { line, column }
	}
}

impl Display for Location {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		let Location { line, column } = self;
		// 3 characters gets us up to 999, which should be enough for most cases
		write!(f, "[{line:>3}:{column:<3}]")
	}
}
