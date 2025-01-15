use std::fmt::{Display, Formatter};

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
		write!(f, "[{}:{}]", self.line, self.column)
	}
}
