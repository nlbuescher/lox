use crate::interpret::Callable;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
	Nil,
	Bool(bool),
	Number(f64),
	String(String),
	Callable(Callable),
}
