use crate::interpret::{Environment, RuntimeError};
use crate::value::Value;

pub trait Callable: ToString {
	fn arity(&self) -> usize;

	fn call(&self, env: &mut Environment, args: &Vec<Value>) -> Result<Value, RuntimeError>;
}
