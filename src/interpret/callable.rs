use crate::error::Error;
use crate::interpret::{Environment, Object};
use crate::value::Value;

pub trait Callable: Object + ToString {
	fn arity(&self) -> usize;

	fn call(&self, env: &mut Environment, args: &[Value]) -> Result<Value, Error>;
}
