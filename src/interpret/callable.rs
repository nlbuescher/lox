use crate::error::Error;
use crate::interpret::dynamic::Dynamic;
use crate::interpret::Environment;
use crate::value::Value;

pub trait Callable: Dynamic + ToString {
	fn arity(&self) -> usize;

	fn call(&self, env: &mut Environment, args: &[Value]) -> Result<Value, Error>;
}
