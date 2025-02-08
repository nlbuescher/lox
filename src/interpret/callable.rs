use std::rc::Rc;

use crate::error::Error;
use crate::interpret::Environment;
use crate::value::Value;

pub trait Callable: ToString {
	fn arity(&self) -> usize;

	fn call(self: Rc<Self>, env: &mut Environment, args: &[Value]) -> Result<Value, Error>;
}
