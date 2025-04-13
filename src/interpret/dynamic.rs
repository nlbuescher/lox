use super::{Callable, Class, Instance};

pub trait Dynamic: ToString {
	fn as_callable(&self) -> Option<&dyn Callable>;
	fn as_class(&self) -> Option<&Class>;
	fn as_instance(&self) -> Option<&Instance>;
	fn as_instance_mut(&mut self) -> Option<&mut Instance>;
}
