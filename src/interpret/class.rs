use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

use crate::error::Error;
use crate::interpret::{Callable, Environment, Function};
use crate::tokenize::Token;
use crate::value::Value;

#[derive(Clone)]
pub struct Class {
	pub name: String,
	methods: HashMap<String, Rc<Function>>,
}

#[derive(Clone)]
pub struct Instance {
	pub class: Rc<Class>,
	fields: HashMap<String, Value>,
}

impl Class {
	pub fn new(name: String, methods: HashMap<String, Rc<Function>>) -> Self {
		Class { name, methods }
	}

	pub fn find_method(&self, name: &String) -> Option<&Rc<Function>> {
		self.methods.get(name)
	}
}

impl Instance {
	fn new(class: Rc<Class>) -> Self {
		Instance { class, fields: HashMap::new() }
	}

	pub fn get(instance: Rc<RefCell<Instance>>, name: &Token) -> Result<Value, Error> {
		if instance.borrow().fields.contains_key(&name.text) {
			return Ok(instance.borrow().fields.get(&name.text).unwrap().clone());
		}

		if let Some(method) = instance.borrow().class.find_method(&name.text) {
			return Ok(Value::Function(method.bind(instance.clone())));
		}

		Err(Error::undefined_value(name))
	}

	pub fn set(&mut self, name: &Token, value: &Value) {
		self.fields.insert(name.text.clone(), value.clone());
	}
}

impl Display for Class {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		write!(f, "{name}", name = self.name)
	}
}

impl PartialEq for Class {
	fn eq(&self, _other: &Self) -> bool {
		false
	}
}

impl Callable for Class {
	fn arity(&self) -> usize {
		if let Some(initializer) = self.find_method(&String::from("init")) {
			initializer.arity()
		} else {
			0
		}
	}

	fn call(self: Rc<Self>, env: &mut Environment, args: &[Value]) -> Result<Value, Error> {
		let instance = Rc::new(RefCell::new(Instance::new(self.clone())));

		if let Some(initializer) = self.find_method(&String::from("init")) {
			initializer.bind(instance.clone()).call(env, args)?;
		}

		Ok(Value::Instance(instance))
	}
}
