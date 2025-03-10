use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

use crate::error::Error;
use crate::interpret::{Callable, Environment, Function};
use crate::tokenize::Token;
use crate::value::Value;

pub trait Object {
	fn get(&self, name: &Token) -> Result<Value, Error>;
	fn set(&mut self, name: &Token, value: Value);
}

#[derive(Clone)]
pub struct Class {
	pub name: String,
	methods: FunctionMap,
	instance: Option<Instance>,
}

#[derive(Clone)]
pub struct Instance {
	pub class: Rc<Class>,
	fields: FieldMap,
}

#[derive(Default, Clone)]
struct FunctionMap(Rc<HashMap<String, Rc<Function>>>);

#[derive(Default, Clone)]
struct FieldMap(Rc<RefCell<HashMap<String, Value>>>);

/////////////////////////////////////////////////////////////////////////////
// Type implementations
/////////////////////////////////////////////////////////////////////////////

impl Class {
	pub fn new(
		name: String,
		class_methods: HashMap<String, Rc<Function>>,
		instance_methods: HashMap<String, Rc<Function>>,
	) -> Self {
		let meta_class = Class {
			name: format!("{name} class"),
			methods: FunctionMap(Rc::new(class_methods)),
			instance: None,
		};
		Class {
			name,
			methods: FunctionMap(Rc::new(instance_methods)),
			instance: Some(Instance::new(meta_class)),
		}
	}

	pub fn find_method(&self, name: &String) -> Option<Rc<Function>> {
		self.methods.get(name).cloned()
	}
}

impl Instance {
	fn new(class: Class) -> Self {
		Instance { class: Rc::new(class), fields: FieldMap(Rc::new(RefCell::new(HashMap::new()))) }
	}
}

impl FunctionMap {
	fn get(&self, name: &String) -> Option<&Rc<Function>> {
		self.0.get(name)
	}
}

impl FieldMap {
	fn contains(&self, name: &String) -> bool {
		self.0.borrow().contains_key(name)
	}

	fn get(&self, name: &String) -> Option<Value> {
		self.0.borrow().get(name).cloned()
	}

	fn set(&self, name: &String, value: Value) {
		self.0.borrow_mut().insert(name.clone(), value);
	}
}

/////////////////////////////////////////////////////////////////////////////
// Display
/////////////////////////////////////////////////////////////////////////////

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

	fn call(&self, env: &mut Environment, args: &[Value]) -> Result<Value, Error> {
		let instance = Instance::new(self.clone());

		if let Some(initializer) = self.find_method(&String::from("init")) {
			initializer.bind(instance.clone()).call(env, args)?;
		}

		Ok(Value::Instance(Box::new(instance)))
	}
}

impl Object for Class {
	fn get(&self, name: &Token) -> Result<Value, Error> {
		if let Some(ref instance) = self.instance {
			return instance.get(name);
		}

		Err(Error::undefined_value(name))
	}

	fn set(&mut self, name: &Token, value: Value) {
		if let Some(ref mut instance) = self.instance {
			instance.set(name, value);
		}
	}
}

impl Object for Instance {
	fn get(&self, name: &Token) -> Result<Value, Error> {
		if self.fields.contains(&name.text) {
			return Ok(self.fields.get(&name.text).unwrap());
		}

		if let Some(method) = self.class.find_method(&name.text) {
			return Ok(Value::Function(method.bind(self.clone())));
		}

		Err(Error::undefined_value(name))
	}

	fn set(&mut self, name: &Token, value: Value) {
		self.fields.set(&name.text, value);
	}
}
