use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

use crate::error::Error;
use crate::interpret::{Callable, Environment, Function};
use crate::tokenize::Token;
use crate::value::Value;

pub fn as_class(object: Rc<RefCell<dyn Object>>) -> Option<Rc<RefCell<Class>>> {
	if !object.borrow().is_class() {
		return None;
	}

	let raw = Rc::into_raw(object) as *const RefCell<Class>;
	Some(unsafe { Rc::from_raw(raw) })
}

pub fn as_instance(object: Rc<RefCell<dyn Object>>) -> Option<Rc<RefCell<Instance>>> {
	if !object.borrow().is_instance() {
		return None;
	}

	let raw = Rc::into_raw(object) as *const RefCell<Instance>;
	Some(unsafe { Rc::from_raw(raw) })
}

pub trait Object: ToString {
	fn is_callable(&self) -> bool {
		false
	}

	fn as_callable(&self) -> Option<&dyn Callable> {
		None
	}

	fn is_class(&self) -> bool {
		false
	}

	fn as_class(&self) -> Option<&Class> {
		None
	}

	fn is_instance(&self) -> bool {
		false
	}

	fn as_instance(&self) -> Option<&Instance> {
		None
	}

	fn as_instance_mut(&mut self) -> Option<&mut Instance> {
		None
	}
}

pub trait Get {
	fn get(&self, name: &Token) -> Result<Value, Error>;
}

pub trait Set {
	fn set(&mut self, name: &Token, value: Value);
}

#[derive(Clone)]
pub struct Class {
	pub name: String,
	pub super_class: Option<Rc<RefCell<Class>>>,
	methods: FunctionMap,
	instance: Option<Instance>,
}

#[derive(Clone)]
pub struct Instance {
	pub class: Rc<Class>,
	fields: FieldMap,
}

#[derive(Default, Clone)]
struct FunctionMap(Rc<HashMap<String, Rc<RefCell<Function>>>>);

#[derive(Default, Clone)]
struct FieldMap(Rc<RefCell<HashMap<String, Value>>>);

/////////////////////////////////////////////////////////////////////////////
// Type implementations
/////////////////////////////////////////////////////////////////////////////

impl Class {
	pub fn new(
		name: String,
		super_class: Option<Rc<RefCell<Class>>>,
		class_methods: HashMap<String, Rc<RefCell<Function>>>,
		instance_methods: HashMap<String, Rc<RefCell<Function>>>,
	) -> Self {
		let meta_class = Class {
			name: format!("{name} class"),
			super_class: None,
			methods: FunctionMap(Rc::new(class_methods)),
			instance: None,
		};
		Class {
			name,
			super_class,
			methods: FunctionMap(Rc::new(instance_methods)),
			instance: Some(Instance::new(meta_class)),
		}
	}

	pub fn find_method(&self, name: &String) -> Option<Rc<RefCell<Function>>> {
		if let Some(method) = self.methods.get(name) {
			return Some(method.clone());
		}

		self.super_class.as_ref().and_then(|super_class| super_class.borrow().find_method(name))
	}
}

impl Instance {
	fn new(class: Class) -> Self {
		Instance { class: Rc::new(class), fields: FieldMap(Rc::new(RefCell::new(HashMap::new()))) }
	}
}

impl FunctionMap {
	fn get(&self, name: &String) -> Option<&Rc<RefCell<Function>>> {
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

	fn set(&self, name: String, value: Value) {
		self.0.borrow_mut().insert(name, value);
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

impl Display for Instance {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		write!(f, "{name} instance", name = self.class.name)
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
			initializer.borrow().arity()
		}
		else {
			0
		}
	}

	fn call(&self, env: &mut Environment, args: &[Value]) -> Result<Value, Error> {
		let instance = Instance::new(self.clone());

		if let Some(initializer) = self.find_method(&String::from("init")) {
			initializer.borrow().bind(instance.clone()).borrow().call(env, args)?;
		}

		Ok(Value::Object(Rc::new(RefCell::new(instance))))
	}
}

impl Object for Class {
	fn is_callable(&self) -> bool {
		true
	}

	fn as_callable(&self) -> Option<&dyn Callable> {
		Some(self)
	}

	fn is_class(&self) -> bool {
		true
	}

	fn as_class(&self) -> Option<&Class> {
		Some(self)
	}
}

impl Get for Class {
	fn get(&self, name: &Token) -> Result<Value, Error> {
		if let Some(instance) = &self.instance {
			return instance.get(name);
		}

		Err(Error::undefined_value(name))
	}
}

impl Set for Class {
	fn set(&mut self, name: &Token, value: Value) {
		if let Some(instance) = &mut self.instance {
			instance.set(name, value);
		}
	}
}

impl Object for Instance {
	fn is_instance(&self) -> bool {
		true
	}

	fn as_instance(&self) -> Option<&Instance> {
		Some(self)
	}

	fn as_instance_mut(&mut self) -> Option<&mut Instance> {
		Some(self)
	}
}

impl Get for Instance {
	fn get(&self, name: &Token) -> Result<Value, Error> {
		if self.fields.contains(&name.text) {
			return Ok(self.fields.get(&name.text).unwrap());
		}

		if let Some(method) = self.class.find_method(&name.text) {
			return Ok(Value::Object(method.borrow().bind(self.clone())));
		}

		Err(Error::undefined_value(name))
	}
}

impl Set for Instance {
	fn set(&mut self, name: &Token, value: Value) {
		self.fields.set(name.text.clone(), value);
	}
}
