use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::interpret::RuntimeError;
use crate::tokenize::Token;
use crate::value::Value;

#[derive(Debug)]
pub struct Scope {
	parent: Option<Rc<RefCell<Scope>>>,
	values: HashMap<String, Option<Value>>,
}

impl Scope {
	pub(super) fn new() -> Rc<RefCell<Self>> {
		Rc::new(RefCell::new(Scope { parent: None, values: HashMap::new() }))
	}

	pub(super) fn with_parent(parent: &Rc<RefCell<Scope>>) -> Rc<RefCell<Self>> {
		Rc::new(RefCell::new(Scope { parent: Some(parent.clone()), values: HashMap::new() }))
	}

	pub(super) fn get(scope: &Rc<RefCell<Scope>>, name: &Token) -> Result<Value, RuntimeError> {
		if let Some(value) = scope.borrow().values.get(&name.text) {
			if let Some(value) = value {
				return Ok(value.clone());
			}
			return Err(RuntimeError::UninitializedVariable(name.clone()));
		}

		if let Some(ref parent) = scope.borrow().parent {
			return Scope::get(parent, name);
		}

		Err(RuntimeError::UndefinedVariable(name.clone()))
	}

	pub(super) fn define(scope: &mut Rc<RefCell<Scope>>, name: &str, value: Option<Value>) {
		scope.borrow_mut().values.insert(name.into(), value);
	}

	pub(super) fn assign(
		scope: &mut Rc<RefCell<Self>>,
		name: &Token,
		value: Value,
	) -> Result<(), RuntimeError> {
		if let Some(variable) = scope.borrow_mut().values.get_mut(&name.text) {
			*variable = Some(value);
			return Ok(());
		}

		if let Some(ref mut parent) = scope.borrow_mut().parent {
			return Scope::assign(parent, name, value);
		}

		Err(RuntimeError::UndefinedVariable(name.clone()))
	}
}
