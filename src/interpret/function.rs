use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use std::ops::Deref;
use std::rc::Rc;

use crate::error::Error;
use crate::interpret::error::Break;
use crate::interpret::visit::Visitor;
use crate::interpret::{Callable, Environment, Instance, Scope};
use crate::parse::BlockStatement;
use crate::tokenize::Token;
use crate::value::Value;

#[derive(Clone)]
pub struct Function {
	name: Option<String>,
	scope: Scope,
	parameters: Vec<Token>,
	pub body: Rc<BlockStatement>,
}

pub struct NativeFunction {
	pub body: Rc<NativeBody>,
}

type NativeBody = dyn Fn(&mut Environment, &[Value]) -> Result<Value, Error>;

impl Function {
	pub fn new(
		name: Option<String>,
		scope: Scope,
		parameters: Vec<Token>,
		body: Rc<BlockStatement>,
	) -> Self {
		Function { name, scope, parameters, body }
	}

	pub fn bind(&self, instance: Rc<RefCell<Instance>>) -> Function {
		let mut scope = Scope::with_parent(&self.scope);
		scope.define("this", Some(Value::Instance(instance)));
		Function::new(self.name.clone(), scope, self.parameters.clone(), self.body.clone())
	}
}

impl NativeFunction {
	pub fn new<F>(body: F) -> Self
	where
		F: Fn(&mut Environment, &[Value]) -> Result<Value, Error> + 'static,
	{
		NativeFunction { body: Rc::new(body) }
	}
}

impl Display for Function {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		if let Some(ref name) = self.name {
			write!(f, "<fn {name}")
		} else {
			write!(f, "<anonymous fn>")
		}
	}
}

impl Display for NativeFunction {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "<native fn>")
	}
}

impl Callable for Function {
	fn arity(&self) -> usize {
		self.parameters.len()
	}

	fn call(self: Rc<Self>, env: &mut Environment, args: &[Value]) -> Result<Value, Error> {
		let scope = Scope::with_parent(&self.scope);

		let result = env.run_in_scope(scope, |environment| {
			for (name, argument) in self.parameters.iter().zip(args) {
				environment.scope.borrow_mut().define(&name.text, Some(argument.clone()));
			}

			environment.visit_statement(self.body.deref())
		});

		match result {
			Ok(_) => Ok(Value::Nil),
			Err(Break::Return(value)) => Ok(value),
			Err(Break::Error(error)) => Err(error),
		}
	}
}

impl Callable for NativeFunction {
	fn arity(&self) -> usize {
		0
	}

	fn call(self: Rc<Self>, env: &mut Environment, args: &[Value]) -> Result<Value, Error> {
		self.body.deref()(env, args)
	}
}
