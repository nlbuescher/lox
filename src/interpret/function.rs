use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

use crate::error::Error;
use crate::interpret::error::Break;
use crate::interpret::object::Object;
use crate::interpret::visit::Visitor;
use crate::interpret::{Callable, Environment, Instance, Scope};
use crate::location::Locate;
use crate::parse::BlockStatement;
use crate::tokenize::{Token, TokenKind};
use crate::value::Value;

#[derive(Clone)]
pub struct Function {
	name: Option<String>,
	is_initializer: bool,
	scope: Scope,
	parameters: Vec<Token>,
	pub body: Rc<BlockStatement>,
}

pub struct NativeFunction {
	arity: usize,
	pub body: Rc<NativeBody>,
}

type NativeBody = dyn Fn(&mut Environment, &[Value]) -> Result<Value, Error>;

impl Function {
	pub fn new(
		name: Option<String>,
		is_initializer: bool,
		scope: Scope,
		parameters: Vec<Token>,
		body: Rc<BlockStatement>,
	) -> Self {
		Function { name, is_initializer, scope, parameters, body }
	}

	pub fn bind(&self, instance: Instance) -> Rc<RefCell<Function>> {
		let mut scope = Scope::with_parent(self.scope.clone());
		scope.define("this", Some(Value::Object(Rc::new(RefCell::new(instance)))));
		Rc::new(RefCell::new(Function::new(
			self.name.clone(),
			self.is_initializer,
			scope,
			self.parameters.clone(),
			self.body.clone(),
		)))
	}
}

impl NativeFunction {
	pub fn new(
		arity: usize,
		body: impl Fn(&mut Environment, &[Value]) -> Result<Value, Error> + 'static,
	) -> Self {
		NativeFunction { arity, body: Rc::new(body) }
	}
}

impl Display for Function {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		if let Some(name) = &self.name {
			write!(f, "<fn {name}")
		}
		else {
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

	fn call(&self, env: &mut Environment, args: &[Value]) -> Result<Value, Error> {
		let scope = Scope::with_parent(self.scope.clone());

		let result = env.run_in_scope(scope, |environment| {
			for (name, argument) in self.parameters.iter().zip(args) {
				environment.scope.borrow_mut().define(&name.text, Some(argument.clone()));
			}

			environment.visit_block(&self.body.statements)
		});

		if let Err(Break::Error(error)) = result {
			return Err(error);
		}

		if self.is_initializer {
			if let Err(Break::Return(value)) = result {
				if !matches!(value, Value::Nil) {
					return Err(Error::initializer_return(self.body.locate()));
				}
			}
			let this = Token::with_text(self.body.locate().clone(), TokenKind::Identifier, "this");
			return self.scope.get(&this);
		}

		match result {
			Ok(_) => Ok(Value::Nil),
			Err(Break::Return(value)) => Ok(value),
			Err(Break::Error(error)) => Err(error),
		}
	}
}

impl Callable for NativeFunction {
	fn arity(&self) -> usize {
		self.arity
	}

	fn call(&self, env: &mut Environment, args: &[Value]) -> Result<Value, Error> {
		(self.body)(env, args)
	}
}

impl Object for Function {
	fn is_callable(&self) -> bool {
		true
	}

	fn as_callable(&self) -> Option<&dyn Callable> {
		Some(self)
	}
}

impl Object for NativeFunction {
	fn is_callable(&self) -> bool {
		true
	}

	fn as_callable(&self) -> Option<&dyn Callable> {
		Some(self)
	}
}
