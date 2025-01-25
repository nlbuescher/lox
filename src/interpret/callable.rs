use std::cell::RefCell;
use std::fmt::{Debug, Formatter};
use std::rc::Rc;

use crate::interpret::error::{Break, RuntimeError};
use crate::interpret::scope::Scope;
use crate::interpret::visit::Visitor;
use crate::interpret::Environment;
use crate::parse::Statement;
use crate::tokenize::Token;
use crate::value::Value;

#[derive(Clone)]
pub struct Callable {
	pub(super) scope: Rc<RefCell<Scope>>,
	pub(super) arguments: Vec<Token>,
	pub(super) body: Body,
}

#[derive(Clone)]
pub enum Body {
	Statement(Rc<Statement>),
	Native(Rc<dyn Fn(&mut Environment, Vec<Value>) -> Result<Value, RuntimeError>>),
}

impl Callable {
	pub fn arity(&self) -> usize {
		self.arguments.len()
	}

	pub fn call(
		&self,
		environment: &mut Environment,
		arguments: Vec<Value>,
	) -> Result<Value, RuntimeError> {
		match self.body {
			Body::Native(ref body) => body(environment, arguments),
			Body::Statement(ref body) => {
				let result =
					environment.run_in_scope(Scope::with_parent(&self.scope), |environment| {
						for (name, argument) in self.arguments.iter().zip(arguments) {
							Scope::define(&mut environment.scope, &name.text, Some(argument));
						}

						environment.visit_statement(body)
					});

				match result {
					Ok(_) => Ok(Value::Nil),
					Err(Break::Return(value)) => Ok(value),
					Err(Break::Error(error)) => Err(error),
				}
			}
		}
	}
}

impl Debug for Callable {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		f.debug_struct("Callable").field("arguments", &self.arguments).finish()
	}
}

impl PartialEq for Callable {
	fn eq(&self, _other: &Self) -> bool {
		false
	}
}
