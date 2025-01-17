use std::collections::HashMap;
use std::fmt::{Display, Formatter};

use crate::location::{Locatable, Location};
use crate::parse::{Expression, Statement};
use crate::tokenize::{Token, TokenKind};
use crate::value::Value;

#[derive(Debug, Clone, Copy)]
pub enum TypeKind {
	Nothing,
	Bool,
	Number,
	String,
}

impl TypeKind {
	fn as_str(&self) -> &'static str {
		match self {
			TypeKind::Nothing => "Nothing",
			TypeKind::Bool => "Bool",
			TypeKind::Number => "Number",
			TypeKind::String => "String",
		}
	}
}

impl Display for TypeKind {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		f.write_str(self.as_str())
	}
}

#[derive(Debug)]
pub enum RuntimeError {
	TypeError { location: Location, expected: TypeKind, actual: TypeKind },
	UndefinedVariable(Box<Token>),
}

impl Display for RuntimeError {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		match self {
			RuntimeError::TypeError { expected, actual, .. } => {
				write!(f, "Expected {expected} but got {actual}")
			}

			RuntimeError::UndefinedVariable(name) => {
				write!(f, "Undefined variable '{name}'", name = name.text)
			}
		}
	}
}

impl Locatable for RuntimeError {
	fn location(&self) -> &Location {
		match self {
			RuntimeError::TypeError { location, .. } => location,
			RuntimeError::UndefinedVariable(name) => name.location(),
		}
	}
}

#[derive(Debug)]
pub struct Environment {
	scope: Option<Box<Scope>>,
}

#[derive(Debug)]
pub struct Scope {
	parent: Option<Box<Scope>>,
	values: HashMap<Box<str>, Value>,
}

impl Scope {
	fn new() -> Self {
		Scope { parent: None, values: HashMap::new() }
	}

	fn with_parent(parent: Box<Scope>) -> Self {
		Scope { parent: Some(parent), values: HashMap::new() }
	}

	fn get(&self, name: &Token) -> Result<&Value, RuntimeError> {
		if let Some(value) = self.values.get(&name.text) {
			return Ok(value);
		}

		if let Some(ref parent) = self.parent {
			return parent.get(name);
		}

		Err(RuntimeError::UndefinedVariable(Box::new(name.clone())))
	}

	fn define(&mut self, name: &Token, value: Value) {
		self.values.insert(name.text.clone(), value);
	}

	fn assign(&mut self, name: &Token, value: Value) -> Result<(), RuntimeError> {
		if let Some(variable) = self.values.get_mut(&name.text) {
			*variable = value.clone();
			return Ok(());
		}

		if let Some(ref mut parent) = self.parent {
			return parent.assign(name, value);
		}

		Err(RuntimeError::UndefinedVariable(Box::new(name.clone())))
	}

	pub fn execute(
		mut self: Box<Self>,
		statement: &Statement,
	) -> Result<(Box<Scope>, Option<Value>), RuntimeError> {
		match statement {
			Statement::Block { statements, .. } => {
				let mut inner = Box::new(Scope::with_parent(self));

				for statement in statements {
					let (scope, _) = inner.execute(statement)?;
					inner = scope;
				}

				self = inner.parent.take().unwrap();

				Ok((self, None))
			}

			Statement::Expression { expression } => {
				let value = self.evaluate_expression(expression)?;
				Ok((self, Some(value)))
			}

			Statement::Print { expression, .. } => {
				println!("{}", self.evaluate_expression(expression)?.to_string());
				Ok((self, None))
			}

			Statement::VariableDeclaration { name, initializer } => {
				let value = match initializer {
					Some(initializer) => self.evaluate_expression(initializer)?,
					None => Value::Nil,
				};

				self.define(name, value);

				Ok((self, None))
			}
		}
	}

	fn evaluate_expression(&mut self, expression: &Expression) -> Result<Value, RuntimeError> {
		match expression {
			Expression::Assignment { name, value } => {
				let value = self.evaluate_expression(value)?;
				self.assign(name, value.clone())?;
				Ok(value)
			}

			Expression::Binary { left, operator, right } => {
				let left_value = self.evaluate_expression(left)?;
				let right_value = self.evaluate_expression(right)?;

				match operator.kind {
					TokenKind::Greater => {
						let left_number = left_value.as_number(left)?;
						let right_number = right_value.as_number(right)?;

						Ok(Value::Bool(left_number > right_number))
					}

					TokenKind::GreaterEqual => {
						let left_number = left_value.as_number(left)?;
						let right_number = right_value.as_number(right)?;

						Ok(Value::Bool(left_number >= right_number))
					}

					TokenKind::Less => {
						let left_number = left_value.as_number(left)?;
						let right_number = right_value.as_number(right)?;

						Ok(Value::Bool(left_number < right_number))
					}

					TokenKind::LessEqual => {
						let left_number = left_value.as_number(left)?;
						let right_number = right_value.as_number(right)?;

						Ok(Value::Bool(left_number <= right_number))
					}

					TokenKind::BangEqual => Ok(Value::Bool(left_value != right_value)),

					TokenKind::EqualEqual => Ok(Value::Bool(left_value == right_value)),

					TokenKind::Minus => {
						let left_number = left_value.as_number(left)?;
						let right_number = right_value.as_number(right)?;

						Ok(Value::Number(left_number - right_number))
					}

					TokenKind::Plus => match left_value.as_number(left) {
						Ok(left_number) => {
							let right_number = right_value.as_number(right)?;

							Ok(Value::Number(left_number + right_number))
						}
						Err(_) => {
							let left_string = left_value.as_string(left)?;
							let right_string = right_value.as_string(right)?;

							Ok(Value::String(format!("{left_string}{right_string}").into()))
						}
					},

					TokenKind::Slash => {
						let left_number = left_value.as_number(left)?;
						let right_number = right_value.as_number(right)?;

						Ok(Value::Number(left_number / right_number))
					}

					TokenKind::Star => {
						let left_number = left_value.as_number(left)?;
						let right_number = right_value.as_number(right)?;

						Ok(Value::Number(left_number * right_number))
					}

					it => unreachable!("Unknown operator {} evaluating binary expression!", it),
				}
			}

			Expression::Grouping(expression) => self.evaluate_expression(expression),

			Expression::Literal(token) => match token.kind {
				TokenKind::Nil => Ok(Value::Nil),
				TokenKind::True => Ok(Value::Bool(true)),
				TokenKind::False => Ok(Value::Bool(false)),
				TokenKind::Number => Ok(token.value.clone().unwrap()),
				TokenKind::String => Ok(token.value.clone().unwrap()),
				it => unreachable!("Unknown token {} evaluating literal!", it),
			},

			Expression::Unary { operator, right } => {
				let right_value = self.evaluate_expression(right)?;

				match operator.kind {
					TokenKind::Bang => Ok(Value::Bool(!right_value.is_truthy())),

					TokenKind::Minus => {
						let right_number = right_value.as_number(right)?;

						Ok(Value::Number(-right_number))
					}

					it => unreachable!("Unknown operator {} evaluating unary expression!", it),
				}
			}

			Expression::Variable(name) => self.get(name).cloned(),
		}
	}
}

impl Environment {
	pub fn new() -> Self {
		Environment { scope: Some(Box::new(Scope::new())) }
	}

	pub fn execute(&mut self, statement: &Statement) -> Result<Option<Value>, RuntimeError> {
		// temporarily move scope out of environment
		let scope = self.scope.take().unwrap();

		// get scope back from execution
		let (scope, value) = scope.execute(statement)?;

		// put scope back
		self.scope = Some(scope);

		Ok(value)
	}
}

impl Value {
	fn type_kind(&self) -> TypeKind {
		match self {
			Value::Nil => TypeKind::Nothing,
			Value::Bool(_) => TypeKind::Bool,
			Value::Number(_) => TypeKind::Number,
			Value::String(_) => TypeKind::String,
		}
	}

	fn is_truthy(&self) -> bool {
		match self {
			Value::Nil => false,
			Value::Bool(b) => *b,
			Value::Number(n) => *n != 0.0,
			Value::String(s) => !s.is_empty(),
		}
	}

	fn as_number(&self, expression: &Expression) -> Result<&f64, RuntimeError> {
		match self {
			Value::Number(n) => Ok(n),
			_ => Err(RuntimeError::TypeError {
				location: expression.location().clone(),
				expected: TypeKind::Number,
				actual: self.type_kind(),
			}),
		}
	}

	fn as_string(&self, expression: &Expression) -> Result<&Box<str>, RuntimeError> {
		match self {
			Value::String(s) => Ok(s),
			_ => Err(RuntimeError::TypeError {
				location: expression.location().clone(),
				expected: TypeKind::String,
				actual: self.type_kind(),
			}),
		}
	}

	fn to_string(&self) -> String {
		match self {
			Value::Nil => "nil".to_string(),
			Value::Bool(b) => b.to_string(),
			Value::Number(n) => n.to_string(),
			Value::String(s) => s.to_string(),
		}
	}
}
