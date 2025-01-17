use std::collections::HashMap;
use std::fmt::{Display, Formatter};

use crate::location::{Locatable, Location};
use crate::parse::{Expression, Statement};
use crate::tokenize::TokenKind;
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
	VariableNotDefined { location: Location, name: Box<str> },
}

impl Display for RuntimeError {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		match self {
			RuntimeError::TypeError { location, expected, actual } => {
				write!(f, "{location} Expected {expected} but got {actual}")
			}
			RuntimeError::VariableNotDefined { location, name } => {
				write!(f, "{location} {name} is not defined")
			}
		}
	}
}

impl Locatable for RuntimeError {
	fn location(&self) -> &Location {
		match self {
			RuntimeError::TypeError { location, .. } => location,
			RuntimeError::VariableNotDefined { location, .. } => location,
		}
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

#[derive(Debug)]
pub struct Environment {
	values: HashMap<Box<str>, Value>,
}

impl Environment {
	pub fn new() -> Self {
		Environment { values: HashMap::new() }
	}

	pub fn execute(&mut self, statement: &Statement) -> Result<Option<Value>, RuntimeError> {
		match statement {
			Statement::Expression { expression } => self.evaluate_expression(expression).map(Some),
			Statement::Print { expression, .. } => {
				println!("{}", self.evaluate_expression(expression)?.to_string());
				Ok(None)
			}
			Statement::VariableDeclaration { name, initializer } => {
				let value = match initializer {
					Some(initializer) => self.evaluate_expression(initializer)?,
					None => Value::Nil,
				};

				self.values.insert(name.text.clone(), value);

				Ok(None)
			}
		}
	}

	fn evaluate_expression(&mut self, expression: &Expression) -> Result<Value, RuntimeError> {
		match expression {
			Expression::Assignment { name, value } => {
				if !self.values.contains_key(&name.text) {
					return Err(RuntimeError::VariableNotDefined {
						location: name.location().clone(),
						name: name.text.clone(),
					});
				}

				let value = self.evaluate_expression(value)?;
				self.values.insert(name.text.clone(), value.clone());
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

			Expression::Variable(name) => match self.values.get(&name.text) {
				None => Err(RuntimeError::VariableNotDefined {
					location: name.location().clone(),
					name: name.text.clone(),
				}),
				Some(value) => Ok(value.clone()),
			},
		}
	}
}
