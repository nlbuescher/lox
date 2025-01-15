use std::collections::HashMap;
use std::fmt::{Display, Formatter};

use crate::location::Location;
use crate::parse::{Expression, Statement};
use crate::tokenize::Token;
use crate::value::Value;

#[derive(Debug)]
pub enum RuntimeError {
	WrongType { location: Location, expected: String, actual: String },
	VariableNotDefined { location: Location, name: String },
}

impl Display for RuntimeError {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		use RuntimeError::*;

		match self {
			WrongType { location, expected, actual } => {
				write!(f, "{location} Expected {expected} but got {actual}")
			}
			VariableNotDefined { location, name } => {
				write!(f, "{location} {name} is not defined")
			}
		}
	}
}

impl Value {
	fn type_name(&self) -> &'static str {
		match self {
			Value::Nil => "Nothing",
			Value::Bool(_) => "Bool",
			Value::Number(_) => "Number",
			Value::String(_) => "String",
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
			_ => Err(RuntimeError::WrongType {
				location: expression.location().clone(),
				expected: "number".into(),
				actual: self.type_name().into(),
			}),
		}
	}

	fn as_string(&self, expression: &Expression) -> Result<String, RuntimeError> {
		match self {
			Value::String(s) => Ok(s.clone()),
			_ => Err(RuntimeError::WrongType {
				location: expression.location().clone(),
				expected: "string".into(),
				actual: self.type_name().into(),
			}),
		}
	}

	fn to_string(&self) -> String {
		match self {
			Value::Nil => "nil".to_string(),
			Value::Bool(b) => b.to_string(),
			Value::Number(n) => n.to_string(),
			Value::String(s) => s.clone(),
		}
	}
}

#[derive(Debug)]
pub struct Environment {
	values: HashMap<String, Value>,
}

impl Environment {
	pub fn new() -> Self {
		Environment { values: HashMap::new() }
	}

	pub fn execute(&mut self, statement: &Statement) -> Result<Option<Value>, RuntimeError> {
		match statement {
			Statement::Expression { expression: value } => {
				self.evaluate_expression(value).map(Some)
			}
			Statement::Print { expression: value } => {
				println!("{}", self.evaluate_expression(value)?.to_string());

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
		use crate::tokenize::TokenKind::*;
		match expression {
			Expression::Assignment { name: Token { location, text, .. }, value } => {
				if !self.values.contains_key(text) {
					return Err(RuntimeError::VariableNotDefined {
						location: location.clone(),
						name: text.clone(),
					});
				}

				let value = self.evaluate_expression(value)?;
				self.values.insert(text.clone(), value.clone());
				Ok(value)
			}

			Expression::Binary { left, operator, right } => {
				let left_value = self.evaluate_expression(left)?;
				let right_value = self.evaluate_expression(right)?;

				match operator.kind {
					Greater => {
						let left_number = left_value.as_number(left)?;
						let right_number = right_value.as_number(right)?;

						Ok(Value::Bool(left_number > right_number))
					}

					GreaterEqual => {
						let left_number = left_value.as_number(left)?;
						let right_number = right_value.as_number(right)?;

						Ok(Value::Bool(left_number >= right_number))
					}

					Less => {
						let left_number = left_value.as_number(left)?;
						let right_number = right_value.as_number(right)?;

						Ok(Value::Bool(left_number < right_number))
					}

					LessEqual => {
						let left_number = left_value.as_number(left)?;
						let right_number = right_value.as_number(right)?;

						Ok(Value::Bool(left_number <= right_number))
					}

					BangEqual => Ok(Value::Bool(left_value != right_value)),

					EqualEqual => Ok(Value::Bool(left_value == right_value)),

					Minus => {
						let left_number = left_value.as_number(left)?;
						let right_number = right_value.as_number(right)?;

						Ok(Value::Number(left_number - right_number))
					}

					Plus => match left_value.as_number(left) {
						Ok(left_number) => {
							let right_number = right_value.as_number(right)?;

							Ok(Value::Number(left_number + right_number))
						}
						Err(_) => {
							let left_string = left_value.as_string(left)?;
							let right_string = right_value.as_string(right)?;

							Ok(Value::String(format!("{left_string}{right_string}")))
						}
					},

					Slash => {
						let left_number = left_value.as_number(left)?;
						let right_number = right_value.as_number(right)?;

						Ok(Value::Number(left_number / right_number))
					}

					Star => {
						let left_number = left_value.as_number(left)?;
						let right_number = right_value.as_number(right)?;

						Ok(Value::Number(left_number * right_number))
					}

					_ => unreachable!("Unknown operator evaluating binary expression!"),
				}
			}

			Expression::Grouping(expression) => self.evaluate_expression(expression),

			Expression::Literal(value, _) => Ok(value.clone()),

			Expression::Unary { operator, right } => {
				let right_value = self.evaluate_expression(right)?;

				match operator.kind {
					Bang => Ok(Value::Bool(!right_value.is_truthy())),

					Minus => {
						let right_number = right_value.as_number(right)?;

						Ok(Value::Number(-right_number))
					}

					_ => unreachable!("Unknown operator evaluating unary expression!"),
				}
			}

			Expression::Variable(Token { location, text, .. }) => match self.values.get(text) {
				None => Err(RuntimeError::VariableNotDefined {
					location: location.clone(),
					name: text.clone(),
				}),
				Some(value) => Ok(value.clone()),
			},
		}
	}
}
