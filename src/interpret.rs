use std::fmt::{Display, Formatter};

use crate::location::Location;
use crate::parse::{Expression, Statement};
use crate::value::Value;

#[derive(Debug)]
pub struct RuntimeError {
	pub location: Location,
	pub expected: String,
	pub actual: String,
}

impl Display for RuntimeError {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		let RuntimeError { location, expected, actual } = self;
		write!(f, "{location} Expected {expected} but got {actual}")
	}
}

impl Value {
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
			_ => Err(RuntimeError {
				location: expression.location(),
				expected: "number".into(),
				actual: self.to_string(),
			}),
		}
	}

	fn as_string(&self, expression: &Expression) -> Result<String, RuntimeError> {
		match self {
			Value::String(s) => Ok(s.clone()),
			_ => Err(RuntimeError {
				location: expression.location(),
				expected: "string".into(),
				actual: self.to_string(),
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
pub struct Interpreter {}

impl Interpreter {
	pub fn new() -> Self {
		Interpreter {}
	}

	pub fn execute(&mut self, statement: &Statement) -> Result<Option<Value>, RuntimeError> {
		match statement {
			Statement::Expression(expression) => self.evaluate_expression(expression).map(Some),
			Statement::Print(expression) => {
				println!("{}", self.evaluate_expression(expression)?.to_string());

				Ok(None)
			}
		}
	}

	fn evaluate_expression(&self, expression: &Expression) -> Result<Value, RuntimeError> {
		use crate::tokenize::TokenKind::*;
		match expression {
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
		}
	}
}
