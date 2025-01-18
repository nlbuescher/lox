use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

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
	UninitializedVariable(Box<Token>),
}

impl Display for RuntimeError {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		if f.alternate() {
			self.location().fmt(f)?;
		}
		match self {
			RuntimeError::TypeError { expected, actual, .. } => {
				write!(f, "Expected {expected} but got {actual}")
			}

			RuntimeError::UndefinedVariable(name) => {
				write!(f, "Undefined variable '{name}'", name = name.text)
			}

			RuntimeError::UninitializedVariable(name) => {
				write!(f, "Uninitialized variable '{name}'", name = name.text)
			}
		}
	}
}

impl Locatable for RuntimeError {
	fn location(&self) -> &Location {
		match self {
			RuntimeError::TypeError { location, .. } => location,
			RuntimeError::UndefinedVariable(name) => name.location(),
			RuntimeError::UninitializedVariable(name) => name.location(),
		}
	}
}

#[derive(Debug)]
pub struct Environment {
	scope: Rc<RefCell<Scope>>,
}

#[derive(Debug)]
pub struct Scope {
	parent: Option<Rc<RefCell<Scope>>>,
	values: HashMap<Box<str>, Option<Value>>,
}

impl Scope {
	fn new() -> Self {
		Scope { parent: None, values: HashMap::new() }
	}

	fn with_parent(parent: Rc<RefCell<Scope>>) -> Self {
		Scope { parent: Some(parent), values: HashMap::new() }
	}

	fn get(scope: &Rc<RefCell<Scope>>, name: &Box<Token>) -> Result<Value, RuntimeError> {
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

	fn define(scope: &mut Rc<RefCell<Scope>>, name: &Token, value: Option<Value>) {
		scope.borrow_mut().values.insert(name.text.clone(), value);
	}

	fn assign(
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

		Err(RuntimeError::UndefinedVariable(Box::new(name.clone())))
	}

	fn evaluate_expression(
		scope: &mut Rc<RefCell<Scope>>,
		expression: &Expression,
	) -> Result<Value, RuntimeError> {
		match expression {
			Expression::Assignment { name, value } => {
				let value = Scope::evaluate_expression(scope, value)?;
				Scope::assign(scope, name, value.clone())?;
				Ok(value)
			}

			Expression::Binary { left, operator, right } => {
				if matches!(operator.kind, TokenKind::And | TokenKind::Or) {
					let left_value = Scope::evaluate_expression(scope, left)?;

					if operator.kind == TokenKind::Or && left_value.is_truthy() {
						Ok(left_value)
					}
					else if !left_value.is_truthy() {
						Ok(left_value)
					}
					else {
						Scope::evaluate_expression(scope, right)
					}
				}
				else {
					let left_value = Scope::evaluate_expression(scope, left)?;
					let right_value = Scope::evaluate_expression(scope, right)?;

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
			}

			Expression::Grouping(expression) => Scope::evaluate_expression(scope, expression),

			Expression::Literal(token) => match token.kind {
				TokenKind::Nil => Ok(Value::Nil),
				TokenKind::True => Ok(Value::Bool(true)),
				TokenKind::False => Ok(Value::Bool(false)),
				TokenKind::Number => Ok(token.value.clone().unwrap()),
				TokenKind::String => Ok(token.value.clone().unwrap()),
				it => unreachable!("Unknown token {} evaluating literal!", it),
			},

			Expression::Unary { operator, right } => {
				let right_value = Scope::evaluate_expression(scope, right)?;

				match operator.kind {
					TokenKind::Bang => Ok(Value::Bool(!right_value.is_truthy())),

					TokenKind::Minus => {
						let right_number = right_value.as_number(right)?;

						Ok(Value::Number(-right_number))
					}

					it => unreachable!("Unknown operator {} evaluating unary expression!", it),
				}
			}

			Expression::Variable(name) => Scope::get(scope, name),
		}
	}
}

impl Environment {
	pub fn new() -> Self {
		Environment { scope: Rc::new(RefCell::new(Scope::new())) }
	}

	pub fn execute(&mut self, statement: &Statement) -> Result<Option<Value>, RuntimeError> {
		match statement {
			Statement::Block { statements, .. } => {
				let previous = self.scope.clone();
				self.scope = Rc::new(RefCell::new(Scope::with_parent(previous.clone())));

				let result =
					statements.iter().try_fold(None, |_, statement| self.execute(statement));

				self.scope = previous.clone();

				result
			}

			Statement::Expression(expression) => {
				let value = Scope::evaluate_expression(&mut self.scope, expression)?;
				Ok(Some(value))
			}

			Statement::For { initializer, condition, increment, body, .. } => {
				if let Some(initializer) = initializer {
					self.execute(initializer)?;
				}

				while condition
					.as_ref()
					.map(|condition| Scope::evaluate_expression(&mut self.scope, &condition))
					.transpose()?
					.unwrap_or(Value::Bool(true))
					.is_truthy()
				{
					self.execute(body)?;

					if let Some(increment) = increment {
						self.execute(&increment)?;
					}
				}

				Ok(None)
			}

			Statement::If { condition, then_branch, else_branch, .. } => {
				let condition = Scope::evaluate_expression(&mut self.scope, condition)?;

				if condition.is_truthy() {
					self.execute(then_branch)?;
					Ok(None)
				}
				else if let Some(else_branch) = else_branch {
					self.execute(else_branch)?;
					Ok(None)
				}
				else {
					Ok(None)
				}
			}

			Statement::Print { expression, .. } => {
				println!(
					"{}",
					Scope::evaluate_expression(&mut self.scope, expression)?.to_string()
				);
				Ok(None)
			}

			Statement::VariableDeclaration { name, initializer, .. } => {
				let value = match initializer {
					None => None,
					Some(ref expression) => {
						Some(Scope::evaluate_expression(&mut self.scope, expression)?)
					}
				};

				Scope::define(&mut self.scope, name, value);

				Ok(None)
			}

			Statement::While { condition, body, .. } => {
				while Scope::evaluate_expression(&mut self.scope, condition)?.is_truthy() {
					self.execute(body)?;
				}

				Ok(None)
			}
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
