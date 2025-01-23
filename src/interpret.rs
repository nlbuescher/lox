use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;

use crate::location::{Locatable, Location};
use crate::parse::{Expression, Statement};
use crate::tokenize::{Token, TokenKind};
use crate::value::Value;

pub type Result<T> = std::result::Result<T, RuntimeError>;

#[derive(Clone)]
pub struct Callable {
	arguments: Box<[Token]>,
	body: Body,
}

#[derive(Clone)]
pub enum Body {
	Statement(Rc<Statement>),
	Native(Rc<dyn Fn(&mut Environment, Vec<Value>) -> Result<Value>>),
}

impl Callable {
	fn arity(&self) -> usize {
		self.arguments.len()
	}

	fn call(&self, environment: &mut Environment, arguments: Vec<Value>) -> Result<Value> {
		match self.body {
			Body::Native(ref body) => body(environment, arguments),
			Body::Statement(ref body) => {
				let previous = environment.scope.clone();
				environment.scope = Rc::new(RefCell::new(Scope::with_parent(previous.clone())));

				for (name, argument) in self.arguments.iter().zip(arguments) {
					Scope::define(&mut environment.scope, &name.text, Some(argument));
				}

				let result = environment.execute_impl(body);

				environment.scope = previous.clone();

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

#[derive(Debug, Clone, Copy)]
pub enum TypeKind {
	Nothing,
	Bool,
	Number,
	String,
	Callable,
}

impl TypeKind {
	fn as_str(&self) -> &'static str {
		match self {
			TypeKind::Nothing => "Nothing",
			TypeKind::Bool => "Bool",
			TypeKind::Number => "Number",
			TypeKind::String => "String",
			TypeKind::Callable => "Callable",
		}
	}
}

impl Display for TypeKind {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		f.write_str(self.as_str())
	}
}

pub enum Break {
	Return(Value),
	Error(RuntimeError),
}

impl From<RuntimeError> for Break {
	fn from(error: RuntimeError) -> Self {
		Break::Error(error)
	}
}

#[derive(Debug)]
pub enum RuntimeError {
	TypeError { location: Location, expected: TypeKind, actual: TypeKind },
	UndefinedVariable(Box<Token>),
	UninitializedVariable(Box<Token>),
	UnexpectedVoid(Location),
	NotCallable(Location),
	UnexpectedNumberOfArguments { location: Location, expected: usize, actual: usize },
}

impl Display for RuntimeError {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		if f.alternate() {
			write!(f, "{location} ", location = self.location())?;
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

			RuntimeError::UnexpectedVoid(_) => write!(f, "Unexpected void"),

			RuntimeError::NotCallable(_) => write!(f, "Can only call functions and classes"),

			RuntimeError::UnexpectedNumberOfArguments { expected, actual, .. } => {
				write!(f, "Expected {expected} arguments but got {actual}")
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
			RuntimeError::UnexpectedVoid(location) => location,
			RuntimeError::NotCallable(location) => location,
			RuntimeError::UnexpectedNumberOfArguments { location, .. } => location,
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

	fn get(scope: &Rc<RefCell<Scope>>, name: &Box<Token>) -> Result<Value> {
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

	fn define(scope: &mut Rc<RefCell<Scope>>, name: &str, value: Option<Value>) {
		scope.borrow_mut().values.insert(name.into(), value);
	}

	fn assign(scope: &mut Rc<RefCell<Self>>, name: &Token, value: Value) -> Result<()> {
		if let Some(variable) = scope.borrow_mut().values.get_mut(&name.text) {
			*variable = Some(value);
			return Ok(());
		}

		if let Some(ref mut parent) = scope.borrow_mut().parent {
			return Scope::assign(parent, name, value);
		}

		Err(RuntimeError::UndefinedVariable(Box::new(name.clone())))
	}
}

impl Environment {
	pub fn new() -> Self {
		let mut environment = Environment { scope: Rc::new(RefCell::new(Scope::new())) };

		Scope::define(
			&mut environment.scope,
			"clock",
			Some(Value::Callable(Callable {
				arguments: Box::new([]),
				body: Body::Native(Rc::new(|_, _| {
					use std::time::{SystemTime, UNIX_EPOCH};

					let now = SystemTime::now();
					let duration = now.duration_since(UNIX_EPOCH).unwrap();
					let seconds = duration.as_secs() as f64;
					let nanos = duration.subsec_nanos() as f64 / 1_000_000_000.0;

					Ok(Value::Number(seconds + nanos))
				})),
			})),
		);

		environment
	}

	pub fn execute(&mut self, statement: &Statement) -> Result<Value> {
		self.execute_impl(statement).map_err(|error| match error {
			Break::Error(error) => error,
			Break::Return(_) => unreachable!("If you've landed here, the parser allowed a return statement outside of a function")
		})
	}

	fn execute_impl(&mut self, statement: &Statement) -> std::result::Result<Value, Break> {
		match statement {
			Statement::Block { statements, .. } => {
				let previous = self.scope.clone();
				self.scope = Rc::new(RefCell::new(Scope::with_parent(previous.clone())));

				let result = statements
					.iter()
					.try_fold(Value::Nil, |_, statement| self.execute_impl(statement));

				self.scope = previous.clone();

				result
			}

			Statement::Expression(expression) => Ok(self.evaluate(expression)?),

			Statement::For { initializer, condition, increment, body, .. } => {
				let previous = self.scope.clone();

				self.scope = Rc::new(RefCell::new(Scope::with_parent(previous.clone())));

				let result = initializer
					.as_ref()
					.map(|initializer| self.execute_impl(initializer))
					.transpose()
					.and_then(|_| {
						while condition
							.as_ref()
							.map(|condition| self.evaluate(condition))
							.transpose()?
							.unwrap_or_else(|| Value::Bool(true))
							.is_truthy()
						{
							self.execute_impl(body)?;

							if let Some(increment) = increment {
								self.execute_impl(&increment)?;
							}
						}

						Ok(())
					});

				self.scope = previous.clone();

				result.map(|_| Value::Nil)
			}

			Statement::Function { name, parameters, body, .. } => {
				let callable =
					Callable { arguments: parameters.clone(), body: Body::Statement(body.clone()) };
				Scope::define(&mut self.scope, &name.text, Some(Value::Callable(callable)));

				Ok(Value::Nil)
			}

			Statement::If { condition, then_branch, else_branch, .. } => {
				let condition = self.evaluate(condition)?;

				if condition.is_truthy() {
					self.execute_impl(then_branch)?;
				} else if let Some((_, else_branch)) = else_branch {
					self.execute_impl(else_branch)?;
				}
				Ok(Value::Nil)
			}

			Statement::Print { expression, .. } => {
				println!("{}", self.evaluate(expression)?.to_string());
				Ok(Value::Nil)
			}

			Statement::Return { expression, .. } => {
				let value = expression
					.as_ref()
					.map(|expression| self.evaluate(expression))
					.transpose()?
					.unwrap_or_else(|| Value::Nil);

				Err(Break::Return(value))
			}

			Statement::VariableDeclaration { name, initializer, .. } => {
				let value = match initializer {
					None => None,
					Some(ref expression) => Some(self.evaluate(expression)?),
				};

				Scope::define(&mut self.scope, &name.text, value);

				Ok(Value::Nil)
			}

			Statement::While { condition, body, .. } => {
				while self.evaluate(condition)?.is_truthy() {
					self.execute_impl(body)?;
				}

				Ok(Value::Nil)
			}
		}
	}

	fn evaluate(&mut self, expression: &Expression) -> Result<Value> {
		match expression {
			Expression::Assignment { name, value } => {
				let value = self.evaluate(value)?;
				Scope::assign(&mut self.scope, name, value.clone())?;
				Ok(value)
			}

			Expression::Binary { left, operator, right } => {
				if matches!(operator.kind, TokenKind::And | TokenKind::Or) {
					let left_value = self.evaluate(left)?;
					if operator.kind == TokenKind::Or && left_value.is_truthy() {
						Ok(left_value)
					} else if !left_value.is_truthy() {
						Ok(left_value)
					} else {
						self.evaluate(right)
					}
				} else {
					let left_value = self.evaluate(left)?;
					let right_value = self.evaluate(right)?;

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

			Expression::Call { callee, arguments_start_location, arguments, .. } => {
				let callee = self.evaluate(callee)?;

				let arguments = arguments
					.iter()
					.map(|argument| self.evaluate(argument))
					.collect::<Result<Vec<Value>>>()?;

				if let Value::Callable(callable) = callee {
					if arguments.len() == callable.arity() {
						Ok(callable.call(self, arguments)?)
					} else {
						Err(RuntimeError::UnexpectedNumberOfArguments {
							location: arguments_start_location.clone(),
							expected: callable.arity(),
							actual: arguments.len(),
						})
					}
				} else {
					Err(RuntimeError::NotCallable(arguments_start_location.clone()))
				}
			}

			Expression::Grouping(expression) => self.evaluate(expression),

			Expression::Literal(token) => match token.kind {
				TokenKind::Nil => Ok(Value::Nil),
				TokenKind::True => Ok(Value::Bool(true)),
				TokenKind::False => Ok(Value::Bool(false)),
				TokenKind::Number => Ok(token.value.clone().unwrap()),
				TokenKind::String => Ok(token.value.clone().unwrap()),
				it => unreachable!("Unknown token {} evaluating literal!", it),
			},

			Expression::Unary { operator, right } => {
				let right_value = self.evaluate(right)?;

				match operator.kind {
					TokenKind::Bang => Ok(Value::Bool(!right_value.is_truthy())),

					TokenKind::Minus => {
						let right_number = right_value.as_number(right)?;

						Ok(Value::Number(-right_number))
					}

					it => unreachable!("Unknown operator {} evaluating unary expression!", it),
				}
			}

			Expression::Variable(name) => Scope::get(&mut self.scope, name),
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
			Value::Callable(_) => TypeKind::Callable,
		}
	}

	fn is_truthy(&self) -> bool {
		match self {
			Value::Nil => false,
			Value::Bool(value) => *value,
			Value::Number(value) => *value != 0.0,
			Value::String(value) => !value.is_empty(),
			Value::Callable(_) => true,
		}
	}

	fn as_number(&self, expression: &Expression) -> Result<&f64> {
		match self {
			Value::Number(n) => Ok(n),
			_ => Err(RuntimeError::TypeError {
				location: expression.location().clone(),
				expected: TypeKind::Number,
				actual: self.type_kind(),
			}),
		}
	}

	fn as_string(&self, expression: &Expression) -> Result<&Box<str>> {
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
			Value::Callable(_) => "<callable>".to_string(),
		}
	}
}
