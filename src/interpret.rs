use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::ops::Deref;
use std::rc::Rc;

use crate::interpret::visit::Visitor;
use crate::location::{Locatable, Location};
use crate::parse::{Expression, Statement};
use crate::tokenize::{Token, TokenKind};
use crate::value::Value;

pub type Result<T> = std::result::Result<T, RuntimeError>;

#[derive(Debug)]
pub struct Environment {
	scope: Rc<RefCell<Scope>>,
}

#[derive(Debug)]
pub struct Scope {
	parent: Option<Rc<RefCell<Scope>>>,
	values: HashMap<Box<str>, Option<Value>>,
}

#[derive(Debug, Clone, Copy)]
pub enum TypeKind {
	Nothing,
	Bool,
	Number,
	String,
	Callable,
}

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

mod visit {
	use std::rc::Rc;

	use super::{Break, RuntimeError};
	use crate::location::Location;
	use crate::parse::{Expression, Statement};
	use crate::tokenize::Token;

	pub trait Visitor<T> {
		fn visit_statement(&mut self, statement: &Statement) -> Result<T, Break>;

		fn visit_block(&mut self, statements: &Box<[Statement]>) -> Result<T, Break>;

		fn visit_for(
			&mut self,
			initializer: Option<&Statement>,
			condition: Option<&Expression>,
			increment: Option<&Statement>,
			body: &Statement,
		) -> Result<T, Break>;

		fn visit_function_declaration(
			&mut self,
			name: &Token,
			parameters: &Box<[Token]>,
			body: Rc<Statement>,
		) -> Result<T, Break>;

		fn visit_if(
			&mut self,
			condition: &Expression,
			then_branch: &Statement,
			else_branch: Option<&Statement>,
		) -> Result<T, Break>;

		fn visit_print(&mut self, expression: &Expression) -> Result<T, Break>;

		fn visit_return(&mut self, expression: Option<&Expression>) -> Result<T, Break>;

		fn visit_variable_declaration(
			&mut self,
			name: &Token,
			initializer: Option<&Expression>,
		) -> Result<T, Break>;

		fn visit_while(&mut self, condition: &Expression, body: &Statement) -> Result<T, Break>;

		fn visit_expression(&mut self, expression: &Expression) -> Result<T, RuntimeError>;

		fn visit_assignment(&mut self, name: &Token, value: &Expression)
		                    -> Result<T, RuntimeError>;

		fn visit_binary(
			&mut self,
			left: &Expression,
			operator: &Token,
			right: &Expression,
		) -> Result<T, RuntimeError>;

		fn visit_call(
			&mut self,
			callee: &Expression,
			location: &Location,
			arguments: &Box<[Expression]>,
		) -> Result<T, RuntimeError>;

		fn visit_literal(&mut self, literal: &Token) -> Result<T, RuntimeError>;

		fn visit_grouping(&mut self, expression: &Expression) -> Result<T, RuntimeError>;

		fn visit_unary(&mut self, operator: &Token, right: &Expression) -> Result<T, RuntimeError>;

		fn visit_variable(&mut self, name: &Token) -> Result<T, RuntimeError>;
	}
}

/////////////////////////////////////////////////////////////////////////////
// Type implementations
/////////////////////////////////////////////////////////////////////////////

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
		self.visit_statement(statement).map_err(|error| match error {
			Break::Error(error) => error,
			Break::Return(_) => unreachable!("If you've landed here, the parser allowed a return statement outside of a function")
		})
	}
}

impl Scope {
	fn new() -> Self {
		Scope { parent: None, values: HashMap::new() }
	}

	fn with_parent(parent: Rc<RefCell<Scope>>) -> Self {
		Scope { parent: Some(parent), values: HashMap::new() }
	}

	fn get(scope: &Rc<RefCell<Scope>>, name: &Token) -> Result<Value> {
		if let Some(value) = scope.borrow().values.get(&name.text) {
			if let Some(value) = value {
				return Ok(value.clone());
			}
			return Err(RuntimeError::UninitializedVariable(Box::new(name.clone())));
		}

		if let Some(ref parent) = scope.borrow().parent {
			return Scope::get(parent, name);
		}

		Err(RuntimeError::UndefinedVariable(Box::new(name.clone())))
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

				let result = environment.visit_statement(body);

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

/////////////////////////////////////////////////////////////////////////////
// Trait implementations
/////////////////////////////////////////////////////////////////////////////

impl Visitor<Value> for Environment {
	fn visit_statement(&mut self, statement: &Statement) -> std::result::Result<Value, Break> {
		match statement {
			Statement::Block { statements, .. } => self.visit_block(statements),

			Statement::Expression(expression) => Ok(self.visit_expression(expression)?),

			Statement::For { initializer, condition, increment, body, .. } => self.visit_for(
				initializer.as_deref(),
				condition.as_deref(),
				increment.as_deref(),
				body,
			),

			Statement::FunctionDeclaration { name, parameters, body, .. } => {
				self.visit_function_declaration(name, parameters, body.clone())
			}

			Statement::If { condition, then_branch, else_branch, .. } => self.visit_if(
				condition,
				then_branch,
				else_branch.as_ref().map(|(_, it)| it.deref()),
			),

			Statement::Print { expression, .. } => self.visit_print(expression),

			Statement::Return { expression, .. } => self.visit_return(expression.as_deref()),

			Statement::VariableDeclaration { name, initializer, .. } => {
				self.visit_variable_declaration(name, initializer.as_deref())
			}

			Statement::While { condition, body, .. } => self.visit_while(condition, body),
		}
	}

	fn visit_block(&mut self, statements: &Box<[Statement]>) -> std::result::Result<Value, Break> {
		let previous = self.scope.clone();

		self.scope = Rc::new(RefCell::new(Scope::with_parent(previous.clone())));

		let result =
			statements.iter().try_fold(Value::Nil, |_, statement| self.visit_statement(statement));

		self.scope = previous.clone();

		result
	}

	fn visit_for(
		&mut self,
		initializer: Option<&Statement>,
		condition: Option<&Expression>,
		increment: Option<&Statement>,
		body: &Statement,
	) -> std::result::Result<Value, Break> {
		let previous = self.scope.clone();

		self.scope = Rc::new(RefCell::new(Scope::with_parent(previous.clone())));

		let result = initializer
			.as_ref()
			.map(|initializer| self.visit_statement(initializer))
			.transpose()
			.and_then(|_| {
				while condition
					.as_ref()
					.map(|condition| self.visit_expression(condition))
					.transpose()?
					.unwrap_or_else(|| Value::Bool(true))
					.is_truthy()
				{
					self.visit_statement(body)?;

					if let Some(increment) = increment {
						self.visit_statement(&increment)?;
					}
				}

				Ok(())
			});

		self.scope = previous.clone();

		result.map(|_| Value::Nil)
	}

	fn visit_function_declaration(
		&mut self,
		name: &Token,
		parameters: &Box<[Token]>,
		body: Rc<Statement>,
	) -> std::result::Result<Value, Break> {
		let callable = Callable { arguments: parameters.clone(), body: Body::Statement(body) };

		Scope::define(&mut self.scope, &name.text, Some(Value::Callable(callable)));

		Ok(Value::Nil)
	}

	fn visit_if(
		&mut self,
		condition: &Expression,
		then_branch: &Statement,
		else_branch: Option<&Statement>,
	) -> std::result::Result<Value, Break> {
		let condition = self.visit_expression(condition)?;

		if condition.is_truthy() {
			self.visit_statement(then_branch)?;
		} else if let Some(else_branch) = else_branch {
			self.visit_statement(else_branch)?;
		}

		Ok(Value::Nil)
	}

	fn visit_print(&mut self, expression: &Expression) -> std::result::Result<Value, Break> {
		println!("{}", self.visit_expression(expression)?.to_string());
		Ok(Value::Nil)
	}

	fn visit_return(
		&mut self,
		expression: Option<&Expression>,
	) -> std::result::Result<Value, Break> {
		let value = expression
			.as_ref()
			.map(|expression| self.visit_expression(expression))
			.transpose()?
			.unwrap_or_else(|| Value::Nil);

		Err(Break::Return(value))
	}

	fn visit_variable_declaration(
		&mut self,
		name: &Token,
		initializer: Option<&Expression>,
	) -> std::result::Result<Value, Break> {
		let value = match initializer {
			None => None,
			Some(ref expression) => Some(self.visit_expression(expression)?),
		};

		Scope::define(&mut self.scope, &name.text, value);

		Ok(Value::Nil)
	}

	fn visit_while(
		&mut self,
		condition: &Expression,
		body: &Statement,
	) -> std::result::Result<Value, Break> {
		while self.visit_expression(condition)?.is_truthy() {
			self.visit_statement(body)?;
		}

		Ok(Value::Nil)
	}

	fn visit_expression(&mut self, expression: &Expression) -> Result<Value> {
		match expression {
			Expression::Assignment { name, value } => self.visit_assignment(name, value),

			Expression::Binary { left, operator, right } => {
				self.visit_binary(left, operator, right)
			}

			Expression::Call { callee, arguments_start_location, arguments, .. } => {
				self.visit_call(callee, arguments_start_location, arguments)
			}

			Expression::Literal(literal) => self.visit_literal(literal),

			Expression::Grouping(expression) => self.visit_grouping(expression),

			Expression::Unary { operator, right } => self.visit_unary(operator, right),

			Expression::Variable(name) => self.visit_variable(name),
		}
	}

	fn visit_assignment(&mut self, name: &Token, value: &Expression) -> Result<Value> {
		let value = self.visit_expression(value)?;

		Scope::assign(&mut self.scope, name, value.clone())?;

		Ok(value)
	}

	fn visit_binary(
		&mut self,
		left: &Expression,
		operator: &Token,
		right: &Expression,
	) -> Result<Value> {
		if matches!(operator.kind, TokenKind::And | TokenKind::Or) {
			let left_value = self.visit_expression(left)?;
			if operator.kind == TokenKind::Or && left_value.is_truthy() {
				Ok(left_value)
			} else if !left_value.is_truthy() {
				Ok(left_value)
			} else {
				self.visit_expression(right)
			}
		} else {
			let left_value = self.visit_expression(left)?;
			let right_value = self.visit_expression(right)?;

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

	fn visit_call(
		&mut self,
		callee: &Expression,
		location: &Location,
		arguments: &Box<[Expression]>,
	) -> Result<Value> {
		let callee = self.visit_expression(callee)?;

		let arguments = arguments
			.iter()
			.map(|argument| self.visit_expression(argument))
			.collect::<Result<Vec<Value>>>()?;

		if let Value::Callable(callable) = callee {
			if arguments.len() == callable.arity() {
				Ok(callable.call(self, arguments)?)
			} else {
				Err(RuntimeError::UnexpectedNumberOfArguments {
					location: location.clone(),
					expected: callable.arity(),
					actual: arguments.len(),
				})
			}
		} else {
			Err(RuntimeError::NotCallable(location.clone()))
		}
	}

	fn visit_literal(&mut self, literal: &Token) -> Result<Value> {
		match literal.kind {
			TokenKind::Nil => Ok(Value::Nil),
			TokenKind::True => Ok(Value::Bool(true)),
			TokenKind::False => Ok(Value::Bool(false)),
			TokenKind::Number => Ok(literal.value.clone().unwrap()),
			TokenKind::String => Ok(literal.value.clone().unwrap()),
			it => unreachable!("Unknown token {} evaluating literal!", it),
		}
	}

	fn visit_grouping(&mut self, expression: &Expression) -> Result<Value> {
		self.visit_expression(expression)
	}

	fn visit_unary(&mut self, operator: &Token, right: &Expression) -> Result<Value> {
		let right_value = self.visit_expression(right)?;

		match operator.kind {
			TokenKind::Bang => Ok(Value::Bool(!right_value.is_truthy())),

			TokenKind::Minus => {
				let right_number = right_value.as_number(right)?;

				Ok(Value::Number(-right_number))
			}

			it => unreachable!("Unknown operator {} evaluating unary expression!", it),
		}
	}

	fn visit_variable(&mut self, name: &Token) -> Result<Value> {
		Scope::get(&mut self.scope, name)
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

impl Display for TypeKind {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		f.write_str(self.as_str())
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
