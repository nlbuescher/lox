use std::cell::RefCell;
use std::io::{stdout, Write};
use std::ops::Deref;
use std::rc::Rc;

use crate::interpret::error::{Break, RuntimeError};
use crate::interpret::function::{Function, NativeFunction};
use crate::interpret::visit::Visitor;
use crate::interpret::{Class, Scope};
use crate::location::Locatable;
use crate::parse::{Expression, Statement};
use crate::tokenize::{Token, TokenKind};
use crate::value::Value;

pub struct Environment {
	output: Rc<RefCell<dyn Write>>,
	pub(super) scope: RefCell<Scope>,
}

impl Default for Environment {
	fn default() -> Self {
		Environment::with_output(Rc::new(RefCell::new(stdout())))
	}
}

impl Environment {
	pub fn with_output(output: Rc<RefCell<dyn Write>>) -> Self {
		let scope = Scope::new();

		let environment = Environment { output, scope: RefCell::new(scope.clone()) };

		environment.scope.borrow_mut().define(
			"clock",
			Some(Value::Function(Rc::new(NativeFunction::new(|_, _| {
				use std::time::{SystemTime, UNIX_EPOCH};

				let now = SystemTime::now();
				let duration = now.duration_since(UNIX_EPOCH).unwrap();
				let seconds = duration.as_secs() as f64;
				let nanos = duration.subsec_nanos() as f64 / 1_000_000_000.0;

				Ok(Value::Number(seconds + nanos))
			})))),
		);

		environment
	}

	pub fn run_in_scope<T, E>(
		&mut self,
		scope: Scope,
		block: impl FnOnce(&mut Environment) -> Result<T, E>,
	) -> Result<T, E> {
		let previous = self.scope.replace(scope);
		let result = block(self);
		self.scope.replace(previous);
		result
	}

	pub fn execute(&mut self, statement: &Statement) -> Result<Value, RuntimeError> {
		self.visit_statement(statement).map_err(|error| match error {
			Break::Error(error) => error,
			Break::Return(_) => unreachable!("If you've landed here, the parser allowed a return statement outside of a function")
		})
	}
}

impl Visitor<Value> for Environment {
	fn visit_statement(&mut self, statement: &Statement) -> Result<Value, Break> {
		match statement {
			Statement::Block { statements, .. } => self.visit_block(statements),

			Statement::ClassDeclaration { name, methods, .. } => {
				self.visit_class_declaration(name, methods)
			}

			Statement::Expression(expression) => Ok(self.visit_expression(expression)?),

			Statement::For { initializer, condition, increment, body, .. } => self.visit_for(
				initializer.as_deref(),
				condition.as_deref(),
				increment.as_deref(),
				body,
			),

			Statement::FunctionDeclaration { name, parameters, body, .. } => {
				self.visit_function_declaration(name, parameters, body)
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

	fn visit_block(&mut self, statements: &Vec<Statement>) -> Result<Value, Break> {
		let scope = Scope::with_parent(self.scope.borrow().deref());

		self.run_in_scope(scope, |environment| {
			statements
				.iter()
				.try_fold(Value::Nil, |_, statement| environment.visit_statement(statement))
		})
	}

	fn visit_class_declaration(
		&mut self,
		name: &Token,
		_methods: &Vec<Statement>,
	) -> Result<Value, Break> {
		self.scope
			.borrow_mut()
			.define(&name.text, Some(Value::Class(Class::new(name.text.clone()))));

		Ok(Value::Nil)
	}

	fn visit_for(
		&mut self,
		initializer: Option<&Statement>,
		condition: Option<&Expression>,
		increment: Option<&Statement>,
		body: &Statement,
	) -> Result<Value, Break> {
		let scope = Scope::with_parent(self.scope.borrow().deref());

		self.run_in_scope(scope, |environment| {
			if let Some(initializer) = initializer {
				environment.visit_statement(initializer)?;
			}

			while condition
				.as_ref()
				.map(|condition| environment.visit_expression(condition))
				.transpose()?
				.unwrap_or_else(|| Value::Bool(true))
				.is_truthy()
			{
				environment.visit_statement(body)?;

				if let Some(increment) = increment {
					environment.visit_statement(increment)?;
				}
			}

			Ok(Value::Nil)
		})
	}

	fn visit_function_declaration(
		&mut self,
		name: &Token,
		parameters: &Vec<Token>,
		body: &Rc<Statement>,
	) -> Result<Value, Break> {
		let function = self.visit_function(parameters, body)?;

		self.scope.borrow_mut().define(&name.text, Some(function));

		Ok(Value::Nil)
	}

	fn visit_if(
		&mut self,
		condition: &Expression,
		then_branch: &Statement,
		else_branch: Option<&Statement>,
	) -> Result<Value, Break> {
		let condition = self.visit_expression(condition)?;

		if condition.is_truthy() {
			self.visit_statement(then_branch)?;
		} else if let Some(else_branch) = else_branch {
			self.visit_statement(else_branch)?;
		}

		Ok(Value::Nil)
	}

	fn visit_print(&mut self, expression: &Expression) -> Result<Value, Break> {
		let value = self.visit_expression(expression)?;
		writeln!(self, "{}", value.to_string()).map_err(RuntimeError::from)?;
		Ok(Value::Nil)
	}

	fn visit_return(&mut self, expression: Option<&Expression>) -> Result<Value, Break> {
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
	) -> Result<Value, Break> {
		let value = match initializer {
			None => None,
			Some(ref expression) => Some(self.visit_expression(expression)?),
		};

		self.scope.borrow_mut().define(&name.text, value);

		Ok(Value::Nil)
	}

	fn visit_while(&mut self, condition: &Expression, body: &Statement) -> Result<Value, Break> {
		while self.visit_expression(condition)?.is_truthy() {
			self.visit_statement(body)?;
		}

		Ok(Value::Nil)
	}

	fn visit_expression(&mut self, expression: &Expression) -> Result<Value, RuntimeError> {
		match expression {
			Expression::Assignment { name, expression } => self.visit_assignment(name, expression),

			Expression::Binary { left, operator, right } => {
				self.visit_binary(left, operator, right)
			}

			Expression::Call { callee, open_paren, arguments, .. } => {
				self.visit_call(callee, open_paren, arguments)
			}

			Expression::Function { parameters, body, .. } => self.visit_function(parameters, body),

			Expression::Literal(literal) => self.visit_literal(literal),

			Expression::Grouping(expression) => self.visit_grouping(expression),

			Expression::Unary { operator, expression } => self.visit_unary(operator, expression),

			Expression::Variable(name) => self.visit_variable(name),
		}
	}

	fn visit_assignment(
		&mut self,
		name: &Token,
		expression: &Expression,
	) -> Result<Value, RuntimeError> {
		let value = self.visit_expression(expression)?;

		self.scope.borrow_mut().assign(name, value.clone())?;

		Ok(value)
	}

	fn visit_binary(
		&mut self,
		left: &Expression,
		operator: &Token,
		right: &Expression,
	) -> Result<Value, RuntimeError> {
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

						Ok(Value::String(format!("{left_string}{right_string}")))
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
		open_paren: &Token,
		arguments: &Vec<Expression>,
	) -> Result<Value, RuntimeError> {
		let callee = self.visit_expression(callee)?;

		let arguments = arguments
			.iter()
			.map(|argument| self.visit_expression(argument))
			.collect::<Result<Vec<Value>, RuntimeError>>()?;

		let callable = if let Value::Function(function) = callee {
			function
		} else if let Value::Class(class) = callee {
			Rc::new(class)
		} else {
			return Err(RuntimeError::NotCallable(open_paren.location().clone()));
		};

		if arguments.len() == callable.arity() {
			Ok(callable.call(self, &arguments)?)
		} else {
			Err(RuntimeError::UnexpectedNumberOfArguments {
				location: open_paren.location().clone(),
				expected: callable.arity(),
				actual: arguments.len(),
			})
		}
	}

	fn visit_function(
		&mut self,
		parameters: &Vec<Token>,
		body: &Rc<Statement>,
	) -> Result<Value, RuntimeError> {
		Ok(Value::Function(Rc::new(Function::new(
			None,
			self.scope.borrow().clone(),
			parameters.clone(),
			body.clone(),
		))))
	}

	fn visit_grouping(&mut self, expression: &Expression) -> Result<Value, RuntimeError> {
		self.visit_expression(expression)
	}

	fn visit_literal(&mut self, literal: &Token) -> Result<Value, RuntimeError> {
		match literal.kind {
			TokenKind::Nil => Ok(Value::Nil),
			TokenKind::True => Ok(Value::Bool(true)),
			TokenKind::False => Ok(Value::Bool(false)),
			TokenKind::Number => Ok(literal.value.clone().unwrap()),
			TokenKind::String => Ok(literal.value.clone().unwrap()),
			it => unreachable!("Unknown token {} evaluating literal!", it),
		}
	}

	fn visit_unary(&mut self, operator: &Token, right: &Expression) -> Result<Value, RuntimeError> {
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

	fn visit_variable(&mut self, name: &Token) -> Result<Value, RuntimeError> {
		self.scope.borrow().get(name)
	}
}

impl Write for Environment {
	fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
		self.output.borrow_mut().write(buf)
	}

	fn flush(&mut self) -> std::io::Result<()> {
		self.output.borrow_mut().flush()
	}
}
