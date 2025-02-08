use std::cell::RefCell;
use std::collections::HashMap;
use std::io::{stdout, Write};
use std::ops::Deref;
use std::rc::Rc;

use crate::error::Error;
use crate::interpret::error::Break;
use crate::interpret::function::{Function, NativeFunction};
use crate::interpret::visit::Visitor;
use crate::interpret::{Class, Instance, Scope, TypeKind};
use crate::location::Locate;
use crate::parse::{
	BlockStatement, Expression, ExpressionStatement, FunctionDeclarationStatement, Statement,
	StatementKind,
};
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

	pub fn execute(&mut self, statement: &dyn Statement) -> Result<Value, Error> {
		self.visit_statement(statement).map_err(|error| match error {
			Break::Error(error) => error,
			Break::Return(_) => unreachable!("If you've landed here, the parser allowed a return statement outside of a function")
		})
	}
}

impl Visitor<Value> for Environment {
	fn visit_statement(&mut self, statement: &dyn Statement) -> Result<Value, Break> {
		match statement.statement_kind() {
			StatementKind::Block => {
				let block = statement.as_block().unwrap();
				self.visit_block(&block.statements)
			}

			StatementKind::ClassDeclaration => {
				let class = statement.as_class_declaration().unwrap();
				self.visit_class_declaration(&class.name, &class.methods)
			}

			StatementKind::Expression => {
				let expression = statement.as_expression().unwrap();
				Ok(self.visit_expression(&expression.expression)?)
			}

			StatementKind::For => {
				let for_ = statement.as_for().unwrap();
				self.visit_for(
					for_.initializer.as_deref(),
					for_.condition.as_deref(),
					for_.increment.as_deref(),
					&for_.body,
				)
			}

			StatementKind::FunctionDeclaration => {
				let function = statement.as_function_declaration().unwrap();
				self.visit_function_declaration(
					&function.name,
					&function.parameters,
					&function.body,
				)
			}

			StatementKind::If => {
				let if_ = statement.as_if().unwrap();
				let else_ = if_.else_branch.as_ref().map(|(_, else_branch)| else_branch.deref());
				self.visit_if(&if_.condition, if_.then_branch.deref(), else_)
			}

			StatementKind::Print => {
				let print = statement.as_print().unwrap();
				self.visit_print(&print.expression)
			}

			StatementKind::Return => {
				let return_ = statement.as_return().unwrap();
				self.visit_return(return_.expression.as_deref())
			}

			StatementKind::VariableDeclaration => {
				let var = statement.as_variable_declaration().unwrap();
				self.visit_variable_declaration(&var.name, var.initializer.as_deref())
			}

			StatementKind::While => {
				let while_ = statement.as_while().unwrap();
				self.visit_while(&while_.condition, &while_.body)
			}
		}
	}

	fn visit_block(&mut self, statements: &[Box<dyn Statement>]) -> Result<Value, Break> {
		let scope = Scope::with_parent(self.scope.borrow().deref());

		self.run_in_scope(scope, |environment| {
			statements
				.iter()
				.try_fold(Value::Nil, |_, statement| environment.visit_statement(statement.deref()))
		})
	}

	fn visit_class_declaration(
		&mut self,
		name: &Token,
		methods: &[FunctionDeclarationStatement],
	) -> Result<Value, Break> {
		let methods = methods
			.iter()
			.map(|method| {
				let function = Function::new(
					Some(method.name.text.clone()),
					self.scope.borrow().clone(),
					method.parameters.clone(),
					method.body.clone(),
				);

				(method.name.text.clone(), Rc::new(function))
			})
			.collect::<HashMap<_, _>>();

		self.scope.borrow_mut().define(
			&name.text,
			Some(Value::Class(Rc::new(Class::new(name.text.clone(), methods)))),
		);

		Ok(Value::Nil)
	}

	fn visit_for(
		&mut self,
		initializer: Option<&dyn Statement>,
		condition: Option<&Expression>,
		increment: Option<&ExpressionStatement>,
		body: &BlockStatement,
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
		parameters: &[Token],
		body: &Rc<BlockStatement>,
	) -> Result<Value, Break> {
		let function = self.visit_function(parameters, body)?;

		self.scope.borrow_mut().define(&name.text, Some(function));

		Ok(Value::Nil)
	}

	fn visit_if(
		&mut self,
		condition: &Expression,
		then_branch: &dyn Statement,
		else_branch: Option<&dyn Statement>,
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
		writeln!(self, "{value}").map_err(Error::from)?;
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
			Some(expression) => Some(self.visit_expression(expression)?),
		};

		self.scope.borrow_mut().define(&name.text, value);

		Ok(Value::Nil)
	}

	fn visit_while(
		&mut self,
		condition: &Expression,
		body: &BlockStatement,
	) -> Result<Value, Break> {
		while self.visit_expression(condition)?.is_truthy() {
			self.visit_statement(body)?;
		}

		Ok(Value::Nil)
	}

	fn visit_expression(&mut self, expression: &Expression) -> Result<Value, Error> {
		match expression {
			Expression::Assignment { name, expression } => self.visit_assignment(name, expression),

			Expression::Binary { left, operator, right } => {
				self.visit_binary(left, operator, right)
			}

			Expression::Call { callee, open_paren, arguments, .. } => {
				self.visit_call(callee, open_paren, arguments)
			}

			Expression::Function { parameters, body, .. } => self.visit_function(parameters, body),

			Expression::Get { object, property } => self.visit_get(object, property),

			Expression::Grouping(expression) => self.visit_grouping(expression),

			Expression::Literal(literal) => self.visit_literal(literal),

			Expression::Set { object, property, value } => self.visit_set(object, property, value),

			Expression::This(keyword) => self.visit_this(keyword),

			Expression::Unary { operator, expression } => self.visit_unary(operator, expression),

			Expression::Variable(name) => self.visit_variable(name),
		}
	}

	fn visit_assignment(&mut self, name: &Token, expression: &Expression) -> Result<Value, Error> {
		let value = self.visit_expression(expression)?;

		self.scope.borrow_mut().assign(name, value.clone())?;

		Ok(value)
	}

	fn visit_binary(
		&mut self,
		left: &Expression,
		operator: &Token,
		right: &Expression,
	) -> Result<Value, Error> {
		if matches!(operator.kind, TokenKind::And | TokenKind::Or) {
			let left_value = self.visit_expression(left)?;
			if operator.kind == TokenKind::Or && left_value.is_truthy() || !left_value.is_truthy() {
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
		arguments: &[Expression],
	) -> Result<Value, Error> {
		let callee = self.visit_expression(callee)?;

		let arguments = arguments
			.iter()
			.map(|argument| self.visit_expression(argument))
			.collect::<Result<Vec<Value>, Error>>()?;

		let callable = if let Value::Function(function) = callee {
			function
		} else if let Value::Class(class) = callee {
			class
		} else {
			return Err(Error::not_callable(open_paren.locate()));
		};

		if arguments.len() == callable.arity() {
			Ok(callable.call(self, &arguments)?)
		} else {
			Err(Error::unexpected_number_of_arguments(
				open_paren.locate(),
				callable.arity(),
				arguments.len(),
			))
		}
	}

	fn visit_function(
		&mut self,
		parameters: &[Token],
		body: &Rc<BlockStatement>,
	) -> Result<Value, Error> {
		Ok(Value::Function(Rc::new(Function::new(
			None,
			self.scope.borrow().clone(),
			Vec::from(parameters),
			body.clone(),
		))))
	}

	fn visit_get(&mut self, object: &Expression, property: &Token) -> Result<Value, Error> {
		let object = self.visit_expression(object)?;

		if let Value::Instance(instance) = object {
			return Instance::get(instance, property);
		}

		Err(Error::type_error(
			property.locate(),
			TypeKind::Instance(String::from("any")),
			object.type_kind(),
		))
	}

	fn visit_grouping(&mut self, expression: &Expression) -> Result<Value, Error> {
		self.visit_expression(expression)
	}

	fn visit_literal(&mut self, literal: &Token) -> Result<Value, Error> {
		match literal.kind {
			TokenKind::Nil => Ok(Value::Nil),
			TokenKind::True => Ok(Value::Bool(true)),
			TokenKind::False => Ok(Value::Bool(false)),
			TokenKind::Number => Ok(literal.value.clone().unwrap()),
			TokenKind::String => Ok(literal.value.clone().unwrap()),
			it => unreachable!("Unknown token {} evaluating literal!", it),
		}
	}

	fn visit_set(
		&mut self,
		object: &Expression,
		property: &Token,
		value: &Expression,
	) -> Result<Value, Error> {
		let object = self.visit_expression(object)?;

		if let Value::Instance(instance) = object {
			let value = self.visit_expression(value)?;
			instance.borrow_mut().set(property, &value);
			Ok(value)
		} else {
			Err(Error::not_instance(property.locate()))
		}
	}

	fn visit_this(&mut self, keyword: &Token) -> Result<Value, Error> {
		self.scope.borrow().get(keyword)
	}

	fn visit_unary(&mut self, operator: &Token, right: &Expression) -> Result<Value, Error> {
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

	fn visit_variable(&mut self, name: &Token) -> Result<Value, Error> {
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
