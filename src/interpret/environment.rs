use std::cell::RefCell;
use std::collections::HashMap;
use std::io::{stdout, Write};
use std::ops::Deref;
use std::rc::Rc;

use crate::error::Error;
use crate::interpret::error::Break;
use crate::interpret::function::{Function, NativeFunction};
use crate::interpret::object::{Get, Set};
use crate::interpret::visit::Visitor;
use crate::interpret::{Class, Scope, TypeKind};
use crate::location::Locate;
use crate::parse::{
	AssignmentExpression, BinaryExpression, BlockStatement, CallExpression,
	ClassDeclarationStatement, Expression, ExpressionStatement, ForStatement,
	FunctionDeclarationStatement, FunctionExpression, GetExpression, GroupingExpression,
	IfStatement, LiteralExpression, PrintStatement, ReturnStatement, SetExpression, Statement,
	SuperExpression, ThisExpression, UnaryExpression, VariableDeclarationStatement,
	VariableExpression, WhileStatement,
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
			Some(Value::Object(Rc::new(RefCell::new(NativeFunction::new(0, |_, _| {
				use std::time::{SystemTime, UNIX_EPOCH};

				let now = SystemTime::now();
				let duration = now.duration_since(UNIX_EPOCH).unwrap();
				let seconds = duration.as_secs() as f64;
				let nanos = duration.subsec_nanos() as f64 / 1_000_000_000.0;

				Ok(Value::Number(seconds + nanos))
			}))))),
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

	pub fn execute(&mut self, statement: &Statement) -> Result<Value, Error> {
		self.visit_statement(statement).map_err(|error| match error {
			Break::Error(error) => error,
			Break::Return(_) => unreachable!("If you've landed here, the parser allowed a return statement outside of a function")
		})
	}
}

impl Visitor<Value> for Environment {
	fn visit_statement(&mut self, statement: &Statement) -> Result<Value, Break> {
		match statement {
			Statement::Block(BlockStatement { statements, .. }) => self.visit_block(statements),

			Statement::ClassDeclaration(ClassDeclarationStatement {
				                            name,
				                            super_class,
				                            methods,
				                            ..
			                            }) => self.visit_class_declaration(name, super_class, methods),

			Statement::Expression(ExpressionStatement { expression }) => {
				Ok(self.visit_expression(expression)?)
			}

			Statement::For(ForStatement { initializer, condition, increment, body, .. }) => self
				.visit_for(
					initializer.as_deref(),
					condition.as_deref(),
					increment.as_deref(),
					body,
				),

			Statement::FunctionDeclaration(FunctionDeclarationStatement {
				                               name,
				                               parameters,
				                               body,
				                               ..
			                               }) => self.visit_function_declaration(name, parameters, body),

			Statement::If(IfStatement { condition, then_branch, else_branch, .. }) => self
				.visit_if(condition, then_branch, else_branch.as_ref().map(|(_, it)| it.deref())),

			Statement::Print(PrintStatement { expression, .. }) => self.visit_print(expression),

			Statement::Return(ReturnStatement { expression, .. }) => {
				self.visit_return(expression.as_deref())
			}

			Statement::VariableDeclaration(VariableDeclarationStatement {
				                               name,
				                               initializer,
				                               ..
			                               }) => self.visit_variable_declaration(name, initializer.as_deref()),

			Statement::While(WhileStatement { condition, body, .. }) => {
				self.visit_while(condition, body)
			}
		}
	}

	fn visit_block(&mut self, statements: &[Statement]) -> Result<Value, Break> {
		let scope = Scope::with_parent(self.scope.borrow().clone());

		self.run_in_scope(scope, |environment| {
			statements
				.iter()
				.try_fold(Value::Nil, |_, statement| environment.visit_statement(statement))
		})
	}

	fn visit_class_declaration(
		&mut self,
		name: &Token,
		super_class: &Option<VariableExpression>,
		methods: &[FunctionDeclarationStatement],
	) -> Result<Value, Break> {
		let super_class = super_class
			.as_ref()
			.map(|expression| self.visit_variable(&expression.0)?.as_class(expression.locate()))
			.transpose()?;

		let mut class_methods = HashMap::new();
		let mut instance_methods = HashMap::new();

		{
			let mut scope = Scope::with_parent(self.scope.borrow().clone());

			if let Some(super_class) = &super_class {
				scope.define("super", Some(Value::Object(super_class.clone())));
			}

			for method in methods {
				let FunctionDeclarationStatement {
					keyword,
					name: Token { text: name, .. },
					parameters,
					body,
				} = method;

				let function = Rc::new(RefCell::new(Function::new(
					Some(name.clone()),
					name == "init",
					scope.clone(),
					parameters.clone(),
					body.clone(),
				)));

				if matches!(&keyword, Some(keyword) if keyword.kind == TokenKind::Class) {
					class_methods.insert(name.clone(), function);
				} else {
					instance_methods.insert(name.clone(), function);
				}
			}
		}

		self.scope.borrow_mut().define(
			&name.text,
			Some(Value::Object(Rc::new(RefCell::new(Class::new(
				name.text.clone(),
				super_class,
				class_methods,
				instance_methods,
			))))),
		);

		Ok(Value::Nil)
	}

	fn visit_for(
		&mut self,
		initializer: Option<&Statement>,
		condition: Option<&Expression>,
		increment: Option<&ExpressionStatement>,
		body: &BlockStatement,
	) -> Result<Value, Break> {
		let scope = Scope::with_parent(self.scope.borrow().clone());

		self.run_in_scope(scope, |environment| {
			if let Some(initializer) = initializer {
				environment.visit_statement(initializer)?;
			}

			while condition
				.map(|condition| environment.visit_expression(condition))
				.transpose()?
				.unwrap_or_else(|| Value::Bool(true))
				.is_truthy()
			{
				environment.visit_block(&body.statements)?;

				if let Some(increment) = increment {
					environment.visit_expression(&increment.expression)?;
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
		then_branch: &BlockStatement,
		else_branch: Option<&Statement>,
	) -> Result<Value, Break> {
		let condition = self.visit_expression(condition)?;

		if condition.is_truthy() {
			self.visit_block(&then_branch.statements)?;
		}
		else if let Some(else_branch) = else_branch {
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
			self.visit_block(&body.statements)?;
		}

		Ok(Value::Nil)
	}

	fn visit_expression(&mut self, expression: &Expression) -> Result<Value, Error> {
		match expression {
			Expression::Assignment(AssignmentExpression { name, expression }) => {
				self.visit_assignment(name, expression)
			}

			Expression::Binary(BinaryExpression { left, operator, right }) => {
				self.visit_binary(left, operator, right)
			}

			Expression::Call(CallExpression { callee, open_paren, arguments, .. }) => {
				self.visit_call(callee, open_paren, arguments)
			}

			Expression::Function(FunctionExpression { parameters, body, .. }) => {
				self.visit_function(parameters, body)
			}

			Expression::Get(GetExpression { object, property }) => self.visit_get(object, property),

			Expression::Grouping(GroupingExpression(expression)) => self.visit_grouping(expression),

			Expression::Literal(LiteralExpression(literal)) => self.visit_literal(literal),

			Expression::Set(SetExpression { object, property, value }) => {
				self.visit_set(object, property, value)
			}

			Expression::Super(SuperExpression { keyword, method }) => {
				self.visit_super(keyword, method)
			}

			Expression::This(ThisExpression(keyword)) => self.visit_this(keyword),

			Expression::Unary(UnaryExpression { operator, expression }) => {
				self.visit_unary(operator, expression)
			}

			Expression::Variable(VariableExpression(name)) => self.visit_variable(name),
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
			}
			else {
				self.visit_expression(right)
			}
		}
		else {
			let left_value = self.visit_expression(left)?;
			let right_value = self.visit_expression(right)?;

			match operator.kind {
				TokenKind::Greater => {
					let left_number = left_value.as_number(left.locate())?;
					let right_number = right_value.as_number(right.locate())?;

					Ok(Value::Bool(left_number > right_number))
				}

				TokenKind::GreaterEqual => {
					let left_number = left_value.as_number(left.locate())?;
					let right_number = right_value.as_number(right.locate())?;

					Ok(Value::Bool(left_number >= right_number))
				}

				TokenKind::Less => {
					let left_number = left_value.as_number(left.locate())?;
					let right_number = right_value.as_number(right.locate())?;

					Ok(Value::Bool(left_number < right_number))
				}

				TokenKind::LessEqual => {
					let left_number = left_value.as_number(left.locate())?;
					let right_number = right_value.as_number(right.locate())?;

					Ok(Value::Bool(left_number <= right_number))
				}

				TokenKind::BangEqual => Ok(Value::Bool(left_value != right_value)),

				TokenKind::EqualEqual => Ok(Value::Bool(left_value == right_value)),

				TokenKind::Minus => {
					let left_number = left_value.as_number(left.locate())?;
					let right_number = right_value.as_number(right.locate())?;

					Ok(Value::Number(left_number - right_number))
				}

				TokenKind::Plus => match left_value.as_number(left.locate()) {
					Ok(left_number) => {
						let right_number = right_value.as_number(right.locate())?;

						Ok(Value::Number(left_number + right_number))
					}
					Err(_) => {
						let left_string = left_value.as_string(left.locate())?;
						let right_string = right_value.as_string(right.locate())?;

						Ok(Value::String(format!("{left_string}{right_string}")))
					}
				},

				TokenKind::Slash => {
					let left_number = left_value.as_number(left.locate())?;
					let right_number = right_value.as_number(right.locate())?;

					Ok(Value::Number(left_number / right_number))
				}

				TokenKind::Star => {
					let left_number = left_value.as_number(left.locate())?;
					let right_number = right_value.as_number(right.locate())?;

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

		if let Value::Object(object) = callee {
			if let Some(callable) = object.borrow().as_callable() {
				if arguments.len() != callable.arity() {
					return Err(Error::unexpected_number_of_arguments(
						open_paren.locate(),
						callable.arity(),
						arguments.len(),
					));
				}

				return callable.call(self, &arguments);
			}
		}

		Err(Error::not_callable(open_paren.locate()))
	}

	fn visit_function(
		&mut self,
		parameters: &[Token],
		body: &Rc<BlockStatement>,
	) -> Result<Value, Error> {
		Ok(Value::Object(Rc::new(RefCell::new(Function::new(
			None,
			false,
			self.scope.borrow().clone(),
			Vec::from(parameters),
			body.clone(),
		)))))
	}

	fn visit_get(&mut self, object: &Expression, property: &Token) -> Result<Value, Error> {
		let object = self.visit_expression(object)?;

		if let Value::Object(object) = &object {
			if let Some(instance) = object.borrow().as_instance() {
				return instance.get(property);
			}

			if let Some(class) = object.borrow().as_class() {
				return class.get(property);
			}
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

		if let Value::Object(object) = object {
			if let Some(instance) = object.borrow_mut().as_instance_mut() {
				let value = self.visit_expression(value)?;
				instance.set(property, value.clone());
				return Ok(value);
			}
		}

		Err(Error::not_instance(property.locate()))
	}

	fn visit_super(&mut self, keyword: &Token, method: &Token) -> Result<Value, Error> {
		let super_class = self.scope.borrow().get(keyword)?.as_class(keyword.locate())?;

		let instance = self
			.visit_this(&Token::with_text(keyword.locate().clone(), TokenKind::This, "this"))?
			.as_instance(keyword.locate())?;

		let function = super_class.borrow().find_method(&method.text);
		if let Some(function) = function {
			Ok(Value::Object(function.borrow().bind(instance.borrow().clone())))
		} else {
			Err(Error::undefined_value(method))
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
				let right_number = right_value.as_number(right.locate())?;

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
