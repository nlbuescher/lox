use std::rc::Rc;

use crate::interpret::error::{Break, RuntimeError};
use crate::location::Location;
use crate::parse::{Expression, Statement};
use crate::tokenize::Token;

pub trait Visitor<T> {
	fn visit_statement(&mut self, statement: &Statement) -> Result<T, Break>;

	fn visit_block(&mut self, statements: &Vec<Statement>) -> Result<T, Break>;

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
		parameters: &Vec<Token>,
		body: &Rc<Statement>,
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

	fn visit_assignment(&mut self, name: &Token, value: &Expression) -> Result<T, RuntimeError>;

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
		arguments: &Vec<Expression>,
	) -> Result<T, RuntimeError>;

	fn visit_function(
		&mut self,
		parameters: &Vec<Token>,
		body: &Rc<Statement>,
	) -> Result<T, RuntimeError>;

	fn visit_grouping(&mut self, expression: &Expression) -> Result<T, RuntimeError>;

	fn visit_literal(&mut self, literal: &Token) -> Result<T, RuntimeError>;

	fn visit_unary(&mut self, operator: &Token, right: &Expression) -> Result<T, RuntimeError>;

	fn visit_variable(&mut self, name: &Token) -> Result<T, RuntimeError>;
}
