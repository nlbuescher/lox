use std::rc::Rc;

use crate::interpret::error::Break;
use crate::parse::{
	BlockStatement, Expression, ExpressionStatement, FunctionDeclarationStatement, Statement,
	VariableExpression,
};
use crate::tokenize::Token;
use crate::Error;

pub trait Visitor<T> {
	fn visit_statement(&mut self, statement: &Statement) -> Result<T, Break>;

	fn visit_block(&mut self, statements: &[Statement]) -> Result<T, Break>;

	fn visit_class_declaration(
		&mut self,
		name: &Token,
		super_class: &Option<VariableExpression>,
		methods: &[FunctionDeclarationStatement],
	) -> Result<T, Break>;

	fn visit_for(
		&mut self,
		initializer: Option<&Statement>,
		condition: Option<&Expression>,
		increment: Option<&ExpressionStatement>,
		body: &BlockStatement,
	) -> Result<T, Break>;

	fn visit_function_declaration(
		&mut self,
		name: &Token,
		parameters: &[Token],
		body: &Rc<BlockStatement>,
	) -> Result<T, Break>;

	fn visit_if(
		&mut self,
		condition: &Expression,
		then_branch: &BlockStatement,
		else_branch: Option<&Statement>,
	) -> Result<T, Break>;

	fn visit_print(&mut self, expression: &Expression) -> Result<T, Break>;

	fn visit_return(&mut self, expression: Option<&Expression>) -> Result<T, Break>;

	fn visit_variable_declaration(
		&mut self,
		name: &Token,
		initializer: Option<&Expression>,
	) -> Result<T, Break>;

	fn visit_while(&mut self, condition: &Expression, body: &BlockStatement) -> Result<T, Break>;

	fn visit_expression(&mut self, expression: &Expression) -> Result<T, Error>;

	fn visit_assignment(&mut self, name: &Token, expression: &Expression) -> Result<T, Error>;

	fn visit_binary(
		&mut self,
		left: &Expression,
		operator: &Token,
		right: &Expression,
	) -> Result<T, Error>;

	fn visit_call(
		&mut self,
		callee: &Expression,
		open_paren: &Token,
		arguments: &[Expression],
	) -> Result<T, Error>;

	fn visit_function(
		&mut self,
		parameters: &[Token],
		body: &Rc<BlockStatement>,
	) -> Result<T, Error>;

	fn visit_get(&mut self, object: &Expression, property: &Token) -> Result<T, Error>;

	fn visit_grouping(&mut self, expression: &Expression) -> Result<T, Error>;

	fn visit_literal(&mut self, literal: &Token) -> Result<T, Error>;

	fn visit_set(
		&mut self,
		object: &Expression,
		property: &Token,
		value: &Expression,
	) -> Result<T, Error>;

	fn visit_super(&mut self, keyword: &Token, method: &Token) -> Result<T, Error>;

	fn visit_this(&mut self, keyword: &Token) -> Result<T, Error>;

	fn visit_unary(&mut self, operator: &Token, right: &Expression) -> Result<T, Error>;

	fn visit_variable(&mut self, name: &Token) -> Result<T, Error>;
}
