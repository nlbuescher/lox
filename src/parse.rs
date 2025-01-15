use std::fmt::{Display, Formatter};
use std::iter::Peekable;
use std::string::String;

use crate::location::Location;
use crate::tokenize;
use crate::tokenize::{Token, TokenKind, Tokens};
use crate::value::Value;

pub type Result<T> = std::result::Result<T, ParseError>;

#[derive(Debug)]
pub struct ParseError {
	location: Location,
	expected: String,
	actual: String,
}

impl Display for ParseError {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		let ParseError { location, expected, actual } = self;
		write!(f, "{location} Expected {expected} but got {actual}")
	}
}

#[derive(Debug, Clone)]
pub enum Expression {
	Binary { left: Box<Expression>, operator: Token, right: Box<Expression> },
	Literal(Value, Token),
	Grouping(Box<Expression>),
	Unary { operator: Token, right: Box<Expression> },
	Variable(Token),
}

impl Expression {
	pub fn location(&self) -> &Location {
		match self {
			Expression::Binary { left, .. } => left.location(),
			Expression::Grouping(expression) => expression.location(),
			Expression::Literal(_, Token { location, .. }) => location,
			Expression::Unary { operator: Token { location, .. }, .. } => location,
			Expression::Variable(Token { location, .. }) => location,
		}
	}
}

impl Display for Expression {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		match self {
			Expression::Binary { left, operator: Token { text, .. }, right } => {
				write!(f, "({text} {left} {right})")
			}

			Expression::Literal(_, Token { text, .. }) => write!(f, "{text}"),

			Expression::Grouping(expression) => write!(f, "(group {expression})"),

			Expression::Unary { operator, right } => write!(f, "({operator} {right})"),

			Expression::Variable(Token { text, .. }) => write!(f, "(var {text})"),
		}
	}
}

#[derive(Debug, Clone)]
pub enum Statement {
	Expression { value: Expression },
	Print { value: Expression },
	VariableDeclaration { name: Token, value: Option<Expression> },
}

impl Display for Statement {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		match self {
			Statement::Expression { value } => write!(f, "(expr {value})"),
			Statement::Print { value } => write!(f, "(print {value})"),
			Statement::VariableDeclaration { name: Token { text, .. }, value } => match value {
				Some(value) => write!(f, "(vardecl {text} = {value})"),
				None => write!(f, "(vardecl {text})"),
			},
		}
	}
}

#[derive(Debug, Clone)]
pub struct Parser<'a> {
	tokens: Peekable<Tokens<'a>>,
	previous: Option<tokenize::Result>,
}

impl<'a> Parser<'a> {
	pub fn new(tokens: Tokens<'a>) -> Self {
		Parser { tokens: tokens.peekable(), previous: None }
	}

	pub fn synchronize(&mut self) {
		use TokenKind::*;

		self.advance();

		loop {
			if self.previous().kind == Semicolon {
				return;
			}

			if let Ok(Token {
				kind: Class | Fun | Var | For | If | While | Print | Return, ..
			}) = self.peek()
			{
				return;
			}

			self.advance();
		}
	}

	fn parse_declaration(&mut self) -> Result<Statement> {
		use TokenKind::*;

		if self.advance_if(Var) {
			self.parse_variable_declaration()
		}
		else {
			self.parse_statement()
		}
	}

	fn parse_variable_declaration(&mut self) -> Result<Statement> {
		use TokenKind::*;

		let name = self.expect(Identifier, "variable name")?;

		let value = if self.advance_if(Equal) { Some(self.parse_expression()?) } else { None };

		self.expect(Semicolon, "';' after variable declaration")?;

		Ok(Statement::VariableDeclaration { name: name.clone(), value })
	}

	fn parse_statement(&mut self) -> Result<Statement> {
		use TokenKind::*;

		if self.advance_if(Print) {
			self.parse_print_statement()
		}
		else {
			self.parse_expression_statement()
		}
	}

	fn parse_expression_statement(&mut self) -> Result<Statement> {
		use TokenKind::*;

		let value = self.parse_expression()?;
		self.expect(Semicolon, "';' after expression")?;
		Ok(Statement::Expression { value })
	}

	fn parse_print_statement(&mut self) -> Result<Statement> {
		use TokenKind::*;

		let value = self.parse_expression()?;
		self.expect(Semicolon, "';' after value")?;
		Ok(Statement::Print { value })
	}

	fn parse_expression(&mut self) -> Result<Expression> {
		self.parse_equality()
	}

	fn parse_equality(&mut self) -> Result<Expression> {
		use TokenKind::*;

		let mut expression = self.parse_comparison()?;

		while self.advance_if_any(&[BangEqual, EqualEqual]) {
			expression = Expression::Binary {
				left: Box::new(expression),
				operator: self.previous(),
				right: Box::new(self.parse_comparison()?),
			}
		}

		Ok(expression)
	}

	fn parse_comparison(&mut self) -> Result<Expression> {
		use TokenKind::*;

		let mut expression = self.parse_term()?;

		while self.advance_if_any(&[Greater, GreaterEqual, Less, LessEqual]) {
			expression = Expression::Binary {
				left: Box::new(expression),
				operator: self.previous(),
				right: Box::new(self.parse_term()?),
			}
		}

		Ok(expression)
	}

	fn parse_term(&mut self) -> Result<Expression> {
		use TokenKind::*;

		let mut expression = self.parse_factor()?;

		while self.advance_if_any(&[Minus, Plus]) {
			expression = Expression::Binary {
				left: Box::new(expression),
				operator: self.previous(),
				right: Box::new(self.parse_factor()?),
			}
		}

		Ok(expression)
	}

	fn parse_factor(&mut self) -> Result<Expression> {
		use TokenKind::*;

		let mut expression = self.parse_unary()?;

		while self.advance_if_any(&[Slash, Star]) {
			expression = Expression::Binary {
				left: Box::new(expression),
				operator: self.previous(),
				right: Box::new(self.parse_unary()?),
			}
		}

		Ok(expression)
	}

	fn parse_unary(&mut self) -> Result<Expression> {
		use TokenKind::*;

		if self.advance_if_any(&[Bang, Minus]) {
			Ok(Expression::Unary {
				operator: self.previous(),
				right: Box::new(self.parse_unary()?),
			})
		}
		else {
			self.parse_primary()
		}
	}

	fn parse_primary(&mut self) -> Result<Expression> {
		use TokenKind::*;

		if self.advance_if(False) {
			return Ok(Expression::Literal(Value::Bool(false), self.previous()));
		}

		if self.advance_if(True) {
			return Ok(Expression::Literal(Value::Bool(true), self.previous()));
		}

		if self.advance_if(Nil) {
			return Ok(Expression::Literal(Value::Nil, self.previous()));
		}

		if self.advance_if_any(&[Number, String]) {
			let token = self.previous();
			return Ok(Expression::Literal(Value::from(token.value.clone().unwrap()), token));
		}

		if self.advance_if(LeftParen) {
			let expression = self.parse_expression()?;
			self.expect(RightParen, "')'")?;
			return Ok(Expression::Grouping(Box::new(expression)));
		}

		if self.advance_if(Identifier) {
			return Ok(Expression::Variable(self.previous()));
		}

		let (location, actual) = match self.peek() {
			Ok(token) => (token.location.clone(), token.to_string()),
			Err(error) => (error.location.clone(), error.to_string()),
		};

		Err(ParseError { location, expected: "expression".to_string(), actual })
	}

	fn peek(&mut self) -> &tokenize::Result {
		// Tokens will always end with EndOfFile token
		self.tokens.peek().unwrap()
	}

	fn check(&mut self, token_type: TokenKind) -> bool {
		matches!(self.peek(), Ok(it) if it.kind == token_type)
	}

	fn expect(&mut self, token_type: TokenKind, expected: &'static str) -> Result<Token> {
		if self.check(token_type) {
			self.advance();
			Ok(self.previous())
		}
		else {
			let (location, actual) = self.peek().clone().map_or_else(
				|error| (error.location.clone(), error.to_string()),
				|token| (token.location.clone(), token.to_string()),
			);

			Err(ParseError { location, expected: expected.to_string(), actual })
		}
	}

	fn advance(&mut self) {
		self.previous = self.tokens.next();
	}

	fn advance_if(&mut self, token_type: TokenKind) -> bool {
		let result = self.check(token_type);

		if result {
			self.advance();
		}

		result
	}

	fn advance_if_any(&mut self, token_types: &[TokenKind]) -> bool {
		for &token_type in token_types {
			if self.check(token_type) {
				self.advance();
				return true;
			}
		}
		false
	}

	/// Gets the previous Token, which is assumed to be valid and present. This function must only
	/// be called after at least one advance, and only if the previous Token was valid.
	fn previous(&mut self) -> Token {
		match self.previous {
			Some(Ok(ref token)) => token.clone(),
			Some(Err(_)) => panic!("Don't call `previous` on an error token!"),
			None => panic!("Don't call `previous` before parsing something!"),
		}
	}
}

impl Iterator for Parser<'_> {
	type Item = Result<Statement>;

	fn next(&mut self) -> Option<Self::Item> {
		use TokenKind::*;

		match self.peek() {
			Ok(Token { kind: EndOfFile, .. }) => {
				self.advance();
				None
			}
			_ => {
				let next = self.parse_declaration();

				if next.is_err() {
					self.synchronize();
				}

				Some(next)
			}
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	pub fn boolean_true() {
		let input = "true";
		let expected = "true".to_string();

		let actual = Parser::new(Tokens::new(input))
			.parse_expression()
			.map_or_else(|error| format!("{error}"), |expression| format!("{expression}"));

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn boolean_false() {
		let input = "false";
		let expected = "false".to_string();

		let actual = Parser::new(Tokens::new(input))
			.parse_expression()
			.map_or_else(|error| format!("{error}"), |expression| format!("{expression}"));

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn number() {
		let input = "420.69";
		let expected = "420.69".to_string();

		let actual = Parser::new(Tokens::new(input))
			.parse_expression()
			.map_or_else(|error| format!("{error}"), |expression| format!("{expression}"));

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn string() {
		let input = "\"Hello, World!\"";
		let expected = "\"Hello, World!\"".to_string();

		let actual = Parser::new(Tokens::new(input))
			.parse_expression()
			.map_or_else(|error| format!("{error}"), |expression| format!("{expression}"));

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn addition() {
		let input = "1 + 2";
		let expected = "(+ 1 2)".to_string();

		let actual = Parser::new(Tokens::new(input))
			.parse_expression()
			.map_or_else(|error| format!("{error}"), |expression| format!("{expression}"));

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn compound_expression() {
		let input = "(1 + 2) * 3 + 4";
		let expected = "(+ (* (group (+ 1 2)) 3) 4)".to_string();

		let actual = Parser::new(Tokens::new(input))
			.parse_expression()
			.map_or_else(|error| format!("{error}"), |expression| format!("{expression}"));

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn subtract_strings() {
		let input = "\"1\" - \"2\"";
		let expected = "(- \"1\" \"2\")".to_string();

		let actual = Parser::new(Tokens::new(input))
			.parse_expression()
			.map_or_else(|error| format!("{error}"), |expression| format!("{expression}"));

		assert_eq!(expected, actual);
	}
}
