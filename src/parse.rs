use std::fmt::{Display, Formatter};
use std::iter::Peekable;
use std::string::String;

use crate::tokenize::TokenType::*;
use crate::tokenize::{
	Error as TokenError, Location, Token, TokenType, Tokens, Value as TokenValue,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
	Nil,
	Bool(bool),
	Number(f64),
	String(String),
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

	fn as_number(&self, expression: &Expression) -> Result<&f64, Error> {
		match self {
			Value::Number(n) => Ok(n),
			_ => Err(Error::UnexpectedType {
				message: "Expected number".into(),
				location: expression.location(),
				value: self.clone(),
			}),
		}
	}

	fn as_string(&self, expression: &Expression) -> Result<String, Error> {
		match self {
			Value::String(s) => Ok(s.clone()),
			_ => Err(Error::UnexpectedType {
				message: "Expected string".into(),
				location: expression.location(),
				value: self.clone(),
			}),
		}
	}
}

impl From<TokenValue> for Value {
	fn from(value: TokenValue) -> Self {
		match value {
			TokenValue::Number(number) => Value::Number(number),
			TokenValue::String(string) => Value::String(string),
		}
	}
}

#[derive(Debug)]
pub enum Error {
	UnexpectedToken { message: String, token: Result<Token, TokenError> },
	UnexpectedType { message: String, location: Location, value: Value },
}

impl Display for Error {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		match self {
			Error::UnexpectedToken {
				message,
				token: Ok(Token { location, token_type, text, .. }),
			} => {
				write!(f, "{location} {message}; got {token_type} {text}")
			}

			Error::UnexpectedToken {
				message,
				token: Err(TokenError { location, error_type, text }),
			} => {
				write!(f, "{location} {message}; got {error_type} {text}")
			}

			Error::UnexpectedType { location, message, value } => {
				write!(f, "{location} {message}; got {value:?}")
			}
		}
	}
}

#[derive(Debug, Clone)]
pub enum Expression {
	Binary { left: Box<Expression>, operator: Token, right: Box<Expression> },
	Literal(Value, Token),
	Grouping(Box<Expression>),
	Unary { operator: Token, right: Box<Expression> },
}

impl Expression {
	pub fn location(&self) -> Location {
		match self {
			Expression::Binary { left, .. } => left.location(),
			Expression::Grouping(expression) => expression.location(),
			Expression::Literal(_, token) => token.location.clone(),
			Expression::Unary { operator, .. } => operator.location.clone(),
		}
	}

	pub fn evaluate(&self) -> Result<Value, Error> {
		match self {
			Expression::Binary { left, operator, right } => {
				let left_value = left.evaluate()?;
				let right_value = right.evaluate()?;

				match operator.token_type {
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

			Expression::Grouping(expression) => expression.evaluate(),

			Expression::Literal(value, _) => Ok(value.clone()),

			Expression::Unary { operator, right } => {
				let right_value = right.evaluate()?;

				match operator.token_type {
					Bang => Ok(Value::Bool(!right_value.is_truthy())),

					Minus => match right_value {
						Value::Number(n) => Ok(Value::Number(-n)),
						_ => Err(Error::UnexpectedType {
							message: "Expected number".into(),
							location: right.location(),
							value: right_value,
						}),
					},

					_ => unreachable!("Unknown operator evaluating unary expression!"),
				}
			}
		}
	}
}

impl Display for Expression {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		match self {
			Expression::Binary { left, operator: Token { text, .. }, right } => {
				write!(f, "({text} {left} {right})")
			}

			Expression::Literal(value, _) => match value {
				Value::Nil => write!(f, "nil"),
				Value::Bool(b) => write!(f, "{b}"),
				Value::Number(n) => write!(f, "{n}"),
				Value::String(s) => write!(f, "\"{s}\""),
			},

			Expression::Grouping(expression) => write!(f, "(group {expression})"),

			Expression::Unary { operator, right } => write!(f, "({operator} {right})"),
		}
	}
}

impl Display for Value {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		match self {
			Value::Nil => write!(f, "nil"),
			Value::Bool(b) => write!(f, "{b}"),
			Value::Number(n) => write!(f, "{n}"),
			Value::String(s) => write!(f, "{s}"),
		}
	}
}

#[derive(Debug, Clone)]
pub enum Statement {
	Expression(Expression),
	Print(Expression),
}

impl Statement {
	pub fn execute(&self) -> Result<Option<Value>, Error> {
		match self {
			Statement::Expression(expression) => expression.evaluate().map(Some),
			Statement::Print(expression) => {
				println!("{}", expression.evaluate()?);
				Ok(None)
			}
		}
	}
}

impl Display for Statement {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		match self {
			Statement::Expression(expression) => write!(f, "(expr {expression});"),
			Statement::Print(expression) => write!(f, "(print {expression});"),
		}
	}
}

pub struct Parser<'a> {
	tokens: Peekable<Tokens<'a>>,
	previous: Option<Result<Token, TokenError>>,
}

impl<'a> Parser<'a> {
	pub fn new(tokens: Tokens<'a>) -> Self {
		Parser { tokens: tokens.peekable(), previous: None }
	}

	fn peek(&mut self) -> &Result<Token, TokenError> {
		// Tokens will always end with EndOfFile token
		self.tokens.peek().unwrap()
	}

	fn previous(&mut self) -> Token {
		if self.previous.is_none() {
			panic!("Don't call `previous` before parsing something!")
		}

		self.previous.take().unwrap().unwrap()
	}

	fn check(&mut self, token_type: TokenType) -> bool {
		matches!(self.peek(), Ok(it) if it.token_type == token_type)
	}

	fn expect(&mut self, token_type: TokenType, message: &str) -> Result<(), Error> {
		if self.check(token_type) {
			self.advance();
			Ok(())
		}
		else {
			Err(Error::UnexpectedToken { token: self.peek().clone(), message: message.to_string() })
		}
	}

	fn advance(&mut self) {
		self.previous = self.tokens.next();
	}

	fn advance_if(&mut self, token_type: TokenType) -> bool {
		let result = self.check(token_type);

		if result {
			self.advance();
		}

		result
	}

	fn advance_if_any(&mut self, token_types: &[TokenType]) -> bool {
		for &token_type in token_types {
			if self.check(token_type) {
				self.advance();
				return true;
			}
		}
		false
	}

	fn parse_primary(&mut self) -> Result<Expression, Error> {
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
			self.expect(RightParen, "expected ')'")?;
			return Ok(Expression::Grouping(Box::new(expression)));
		}

		Err(Error::UnexpectedToken {
			token: self.peek().clone(),
			message: "Expected expression".to_string(),
		})
	}

	fn parse_unary(&mut self) -> Result<Expression, Error> {
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

	fn parse_factor(&mut self) -> Result<Expression, Error> {
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

	fn parse_term(&mut self) -> Result<Expression, Error> {
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

	fn parse_comparison(&mut self) -> Result<Expression, Error> {
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

	fn parse_equality(&mut self) -> Result<Expression, Error> {
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

	fn parse_expression(&mut self) -> Result<Expression, Error> {
		self.parse_equality()
	}

	fn parse_print_statement(&mut self) -> Result<Statement, Error> {
		let value = self.parse_expression()?;
		self.expect(Semicolon, "Expected ';' after value")?;
		Ok(Statement::Print(value))
	}

	fn parse_expression_statement(&mut self) -> Result<Statement, Error> {
		let value = self.parse_expression()?;
		self.expect(Semicolon, "Expected ';' after expression")?;
		Ok(Statement::Expression(value))
	}

	fn parse_statement(&mut self) -> Result<Statement, Error> {
		if self.advance_if(Print) {
			self.parse_print_statement()
		}
		else {
			self.parse_expression_statement()
		}
	}

	#[allow(dead_code)]
	pub fn synchronize(&mut self) {
		self.advance();

		loop {
			if self.previous().token_type == Semicolon {
				return;
			}

			if let Ok(Token {
				token_type: Class | Fun | Var | For | If | While | Print | Return,
				..
			}) = self.peek()
			{
				return;
			}

			self.advance();
		}
	}
}

impl Iterator for Parser<'_> {
	type Item = Result<Statement, Error>;

	fn next(&mut self) -> Option<Self::Item> {
		match self.peek() {
			Ok(Token { token_type: EndOfFile, .. }) => {
				self.advance();
				None
			}
			_ => Some(self.parse_statement()),
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
