use std::fmt::{Display, Formatter};
use std::iter::Peekable;
use std::string::String;

use crate::tokenize::TokenType::*;
use crate::tokenize::{Error as TokenError, Token, TokenType, Tokens};

#[derive(Debug)]
pub enum Error {
	UnexpectedToken { token: Result<Token, TokenError>, message: String },
}

impl Display for Error {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		match self {
			Error::UnexpectedToken {
				token: Ok(Token { location, token_type, text, .. }),
				message,
			} => {
				write!(f, "{location} {message}; got {token_type} {text}")
			}

			Error::UnexpectedToken {
				token: Err(TokenError { location, error_type, text }),
				message,
			} => {
				write!(f, "{location} {message}; got {error_type} {text}")
			}
		}
	}
}

pub enum Expression {
	Binary { left: Box<Expression>, operator: Token, right: Box<Expression> },
	Literal(Token),
	Grouping(Box<Expression>),
	Unary { operator: Token, right: Box<Expression> },
}

impl Display for Expression {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		match self {
			Expression::Binary { left, operator: Token { text, .. }, right } => {
				write!(f, "({text} {left} {right})")
			}

			Expression::Literal(Token { value, text, .. }) => match value {
				Some(value) => write!(f, "{value}"),
				None => write!(f, "{text}"),
			},

			Expression::Grouping(expression) => write!(f, "(group {expression})"),

			Expression::Unary { operator, right } => write!(f, "({operator} {right})"),
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

	fn peek(&mut self) -> Option<&Result<Token, TokenError>> {
		self.tokens.peek()
	}

	fn previous(&mut self) -> Token {
		if self.previous.is_none() {
			panic!("Don't call `previous` before parsing something!")
		}

		self.previous.take().unwrap().unwrap()
	}

	fn check(&mut self, token_type: TokenType) -> bool {
		matches!(self.peek(), Some(Ok(it)) if it.token_type == token_type)
	}

	fn expect(&mut self, token_type: TokenType, message: &str) -> Result<(), Error> {
		if self.check(token_type) {
			self.advance();
			Ok(())
		}
		else {
			Err(Error::UnexpectedToken {
				token: self.peek().unwrap().clone(),
				message: message.to_string(),
			})
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
		if self.advance_if_any(&[False, True, Nil, Number, TokenType::String]) {
			return Ok(Expression::Literal(self.previous()));
		}

		if self.advance_if(LeftParen) {
			let expression = self.parse_expression()?;
			self.expect(RightParen, "expected ')'")?;
			return Ok(Expression::Grouping(Box::new(expression)));
		}

		Err(Error::UnexpectedToken {
			token: self.peek().unwrap().clone(),
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

	pub fn parse(&mut self) -> Result<Expression, Error> {
		self.parse_expression()
	}

	#[allow(dead_code)]
	pub fn synchronize(&mut self) {
		self.advance();

		while self.peek().is_some() {
			if self.previous().token_type == Semicolon {
				return;
			}

			if let Some(Ok(Token {
				token_type: Class | Fun | Var | For | If | While | Print | Return,
				..
			})) = self.peek()
			{
				return;
			}

			self.advance();
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
			.parse()
			.map_or_else(|error| format!("{error}"), |expression| format!("{expression}"));

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn boolean_false() {
		let input = "false";
		let expected = "false".to_string();

		let actual = Parser::new(Tokens::new(input))
			.parse()
			.map_or_else(|error| format!("{error}"), |expression| format!("{expression}"));

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn number() {
		let input = "420.69";
		let expected = "420.69".to_string();

		let actual = Parser::new(Tokens::new(input))
			.parse()
			.map_or_else(|error| format!("{error}"), |expression| format!("{expression}"));

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn string() {
		let input = "\"Hello, World!\"";
		let expected = "Hello, World!".to_string();

		let actual = Parser::new(Tokens::new(input))
			.parse()
			.map_or_else(|error| format!("{error}"), |expression| format!("{expression}"));

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn addition() {
		let input = "1 + 2";
		let expected = "(+ 1 2)".to_string();

		let actual = Parser::new(Tokens::new(input))
			.parse()
			.map_or_else(|error| format!("{error}"), |expression| format!("{expression}"));

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn compound_expression() {
		let input = "(1 + 2) * 3 + 4";
		let expected = "(+ (* (group (+ 1 2)) 3) 4)".to_string();

		let actual = Parser::new(Tokens::new(input))
			.parse()
			.map_or_else(|error| format!("{error}"), |expression| format!("{expression}"));

		assert_eq!(expected, actual);
	}
}
