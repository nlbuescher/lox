use crate::tokenize::{AnnotatedToken, Token};
use std::fmt::Display;

pub enum Expression {
	Binary { left: Box<Expression>, operator: AnnotatedToken, right: Box<Expression> },
	Literal(AnnotatedToken),
	Grouping(Box<Expression>),
	Unary { operator: AnnotatedToken, right: Box<Expression> },
}

#[derive(Debug)]
pub enum Error {
	UnexpectedToken { token: AnnotatedToken, message: String },
}

impl Display for Error {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Error::UnexpectedToken { token: AnnotatedToken { token, location }, message } => {
				write!(f, "{location} {message}: got {}", token.text())
			}
		}
	}
}

pub struct Parser<'a> {
	tokens: &'a [AnnotatedToken],
	position: usize,
}

impl<'a> Parser<'a> {
	pub fn new(tokens: &'a [AnnotatedToken]) -> Self {
		Parser { tokens, position: 0 }
	}

	fn peek(&self) -> Option<&AnnotatedToken> {
		if self.position < self.tokens.len() {
			Some(&self.tokens[self.position])
		}
		else {
			None
		}
	}

	fn previous(&self) -> AnnotatedToken {
		if self.position > 0 && self.position < self.tokens.len() {
			self.tokens[self.position - 1].clone()
		}
		else {
			unreachable!("Don't call `previous` before parsing something!")
		}
	}

	fn check(&self, predicate: impl FnOnce(&Token) -> bool) -> bool {
		match self.peek() {
			Some(AnnotatedToken { token, .. }) => predicate(token),
			None => false,
		}
	}

	fn expect(
		&mut self,
		predicate: impl FnOnce(&Token) -> bool,
		message: &str,
	) -> Result<(), Error> {
		if self.check(predicate) {
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
		if self.peek().is_some() {
			self.position += 1;
		}
	}

	fn advance_if(&mut self, predicate: impl Fn(&Token) -> bool) -> bool {
		let result = self.check(predicate);

		if result {
			self.advance();
		}

		result
	}

	fn parse_primary(&mut self) -> Result<Expression, Error> {
		if self.advance_if(|next| {
			matches!(
				next,
				Token::False
					| Token::True | Token::Nil
					| Token::Number { .. }
					| Token::String { .. }
			)
		}) {
			return Ok(Expression::Literal(self.previous()));
		}

		if self.advance_if(|next| matches!(next, Token::LeftParen)) {
			let expression = self.parse_expression()?;
			self.expect(|next| matches!(next, Token::RightParen), "expected ')'")?;
			return Ok(Expression::Grouping(Box::new(expression)));
		}

		Err(Error::UnexpectedToken {
			token: self.peek().unwrap().clone(),
			message: "Expected expression".to_string(),
		})
	}

	fn parse_unary(&mut self) -> Result<Expression, Error> {
		if self.advance_if(|next| matches!(next, Token::Bang | Token::Minus)) {
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

		while self.advance_if(|next| matches!(next, Token::Slash | Token::Star)) {
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

		while self.advance_if(|next| matches!(next, Token::Minus | Token::Plus)) {
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

		while self.advance_if(|next| {
			matches!(next, Token::Greater | Token::GreaterEqual | Token::Less | Token::LessEqual)
		}) {
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

		while self.advance_if(|next| matches!(next, Token::BangEqual | Token::EqualEqual)) {
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
}
