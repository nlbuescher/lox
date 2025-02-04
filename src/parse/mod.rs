mod error;
mod expression;
mod statement;

use std::iter::Peekable;
use std::rc::Rc;

pub use expression::Expression;
pub use statement::Statement;

use crate::error::{Error, Result};
use crate::location::Locate;
use crate::tokenize;
use crate::tokenize::{Token, TokenKind, Tokens};

#[derive(Clone)]
pub struct Parser<'a> {
	tokens: Peekable<Tokens<'a>>,
	previous: Option<tokenize::Result>,
}

impl<'a> Parser<'a> {
	pub fn new(tokens: Tokens<'a>) -> Self {
		Parser { tokens: tokens.peekable(), previous: None }
	}

	pub fn synchronize(&mut self) {
		if matches!(self.peek(), Ok(token) if token.kind == TokenKind::EndOfFile) {
			return;
		}

		self.advance();

		loop {
			if self.previous().kind == TokenKind::Semicolon {
				break;
			}

			if self.peek().as_ref().is_ok_and(|it| {
				matches!(
					it.kind,
					TokenKind::Class
						| TokenKind::Fun | TokenKind::Var
						| TokenKind::For | TokenKind::If
						| TokenKind::While | TokenKind::Print
						| TokenKind::Return
				)
			}) {
				break;
			}

			self.advance();
		}
	}

	fn parse_declaration(&mut self) -> Result<Statement> {
		if self.advance_if(TokenKind::Class) {
			return self.parse_class_declaration();
		}

		if self.advance_if(TokenKind::Fun) {
			return self.parse_function_declaration();
		}

		if self.advance_if(TokenKind::Var) {
			return self.parse_variable_declaration();
		}

		self.parse_statement()
	}

	fn parse_class_declaration(&mut self) -> Result<Statement> {
		let keyword = Box::new(self.previous());
		let name = Box::new(self.expect(TokenKind::Identifier, "class name")?);
		let open_brace = Box::new(self.expect(TokenKind::LeftBrace, "'{' before class body")?);

		let mut methods = Vec::new();
		while !self.check(TokenKind::RightBrace) {
			methods.push(self.parse_function_declaration()?);
		}

		let close_brace = Box::new(self.expect(TokenKind::RightBrace, "'}' after class body")?);

		Ok(Statement::ClassDeclaration { keyword, name, open_brace, methods, close_brace })
	}

	fn parse_function_declaration(&mut self) -> Result<Statement> {
		let keyword = Box::new(self.previous());
		let name = Box::new(self.expect(TokenKind::Identifier, "function name")?);

		self.expect(TokenKind::LeftParen, "'(' after function name")?;

		let mut parameters = Vec::new();
		if !self.check(TokenKind::RightParen) {
			parameters.push(self.expect(TokenKind::Identifier, "parameter name")?);

			while self.advance_if(TokenKind::Comma) {
				parameters.push(self.expect(TokenKind::Identifier, "parameter name")?);
			}
		}

		self.expect(TokenKind::RightParen, "')' after parameters")?;
		self.expect(TokenKind::LeftBrace, "'{' before function body")?;

		let body = Rc::new(self.parse_block()?);

		Ok(Statement::FunctionDeclaration { keyword, name, parameters, body })
	}

	fn parse_variable_declaration(&mut self) -> Result<Statement> {
		let keyword = Box::new(self.previous());
		let name = Box::new(self.expect(TokenKind::Identifier, "variable name")?);

		let initializer = if self.advance_if(TokenKind::Equal) {
			Some(Box::new(self.parse_expression()?))
		} else {
			None
		};

		self.expect(TokenKind::Semicolon, "';' after variable declaration")?;

		Ok(Statement::VariableDeclaration { keyword, name, initializer })
	}

	fn parse_statement(&mut self) -> Result<Statement> {
		if self.advance_if(TokenKind::LeftBrace) {
			return self.parse_block();
		}

		if self.advance_if(TokenKind::For) {
			return self.parse_for_statement();
		}

		if self.advance_if(TokenKind::If) {
			return self.parse_if_statement();
		}

		if self.advance_if(TokenKind::Print) {
			return self.parse_print_statement();
		}

		if self.advance_if(TokenKind::Return) {
			return self.parse_return_statement();
		}

		if self.advance_if(TokenKind::While) {
			return self.parse_while_statement();
		}

		self.parse_expression_statement()
	}

	fn parse_return_statement(&mut self) -> Result<Statement> {
		let keyword = Box::new(self.previous());

		let expression = if !self.check(TokenKind::Semicolon) {
			Some(Box::new(self.parse_expression()?))
		} else {
			None
		};

		self.expect(TokenKind::Semicolon, "';' after return value")?;

		Ok(Statement::Return { keyword, expression })
	}

	fn parse_block(&mut self) -> Result<Statement> {
		let open_brace = Box::new(self.previous());

		let mut statements = Vec::new();
		while !self.check(TokenKind::RightBrace) {
			statements.push(self.parse_declaration()?);
		}

		let close_brace = Box::new(self.expect(TokenKind::RightBrace, "'}' after block")?);

		Ok(Statement::Block { open_brace, statements, close_brace })
	}

	fn parse_for_statement(&mut self) -> Result<Statement> {
		let keyword = Box::new(self.previous());

		self.expect(TokenKind::LeftParen, "'(' after 'for'")?;

		let initializer = if self.advance_if(TokenKind::Semicolon) {
			None
		} else if self.advance_if(TokenKind::Var) {
			Some(Box::new(self.parse_variable_declaration()?))
		} else {
			Some(Box::new(self.parse_expression_statement()?))
		};

		let condition = if !self.check(TokenKind::Semicolon) {
			Some(Box::new(self.parse_expression()?))
		} else {
			None
		};

		self.expect(TokenKind::Semicolon, "';' after for condition")?;

		let increment = if !self.check(TokenKind::RightParen) {
			Some(Box::new(Statement::Expression(Box::new(self.parse_expression()?))))
		} else {
			None
		};

		self.expect(TokenKind::RightParen, "')' after for clauses")?;
		self.expect(TokenKind::LeftBrace, "block after for clauses")?;

		let body = Box::new(self.parse_block()?);

		Ok(Statement::For { keyword, initializer, condition, increment, body })
	}

	fn parse_if_statement(&mut self) -> Result<Statement> {
		let keyword = Box::new(self.previous());

		let condition = Box::new(self.parse_expression()?);

		self.expect(TokenKind::LeftBrace, "'{' after if condition")?;

		let then_branch = Box::new(self.parse_block()?);

		let else_branch = if self.advance_if(TokenKind::Else) {
			let keyword = self.previous();

			let next = self
				.expect_any(&[TokenKind::If, TokenKind::LeftBrace], "'if' or '{' after 'else'")?;

			let else_branch = if next.kind == TokenKind::If {
				Box::new(self.parse_if_statement()?)
			} else {
				Box::new(self.parse_block()?)
			};

			Some((keyword, else_branch))
		} else {
			None
		};

		Ok(Statement::If { keyword, condition, then_branch, else_branch })
	}

	fn parse_print_statement(&mut self) -> Result<Statement> {
		let keyword = Box::new(self.previous());
		let expression = self.parse_expression()?;
		self.expect(TokenKind::Semicolon, "';' after expression")?;
		Ok(Statement::Print { keyword, expression: Box::new(expression) })
	}

	fn parse_while_statement(&mut self) -> Result<Statement> {
		let keyword = Box::new(self.previous());
		let condition = Box::new(self.parse_expression()?);
		self.expect(TokenKind::LeftBrace, "'{' after while condition")?;
		let body = Box::new(self.parse_block()?);
		Ok(Statement::While { keyword, condition, body })
	}

	fn parse_expression_statement(&mut self) -> Result<Statement> {
		let expression = self.parse_expression()?;
		self.expect(TokenKind::Semicolon, "';' after expression")?;
		Ok(Statement::Expression(Box::new(expression)))
	}

	fn parse_expression(&mut self) -> Result<Expression> {
		self.parse_assignment()
	}

	fn parse_assignment(&mut self) -> Result<Expression> {
		let expression = self.parse_or()?;

		if self.advance_if(TokenKind::Equal) {
			let equals = self.previous();
			let value = self.parse_assignment()?;

			if let Expression::Variable(name) = expression {
				return Ok(Expression::Assignment { name, expression: Box::new(value) });
			}

			if let Expression::Get { object, property } = expression {
				return Ok(Expression::Set { object, property, value: Box::new(value) });
			}

			return Err(Error::invalid_assignment(equals.locate().clone()));
		}

		Ok(expression)
	}

	fn parse_or(&mut self) -> Result<Expression> {
		let mut expression = self.parse_and()?;

		while self.advance_if(TokenKind::Or) {
			expression = Expression::Binary {
				left: Box::new(expression),
				operator: Box::new(self.previous()),
				right: Box::new(self.parse_and()?),
			}
		}

		Ok(expression)
	}

	fn parse_and(&mut self) -> Result<Expression> {
		let mut expression = self.parse_equality()?;

		while self.advance_if(TokenKind::And) {
			expression = Expression::Binary {
				left: Box::new(expression),
				operator: Box::new(self.previous()),
				right: Box::new(self.parse_equality()?),
			}
		}

		Ok(expression)
	}

	fn parse_equality(&mut self) -> Result<Expression> {
		let mut expression = self.parse_comparison()?;

		while self.advance_if_any(&[TokenKind::BangEqual, TokenKind::EqualEqual]) {
			expression = Expression::Binary {
				left: Box::new(expression),
				operator: Box::new(self.previous()),
				right: Box::new(self.parse_comparison()?),
			}
		}

		Ok(expression)
	}

	fn parse_comparison(&mut self) -> Result<Expression> {
		let mut expression = self.parse_term()?;

		while self.advance_if_any(&[
			TokenKind::Greater,
			TokenKind::GreaterEqual,
			TokenKind::Less,
			TokenKind::LessEqual,
		]) {
			expression = Expression::Binary {
				left: Box::new(expression),
				operator: Box::new(self.previous()),
				right: Box::new(self.parse_term()?),
			}
		}

		Ok(expression)
	}

	fn parse_term(&mut self) -> Result<Expression> {
		let mut expression = self.parse_factor()?;

		while self.advance_if_any(&[TokenKind::Minus, TokenKind::Plus]) {
			expression = Expression::Binary {
				left: Box::new(expression),
				operator: Box::new(self.previous()),
				right: Box::new(self.parse_factor()?),
			}
		}

		Ok(expression)
	}

	fn parse_factor(&mut self) -> Result<Expression> {
		let mut expression = self.parse_unary()?;

		while self.advance_if_any(&[TokenKind::Slash, TokenKind::Star]) {
			expression = Expression::Binary {
				left: Box::new(expression),
				operator: Box::new(self.previous()),
				right: Box::new(self.parse_unary()?),
			}
		}

		Ok(expression)
	}

	fn parse_unary(&mut self) -> Result<Expression> {
		if self.advance_if_any(&[TokenKind::Bang, TokenKind::Minus]) {
			Ok(Expression::Unary {
				operator: Box::new(self.previous()),
				expression: Box::new(self.parse_unary()?),
			})
		} else {
			self.parse_call()
		}
	}

	fn parse_call(&mut self) -> Result<Expression> {
		let mut expression = self.parse_primary()?;

		if self.advance_if(TokenKind::LeftParen) {
			let open_paren = Box::new(self.previous());

			let mut arguments = Vec::new();

			if !self.check(TokenKind::RightParen) {
				arguments.push(self.parse_expression()?);

				while self.advance_if(TokenKind::Comma) {
					arguments.push(self.parse_expression()?);
				}
			}

			let close_paren = Box::new(self.expect(TokenKind::RightParen, "')' after arguments")?);

			expression = Expression::Call {
				callee: Box::new(expression),
				open_paren,
				arguments,
				close_paren,
			};
		} else if self.advance_if(TokenKind::Dot) {
			let property = Box::new(self.expect(TokenKind::Identifier, "property name after '.'")?);
			expression = Expression::Get { object: Box::new(expression), property };
		}

		Ok(expression)
	}

	fn parse_primary(&mut self) -> Result<Expression> {
		if self.advance_if_any(&[
			TokenKind::False,
			TokenKind::True,
			TokenKind::Nil,
			TokenKind::Number,
			TokenKind::String,
		]) {
			return Ok(Expression::Literal(Box::new(self.previous())));
		}

		if self.advance_if(TokenKind::LeftParen) {
			let expression = self.parse_expression()?;
			self.expect(TokenKind::RightParen, "')'")?;
			return Ok(Expression::Grouping(Box::new(expression)));
		}

		if self.advance_if(TokenKind::Identifier) {
			return Ok(Expression::Variable(Box::new(self.previous())));
		}

		if self.advance_if(TokenKind::Fun) {
			let keyword = Box::new(self.previous());

			let open_paren = Box::new(self.expect(TokenKind::LeftParen, "'(' after fun keyword")?);

			let mut parameters = Vec::new();
			if !self.check(TokenKind::RightParen) {
				parameters.push(self.expect(TokenKind::Identifier, "parameter name")?);

				while self.advance_if(TokenKind::Comma) {
					parameters.push(self.expect(TokenKind::Identifier, "parameter name")?);
				}
			}

			let close_paren = Box::new(self.expect(TokenKind::RightParen, "')' after parameters")?);

			self.expect(TokenKind::LeftBrace, "'{' before function body")?;

			let body = Rc::new(self.parse_block()?);

			return Ok(Expression::Function { keyword, open_paren, parameters, close_paren, body });
		}

		Err(Error::unexpected_token("expression", self.peek()))
	}

	fn peek(&mut self) -> &tokenize::Result {
		// Tokens will always end with EndOfFile token
		self.tokens.peek().unwrap()
	}

	fn check(&mut self, token_kind: TokenKind) -> bool {
		matches!(self.peek(), Ok(it) if it.kind == token_kind)
	}

	fn expect(&mut self, token_kind: TokenKind, expected: &'static str) -> Result<Token> {
		self.expect_any(&[token_kind], expected)
	}

	fn expect_any(&mut self, token_types: &[TokenKind], expected: &'static str) -> Result<Token> {
		for &token_kind in token_types {
			if self.check(token_kind) {
				self.advance();
				return Ok(self.previous());
			}
		}

		Err(Error::unexpected_token(expected, self.peek()))
	}

	fn advance(&mut self) {
		self.previous = self.tokens.next();
	}

	fn advance_if(&mut self, token_kind: TokenKind) -> bool {
		if self.check(token_kind) {
			self.advance();
			return true;
		}
		false
	}

	fn advance_if_any(&mut self, token_types: &[TokenKind]) -> bool {
		for &token_kind in token_types {
			if self.check(token_kind) {
				self.advance();
				return true;
			}
		}
		false
	}

	/// Gets the previous Token, which is assumed to be present and valid. This function must only
	/// be called after at least one advance, and only if the previous Token was valid.
	fn previous(&mut self) -> Token {
		match self.previous {
			None => panic!("Don't call `previous` before parsing something!"),
			Some(Err(_)) => panic!("Don't call `previous` on an error token!"),
			Some(Ok(ref token)) => token.clone(),
		}
	}
}

impl Iterator for Parser<'_> {
	type Item = Result<Statement>;

	fn next(&mut self) -> Option<Self::Item> {
		if matches!(self.peek(), Ok(token) if token.kind == TokenKind::EndOfFile) {
			self.advance();
			None
		} else {
			let next = self.parse_declaration();

			if next.is_err() {
				self.synchronize();
			}

			Some(next)
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use crate::tokenize::Tokens;

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
