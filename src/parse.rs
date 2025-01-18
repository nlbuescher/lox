use std::fmt::{Display, Formatter};
use std::io::{stdout, Write};
use std::iter::Peekable;
use std::ops::Deref;

use crate::location::{Locatable, Location};
use crate::tokenize;
use crate::tokenize::{Error, Token, TokenKind, Tokens};

pub type Result<T> = std::result::Result<T, ParseError>;

#[derive(Debug)]
pub enum ParseError {
	UnexpectedToken { expected: &'static str, actual: Box<tokenize::Result> },
	InvalidAssignment { location: Location },
}

impl Display for ParseError {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		if f.alternate() {
			write!(f, "{location} ", location = self.location())?;
		}
		match self {
			ParseError::UnexpectedToken { expected, actual } => match actual.deref() {
				Ok(token) => write!(f, "Expected {expected} but got {token}"),
				Err(error) => write!(f, "Expected {expected} but got {error}"),
			},
			ParseError::InvalidAssignment { .. } => {
				write!(f, "Invalid assignment target")
			}
		}
	}
}

impl Locatable for ParseError {
	fn location(&self) -> &Location {
		match self {
			ParseError::UnexpectedToken { actual, .. } => actual.location(),
			ParseError::InvalidAssignment { location, .. } => location,
		}
	}
}

#[derive(Debug, Clone)]
pub enum Expression {
	Assignment { name: Box<Token>, value: Box<Expression> },
	Binary { left: Box<Expression>, operator: Box<Token>, right: Box<Expression> },
	Literal(Box<Token>),
	Grouping(Box<Expression>),
	Unary { operator: Box<Token>, right: Box<Expression> },
	Variable(Box<Token>),
}

impl Locatable for Expression {
	fn location(&self) -> &Location {
		match self {
			Expression::Assignment { name, .. } => name.location(),
			Expression::Binary { left, .. } => left.location(),
			Expression::Grouping(expression) => expression.location(),
			Expression::Literal(token) => token.location(),
			Expression::Unary { operator, .. } => operator.location(),
			Expression::Variable(token) => token.location(),
		}
	}
}

impl Display for Expression {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		if f.alternate() {
			write!(f, "{location} ", location = self.location())?;
		}
		match self {
			Expression::Assignment { name, value } => write!(f, "(assign {name} = {value})"),

			Expression::Binary { left, operator, right } => {
				write!(f, "({text} {left} {right})", text = operator.text)
			}

			Expression::Literal(token) => write!(f, "{text}", text = token.text),

			Expression::Grouping(expression) => write!(f, "(group {expression})"),

			Expression::Unary { operator, right } => write!(f, "({operator} {right})"),

			Expression::Variable(token) => write!(f, "(var {text})", text = token.text),
		}
	}
}

#[derive(Debug, Clone)]
pub enum Statement {
	Block {
		start_location: Location,
		end_location: Location,
		statements: Vec<Statement>,
	},
	Expression(Box<Expression>),
	For {
		location: Location,
		initializer: Option<Box<Statement>>,
		condition: Option<Box<Expression>>,
		increment: Option<Box<Statement>>,
		body: Box<Statement>,
	},
	If {
		if_location: Location,
		condition: Box<Expression>,
		then_branch: Box<Statement>,
		else_location: Option<Location>,
		else_branch: Option<Box<Statement>>,
	},
	Print {
		location: Location,
		expression: Box<Expression>,
	},
	VariableDeclaration {
		location: Location,
		name: Box<Token>,
		initializer: Option<Box<Expression>>,
	},
	While {
		location: Location,
		condition: Box<Expression>,
		body: Box<Statement>,
	},
}

impl Display for Statement {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		match self {
			Statement::Block { start_location, end_location, statements, .. } => {
				if f.alternate() {
					start_location.fmt(f)?;
				}
				writeln!(f, "{{")?;
				stdout().flush().unwrap();

				let width = f.width().map(|it| it + 1);

				for statement in statements {
					if let Some(width) = width {
						if f.alternate() {
							writeln!(f, "{statement:#width$}")?;
						}
						else {
							writeln!(f, "{statement:width$}")?;
						}
					}
					else {
						if f.alternate() {
							write!(f, "{statement:#}")?;
						}
						else {
							write!(f, "{statement}")?;
						}
					}
				}

				if f.alternate() {
					end_location.fmt(f)?;
				}
				write!(f, "}}")
			}

			Statement::Expression(expression) => {
				if f.alternate() {
					expression.location().fmt(f)?;
				}
				write!(f, "(expr {expression})")
			}

			Statement::For { location, initializer, condition, increment, body } => {
				let initializer = initializer.as_ref().map_or(String::new(), |it| it.to_string());
				let condition = condition.as_ref().map_or(String::new(), |it| it.to_string());
				let increment = increment.as_ref().map_or(String::new(), |it| it.to_string());

				if let Some(width) = f.width() {
					if f.alternate() {
						location.fmt(f)?;
						write!(f, "for ({initializer} ; {condition} ; {increment})\n{body:#width$}")
					} else {
						write!(f, "for ({initializer} ; {condition} ; {increment})\n{body:width$}")
					}
				} else {
					if f.alternate() {
						location.fmt(f)?;
						write!(f, "for ({initializer} ; {condition} ; {increment})\n{body:#}")
					} else {
						write!(f, "for ({initializer} ; {condition} ; {increment})\n{body}")
					}
				}
			}

			Statement::If { if_location, condition, then_branch, else_location, else_branch } => {
				if f.alternate() {
					if let Some(width) = f.width() {
						if_location.fmt(f)?;
						write!(f, "if {condition}\n{then_branch:#width$}")?;
						if let Some(else_branch) = else_branch {
							writeln!(f)?;
							else_location.as_ref().unwrap().fmt(f)?;
							write!(f, "else\n{else_branch:#width$}")?;
						}
					}
					else {
						if_location.fmt(f)?;
						write!(f, "if {condition}\n{then_branch:#}")?;
						if let Some(else_branch) = else_branch {
							writeln!(f)?;
							else_location.as_ref().unwrap().fmt(f)?;
							write!(f, "else\n{else_branch:#}")?;
						}
					}
				}
				else {
					if let Some(width) = f.width().map(|it| it + 1) {
						write!(f, "if {condition}\n{then_branch:width$}")?;
						if let Some(else_branch) = else_branch {
							writeln!(f, "\nelse\n{else_branch:width$}")?;
						}
					}
					else {
						if_location.fmt(f)?;
						write!(f, "if {condition}\n{then_branch}")?;
						if let Some(else_branch) = else_branch {
							write!(f, "\nelse\n{else_branch}")?;
						}
					}
				}
				Ok(())
			}

			Statement::Print { location, expression } => {
				if f.alternate() {
					location.fmt(f)?;
				}
				write!(f, "(print {expression})")
			}

			Statement::VariableDeclaration { location, name, initializer } => {
				if f.alternate() {
					location.fmt(f)?;
				}
				match initializer {
					Some(expression) => {
						write!(f, "(vardecl {text} = {expression})", text = name.text)
					}

					None => write!(f, "(vardecl {text})", text = name.text),
				}
			}

			Statement::While { location, condition, body } => {
				if let Some(width) = f.width() {
					if f.alternate() {
						location.fmt(f)?;
						write!(f, "while {condition}\n{body:#width$}")
					} else {
						write!(f, "while {condition}\n{body:width$}")
					}
				} else {
					if f.alternate() {
						location.fmt(f)?;
						write!(f, "while {condition}\n{body:#}")
					} else {
						write!(f, "while {condition}\n{body}")
					}
				}
			}
		}
	}
}

impl Locatable for Statement {
	fn location(&self) -> &Location {
		match self {
			Statement::Block { start_location, .. } => start_location,
			Statement::Expression(expression) => expression.location(),
			Statement::For { location, .. } => location,
			Statement::If { if_location: location, .. } => location,
			Statement::Print { location, .. } => location,
			Statement::VariableDeclaration { name, .. } => name.location(),
			Statement::While { location, .. } => location,
		}
	}
}

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

			if self.peek().is_ok_and(|it| {
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
		if self.advance_if(TokenKind::Var) {
			self.parse_variable_declaration()
		}
		else {
			self.parse_statement()
		}
	}

	fn parse_variable_declaration(&mut self) -> Result<Statement> {
		let location = self.previous.as_ref().unwrap().location().clone();
		let name = Box::new(self.expect(TokenKind::Identifier, "variable name")?);

		let initializer = if self.advance_if(TokenKind::Equal) {
			Some(Box::new(self.parse_expression()?))
		} else {
			None
		};

		self.expect(TokenKind::Semicolon, "';' after variable declaration")?;

		Ok(Statement::VariableDeclaration { location, name, initializer })
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

		if self.advance_if(TokenKind::While) {
			return self.parse_while_statement();
		}

		self.parse_expression_statement()
	}

	fn parse_block(&mut self) -> Result<Statement> {
		let start_location = self.previous.as_ref().unwrap().location().clone();

		let mut statements = Vec::new();

		while !self.check(TokenKind::RightBrace) {
			statements.push(self.parse_declaration()?);
		}

		self.expect(TokenKind::RightBrace, "'}' after block")?;

		let end_location = self.previous.as_ref().unwrap().location().clone();

		Ok(Statement::Block { start_location, end_location, statements })
	}

	fn parse_for_statement(&mut self) -> Result<Statement> {
		let location = self.previous.as_ref().unwrap().location().clone();

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

		Ok(Statement::For { location, initializer, condition, increment, body })
	}

	fn parse_if_statement(&mut self) -> Result<Statement> {
		let if_location = self.previous.as_ref().unwrap().location().clone();

		let condition = Box::new(self.parse_expression()?);

		self.expect(TokenKind::LeftBrace, "'{' after if condition")?;

		let then_branch = Box::new(self.parse_block()?);

		let (else_location, else_branch) = if self.advance_if(TokenKind::Else) {
			let else_location = self.previous.as_ref().unwrap().location().clone();

			let next = self
				.expect_any(&[TokenKind::If, TokenKind::LeftBrace], "'if' or '{' after 'else'")?;

			let else_branch = if next.kind == TokenKind::If {
				Box::new(self.parse_if_statement()?)
			}
			else {
				Box::new(self.parse_block()?)
			};

			(Some(else_location), Some(else_branch))
		}
		else {
			(None, None)
		};

		Ok(Statement::If { if_location, condition, then_branch, else_location, else_branch })
	}

	fn parse_print_statement(&mut self) -> Result<Statement> {
		let location = self.previous.as_ref().unwrap().location().clone();
		let expression = self.parse_expression()?;
		self.expect(TokenKind::Semicolon, "';' after expression")?;
		Ok(Statement::Print { location, expression: Box::new(expression) })
	}

	fn parse_while_statement(&mut self) -> Result<Statement> {
		let location = self.previous.as_ref().unwrap().location().clone();
		let condition = Box::new(self.parse_expression()?);
		self.expect(TokenKind::LeftBrace, "'{' after while condition")?;
		let body = Box::new(self.parse_block()?);
		Ok(Statement::While { location, condition, body })
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

			return if let Expression::Variable(name) = expression {
				Ok(Expression::Assignment { name, value: Box::new(value) })
			}
			else {
				Err(ParseError::InvalidAssignment { location: equals.location().clone() })
			};
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
				right: Box::new(self.parse_unary()?),
			})
		}
		else {
			self.parse_primary()
		}
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

		Err(ParseError::UnexpectedToken {
			expected: "expression",
			actual: Box::new(self.peek().map(Clone::clone).map_err(Clone::clone)),
		})
	}

	fn peek(&mut self) -> std::result::Result<&Token, &Error> {
		// Tokens will always end with EndOfFile token
		self.tokens.peek().unwrap().as_ref()
	}

	fn check(&mut self, token_kind: TokenKind) -> bool {
		matches!(self.peek(), Ok(it) if it.kind == token_kind
		)
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

		Err(ParseError::UnexpectedToken {
			expected,
			actual: Box::new(self.peek().cloned().map_err(Clone::clone)),
		})
	}

	fn advance(&mut self) {
		self.previous = self.tokens.next();
	}

	fn advance_if(&mut self, token_kind: TokenKind) -> bool {
		let result = self.check(token_kind);

		if result {
			self.advance();
		}

		result
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

	/// Gets the previous Token, which is assumed to be valid and present. This function must only
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
		}
		else {
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
	use crate::parse::Parser;
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
