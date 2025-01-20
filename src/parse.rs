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

#[derive(Clone)]
pub struct Parser<'a> {
	tokens: Peekable<Tokens<'a>>,
	previous: Option<tokenize::Result>,
}

#[derive(Debug, Clone)]
pub enum Statement {
	Block {
		start_location: Location,
		end_location: Location,
		statements: Box<[Statement]>,
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
		else_branch: Option<(Location, Box<Statement>)>,
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

#[derive(Debug, Clone)]
pub enum Expression {
	Assignment {
		name: Box<Token>,
		value: Box<Expression>,
	},
	Binary {
		left: Box<Expression>,
		operator: Box<Token>,
		right: Box<Expression>,
	},
	Call {
		callee: Box<Expression>,
		arguments_start_location: Location,
		arguments: Box<[Expression]>,
	},
	Literal(Box<Token>),
	Grouping(Box<Expression>),
	Unary {
		operator: Box<Token>,
		right: Box<Expression>,
	},
	Variable(Box<Token>),
}

/////////////////////////////////////////////////////////////////////////////
// Type implementations
/////////////////////////////////////////////////////////////////////////////

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
		let location = self.previous().location().clone();
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
		let start_location = self.previous().location().clone();

		let mut statements = Vec::new();

		while !self.check(TokenKind::RightBrace) {
			statements.push(self.parse_declaration()?);
		}

		self.expect(TokenKind::RightBrace, "'}' after block")?;

		let end_location = self.previous().location().clone();

		Ok(Statement::Block { start_location, end_location, statements: statements.into() })
	}

	fn parse_for_statement(&mut self) -> Result<Statement> {
		let location = self.previous().location().clone();

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
		let if_location = self.previous().location().clone();

		let condition = Box::new(self.parse_expression()?);

		self.expect(TokenKind::LeftBrace, "'{' after if condition")?;

		let then_branch = Box::new(self.parse_block()?);

		let else_branch = if self.advance_if(TokenKind::Else) {
			let else_location = self.previous().location().clone();

			let next = self
				.expect_any(&[TokenKind::If, TokenKind::LeftBrace], "'if' or '{' after 'else'")?;

			let else_branch = if next.kind == TokenKind::If {
				Box::new(self.parse_if_statement()?)
			}
			else {
				Box::new(self.parse_block()?)
			};

			Some((else_location, else_branch))
		}
		else {
			None
		};

		Ok(Statement::If { if_location, condition, then_branch, else_branch })
	}

	fn parse_print_statement(&mut self) -> Result<Statement> {
		let location = self.previous().location().clone();
		let expression = self.parse_expression()?;
		self.expect(TokenKind::Semicolon, "';' after expression")?;
		Ok(Statement::Print { location, expression: Box::new(expression) })
	}

	fn parse_while_statement(&mut self) -> Result<Statement> {
		let location = self.previous().location().clone();
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
			let equals = self.previous().clone();
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
				operator: Box::new(self.previous().clone()),
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
				operator: Box::new(self.previous().clone()),
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
				operator: Box::new(self.previous().clone()),
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
				operator: Box::new(self.previous().clone()),
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
				operator: Box::new(self.previous().clone()),
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
				operator: Box::new(self.previous().clone()),
				right: Box::new(self.parse_unary()?),
			}
		}

		Ok(expression)
	}

	fn parse_unary(&mut self) -> Result<Expression> {
		if self.advance_if_any(&[TokenKind::Bang, TokenKind::Minus]) {
			Ok(Expression::Unary {
				operator: Box::new(self.previous().clone()),
				right: Box::new(self.parse_unary()?),
			})
		}
		else {
			self.parse_call()
		}
	}

	fn parse_call(&mut self) -> Result<Expression> {
		let mut expression = self.parse_primary()?;

		if self.advance_if(TokenKind::LeftParen) {
			let arguments_start_location = self.previous().location().clone();

			let mut arguments = Vec::new();

			if !self.check(TokenKind::RightParen) {
				arguments.push(self.parse_expression()?);

				while self.advance_if(TokenKind::Comma) {
					arguments.push(self.parse_expression()?);
				}
			}

			self.expect(TokenKind::RightParen, "')' after arguments")?;

			expression = Expression::Call {
				callee: Box::new(expression),
				arguments_start_location,
				arguments: arguments.into(),
			};
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
			return Ok(Expression::Literal(Box::new(self.previous().clone())));
		}

		if self.advance_if(TokenKind::LeftParen) {
			let expression = self.parse_expression()?;
			self.expect(TokenKind::RightParen, "')'")?;
			return Ok(Expression::Grouping(Box::new(expression)));
		}

		if self.advance_if(TokenKind::Identifier) {
			return Ok(Expression::Variable(Box::new(self.previous().clone())));
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
				return Ok(self.previous().clone());
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
	fn previous(&mut self) -> &Token {
		match self.previous {
			None => panic!("Don't call `previous` before parsing something!"),
			Some(Err(_)) => panic!("Don't call `previous` on an error token!"),
			Some(Ok(ref token)) => token,
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

/////////////////////////////////////////////////////////////////////////////
// Display
/////////////////////////////////////////////////////////////////////////////

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

impl Display for Statement {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		const PAD: &str = "";

		let width = f.width().unwrap_or(0);

		match self {
			Statement::Block { start_location, end_location, statements, .. } => {
				if f.alternate() {
					write!(f, "{start_location}{PAD:width$} ")?;
				}
				writeln!(f, "{{")?;
				stdout().flush().unwrap();

				{
					let width = width + 1;
					for statement in statements {
						if f.alternate() {
							writeln!(f, "{statement:#width$}")?;
						} else {
							writeln!(f, "{statement:width$}")?;
						}
					}
				}

				if f.alternate() {
					write!(f, "{end_location}{PAD:width$} ")?;
				}
				write!(f, "}}")
			}

			Statement::Expression(expression) => {
				if f.alternate() {
					write!(f, "{location}{PAD:width$} ", location = expression.location())?;
				}
				write!(f, "(expr {expression})")
			}

			Statement::For { location, initializer, condition, increment, body } => {
				let initializer = initializer.as_ref().map_or(String::new(), |it| it.to_string());
				let condition = condition.as_ref().map_or(String::new(), |it| it.to_string());
				let increment = increment.as_ref().map_or(String::new(), |it| it.to_string());

				if f.alternate() {
					write!(f, "{location}{PAD:width$} ")?;
				}

				writeln!(f, "for ({initializer} ; {condition} ; {increment})")?;

				if f.alternate() {
					write!(f, "{body:#width$}")
				} else {
					write!(f, "{body:width$}")
				}
			}

			Statement::If { if_location, condition, then_branch, else_branch } => {
				if f.alternate() {
					write!(f, "{if_location}{PAD:width$} ")?;
				}

				writeln!(f, "if {condition}")?;

				if f.alternate() {
					write!(f, "{then_branch:#width$}")?;
				} else {
					write!(f, "{then_branch:width$}")?;
				}

				if let Some((else_location, else_branch)) = else_branch {
					writeln!(f)?;
					if f.alternate() {
						write!(f, "{else_location}{PAD:width$} ")?;
					}

					writeln!(f, "else")?;

					if f.alternate() {
						write!(f, "{else_branch:#width$}")?;
					} else {
						write!(f, "else\n{else_branch:width$}")?;
					}
				}
				Ok(())
			}

			Statement::Print { location, expression } => {
				if f.alternate() {
					write!(f, "{location} {PAD:width$}")?;
				}
				write!(f, "(print {expression})")
			}

			Statement::VariableDeclaration { location, name, initializer } => {
				let Token { text, .. } = name.deref();

				if f.alternate() {
					write!(f, "{location} {PAD:width$}")?;
				}
				match initializer {
					Some(expression) => {
						write!(f, "(vardecl {text} = {expression})")
					}

					None => write!(f, "(vardecl {text})"),
				}
			}

			Statement::While { location, condition, body } => {
				if f.alternate() {
					write!(f, "{location} {PAD:width$}")?;
				}

				writeln!(f, "while {condition}")?;

				if f.alternate() {
					write!(f, "{body:#width$}")
				} else {
					write!(f, "{body:width$}")
				}
			}
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
				let Token { text, .. } = operator.deref();
				write!(f, "({text} {left} {right})")
			}

			Expression::Call { callee, arguments, .. } => {
				write!(f, "(call {callee} ( ")?;
				for argument in arguments {
					write!(f, "{argument} ")?;
				}
				write!(f, ") )")
			}

			Expression::Literal(token) => {
				let Token { text, .. } = token.deref();
				write!(f, "{text}")
			}

			Expression::Grouping(expression) => write!(f, "(group {expression})"),

			Expression::Unary { operator, right } => {
				let Token { text, .. } = operator.deref();
				write!(f, "({text} {right})")
			}

			Expression::Variable(token) => {
				let Token { text, .. } = token.deref();
				write!(f, "(var {text})")
			}
		}
	}
}

/////////////////////////////////////////////////////////////////////////////
// Locatable
/////////////////////////////////////////////////////////////////////////////

impl Locatable for ParseError {
	fn location(&self) -> &Location {
		match self {
			ParseError::UnexpectedToken { actual, .. } => actual.location(),
			ParseError::InvalidAssignment { location, .. } => location,
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

impl Locatable for Expression {
	fn location(&self) -> &Location {
		match self {
			Expression::Assignment { name, .. } => name.location(),
			Expression::Binary { left, .. } => left.location(),
			Expression::Call { callee, .. } => callee.location(),
			Expression::Grouping(expression) => expression.location(),
			Expression::Literal(token) => token.location(),
			Expression::Unary { operator, .. } => operator.location(),
			Expression::Variable(token) => token.location(),
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
