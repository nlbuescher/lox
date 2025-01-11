use std::fmt::{Display, Formatter};
use std::str::Chars;
use std::string::String;

use crate::tokenize::ErrorType::*;
use crate::tokenize::TokenType::*;

#[derive(Debug, Clone, PartialEq)]
pub enum ErrorType {
	UnexpectedCharacter,
	UnterminatedString,
}

impl Display for ErrorType {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		match self {
			UnexpectedCharacter => write!(f, "unexpected character"),
			UnterminatedString => write!(f, "unterminated string"),
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct Error {
	pub location: Location,
	pub error_type: ErrorType,
	pub text: String,
}

impl Error {
	pub fn new(location: Location, error: ErrorType, text: String) -> Error {
		Error { error_type: error, text: text.into(), location }
	}
}

impl Display for Error {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "{} {}: {}", self.location, self.error_type, self.text)
	}
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType {
	// Single-character tokens
	LeftParen,
	RightParen,
	LeftBrace,
	RightBrace,
	Comma,
	Dot,
	Minus,
	Plus,
	Semicolon,
	Slash,
	Star,

	// One- or two-character tokens
	Bang,
	BangEqual,
	Equal,
	EqualEqual,
	Greater,
	GreaterEqual,
	Less,
	LessEqual,

	// Literals
	Identifier,
	Number,
	String,

	// Keywords
	And,
	Class,
	Else,
	False,
	Fun,
	For,
	If,
	Nil,
	Or,
	Print,
	Return,
	Super,
	This,
	True,
	Var,
	While,

	// Other
	Comment,
	EndOfFile,
}

impl Display for TokenType {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(
			f,
			"{}",
			match self {
				LeftParen => "LEFT_PAREN",
				RightParen => "RIGHT_PAREN",
				LeftBrace => "LEFT_BRACE",
				RightBrace => "RIGHT_BRACE",
				Comma => "COMMA",
				Dot => "DOT",
				Minus => "MINUS",
				Plus => "PLUS",
				Semicolon => "SEMICOLON",
				Slash => "SLASH",
				Star => "STAR",

				Bang => "BANG",
				BangEqual => "BANG_EQUAL",
				Equal => "EQUAL",
				EqualEqual => "EQUAL_EQUAL",
				Greater => "GREATER",
				GreaterEqual => "GREATER_EQUAL",
				Less => "LESS",
				LessEqual => "LESS_EQUAL",

				Identifier => "IDENTIFIER",
				Number => "NUMBER",
				TokenType::String => "STRING",

				And => "AND",
				Class => "CLASS",
				Else => "ELSE",
				False => "FALSE",
				Fun => "FUN",
				For => "FOR",
				If => "IF",
				Nil => "NIL",
				Or => "OR",
				Print => "PRINT",
				Return => "RETURN",
				Super => "SUPER",
				This => "THIS",
				True => "TRUE",
				Var => "VAR",
				While => "WHILE",

				Comment => "COMMENT",
				EndOfFile => "EOF",
			}
		)
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct Location {
	pub line: usize,
	pub column: usize,
}

impl Location {
	pub fn new(line: usize, column: usize) -> Location {
		Location { line, column }
	}
}

impl Display for Location {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "[{}:{}]", self.line, self.column)
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
	Number(f64),
	String(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
	pub location: Location,
	pub token_type: TokenType,
	pub text: String,
	pub value: Option<Value>,
}

impl Token {
	pub fn with_text(location: Location, token_type: TokenType, text: impl Into<String>) -> Self {
		Token { location, token_type, text: text.into(), value: None }
	}

	pub fn with_value(
		location: Location,
		token_type: TokenType,
		text: impl Into<String>,
		value: Value,
	) -> Self {
		Token { location, token_type, text: text.into(), value: Some(value) }
	}
}

impl Display for Token {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		match self {
			Token { value: Some(value), .. } => {
				write!(f, "{} {} {} {:?}", self.location, self.token_type, self.text, value)
			}
			Token { value: None, .. } => {
				write!(f, "{} {} {}", self.location, self.token_type, self.text)
			}
		}
	}
}

#[derive(Debug, Clone)]
pub struct Tokens<'a> {
	source: Chars<'a>,
	has_next: bool,
	current: Option<char>,
	next: Option<char>,
	current_location: Location,
	start_location: Location,
	buffer: String,
}

impl<'a> Tokens<'a> {
	pub fn new(source: &'a str) -> Self {
		let mut chars = source.chars();

		Tokens {
			current: chars.next(),
			next: chars.next(),
			source: chars,
			has_next: true,
			buffer: String::new(),
			start_location: Location::new(1, 1),
			current_location: Location::new(1, 1),
		}
	}

	fn peek(&mut self) -> Option<&char> {
		self.current.as_ref()
	}

	fn peek_next(&mut self) -> Option<&char> {
		self.next.as_ref()
	}

	fn advance(&mut self, include_whitespace: bool) -> Option<char> {
		let result = self.current;

		self.current = self.next;
		self.next = self.source.next();

		if let Some(c) = result {
			// add char to buffer
			self.buffer.push(c);

			// update location
			match c {
				'\n' => {
					self.current_location.line += 1;
					self.current_location.column = 1;
				}
				'\t' => {
					self.current_location.column =
						((self.current_location.column + 3) & !0b11usize) + 1;
				}
				_ => self.current_location.column += 1,
			}

			if !include_whitespace && c.is_whitespace() {
				self.start_location = self.current_location.clone();
				self.buffer.clear();
			}
		}

		result
	}

	/// returns: whether the tokenizer advanced
	fn advance_if(&mut self, expected: char) -> bool {
		if self.peek() != Some(&expected) {
			false
		}
		else {
			self.advance(true);
			true
		}
	}

	pub fn reset_buffer(&mut self) {
		self.buffer.clear();
		self.start_location = self.current_location.clone();
	}

	fn get_error(&mut self, error_type: ErrorType) -> Error {
		let result = Error {
			location: self.start_location.clone(),
			error_type,
			text: self.buffer.as_str().into(),
		};

		self.reset_buffer();

		result
	}

	fn get_token(&mut self, token_type: TokenType) -> Token {
		let result =
			Token::with_text(self.start_location.clone(), token_type, self.buffer.as_str());

		self.reset_buffer();

		result
	}

	fn get_value_token(&mut self, token_type: TokenType, value: Value) -> Token {
		let result =
			Token::with_value(self.start_location.clone(), token_type, self.buffer.as_str(), value);

		self.reset_buffer();

		result
	}

	fn get_string_token(&mut self) -> Result<Token, Error> {
		while self.peek() != Some(&'"') && self.peek() != None {
			self.advance(true);
		}

		if self.peek() == None {
			Err(self.get_error(UnterminatedString))
		}
		else {
			// consume the closing quote
			self.advance(true);

			Ok(self.get_value_token(
				TokenType::String,
				Value::String(self.buffer[1..self.buffer.len() - 1].to_string()),
			))
		}
	}

	fn get_number_token(&mut self) -> Token {
		while self.peek().take_if(|it| it.is_ascii_digit()) != None {
			self.advance(false);
		}

		if self.peek() == Some(&'.') && self.peek_next().take_if(|it| it.is_ascii_digit()) != None {
			// Consume the decimal point
			self.advance(false);

			while self.peek().take_if(|it| it.is_ascii_digit()) != None {
				self.advance(false);
			}
		}

		self.get_value_token(Number, Value::Number(self.buffer.parse::<f64>().unwrap()))
	}

	fn get_identifier_token(&mut self) -> Token {
		while self.peek().take_if(|it| **it == '_' || it.is_ascii_alphanumeric()) != None {
			self.advance(false);
		}

		match self.buffer.as_str() {
			"and" => self.get_token(And),
			"class" => self.get_token(Class),
			"else" => self.get_token(Else),
			"false" => self.get_token(False),
			"for" => self.get_token(For),
			"fun" => self.get_token(Fun),
			"if" => self.get_token(If),
			"nil" => self.get_token(Nil),
			"or" => self.get_token(Or),
			"print" => self.get_token(Print),
			"return" => self.get_token(Return),
			"super" => self.get_token(Super),
			"this" => self.get_token(This),
			"true" => self.get_token(True),
			"var" => self.get_token(Var),
			"while" => self.get_token(While),
			_ => self.get_token(Identifier),
		}
	}
}

impl Iterator for Tokens<'_> {
	type Item = Result<Token, Error>;

	fn next(&mut self) -> Option<Self::Item> {
		if !self.has_next {
			return None;
		}

		let next = self.advance(false);

		if next.is_none() {
			self.has_next = false;
			return Some(Ok(self.get_token(EndOfFile)));
		}

		match next.unwrap() {
			'(' => Some(Ok(self.get_token(LeftParen))),
			')' => Some(Ok(self.get_token(RightParen))),
			'{' => Some(Ok(self.get_token(LeftBrace))),
			'}' => Some(Ok(self.get_token(RightBrace))),
			',' => Some(Ok(self.get_token(Comma))),
			'.' => Some(Ok(self.get_token(Dot))),
			'-' => Some(Ok(self.get_token(Minus))),
			'+' => Some(Ok(self.get_token(Plus))),
			';' => Some(Ok(self.get_token(Semicolon))),
			'*' => Some(Ok(self.get_token(Star))),

			'!' => {
				let token_type = if self.advance_if('=') { BangEqual } else { Bang };
				Some(Ok(self.get_token(token_type)))
			}

			'=' => {
				let token_type = if self.advance_if('=') { EqualEqual } else { Equal };
				Some(Ok(self.get_token(token_type)))
			}

			'>' => {
				let token_type = if self.advance_if('=') { GreaterEqual } else { Greater };
				Some(Ok(self.get_token(token_type)))
			}

			'<' => {
				let token_type = if self.advance_if('=') { LessEqual } else { Less };
				Some(Ok(self.get_token(token_type)))
			}

			'/' => {
				if self.advance_if('/') {
					while self.peek() != Some(&'\n') && self.peek() != None {
						self.advance(true);
					}
					Some(Ok(self.get_token(Comment)))
				}
				else {
					Some(Ok(self.get_token(Slash)))
				}
			}

			'"' => Some(self.get_string_token()),

			c if c.is_ascii_whitespace() => self.next(),

			c if c.is_ascii_digit() => Some(Ok(self.get_number_token())),

			c if c.is_ascii_alphabetic() || c == '_' => Some(Ok(self.get_identifier_token())),

			_ => Some(Err(self.get_error(UnexpectedCharacter))),
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	pub fn empty_source() {
		let input = "";
		let expected = vec![Ok(Token::with_text(Location::new(1, 1), EndOfFile, ""))];

		let actual = Tokens::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn parentheses() {
		let input = "(()";
		let expected = vec![
			Ok(Token::with_text(Location::new(1, 1), LeftParen, "(")),
			Ok(Token::with_text(Location::new(1, 2), LeftParen, "(")),
			Ok(Token::with_text(Location::new(1, 3), RightParen, ")")),
			Ok(Token::with_text(Location::new(1, 4), EndOfFile, "")),
		];

		let actual = Tokens::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn whitespace() {
		let input = "\t(  \t(";
		let expected = vec![
			Ok(Token::with_text(Location::new(1, 5), LeftParen, "(")),
			Ok(Token::with_text(Location::new(1, 9), LeftParen, "(")),
			Ok(Token::with_text(Location::new(1, 10), EndOfFile, "")),
		];

		let actual = Tokens::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn newlines() {
		let input = "\r\n\r\n  (\n";
		let expected = vec![
			Ok(Token::with_text(Location::new(3, 3), LeftParen, "(")),
			Ok(Token::with_text(Location::new(4, 1), EndOfFile, "")),
		];

		let actual = Tokens::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn braces() {
		let input = "{{}}";
		let expected = vec![
			Ok(Token::with_text(Location::new(1, 1), LeftBrace, "{")),
			Ok(Token::with_text(Location::new(1, 2), LeftBrace, "{")),
			Ok(Token::with_text(Location::new(1, 3), RightBrace, "}")),
			Ok(Token::with_text(Location::new(1, 4), RightBrace, "}")),
			Ok(Token::with_text(Location::new(1, 5), EndOfFile, "")),
		];

		let actual = Tokens::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn other_single_character_tokens() {
		let input = "({+.*,- ;})";
		let expected = vec![
			Ok(Token::with_text(Location::new(1, 1), LeftParen, "(")),
			Ok(Token::with_text(Location::new(1, 2), LeftBrace, "{")),
			Ok(Token::with_text(Location::new(1, 3), Plus, "+")),
			Ok(Token::with_text(Location::new(1, 4), Dot, ".")),
			Ok(Token::with_text(Location::new(1, 5), Star, "*")),
			Ok(Token::with_text(Location::new(1, 6), Comma, ",")),
			Ok(Token::with_text(Location::new(1, 7), Minus, "-")),
			Ok(Token::with_text(Location::new(1, 9), Semicolon, ";")),
			Ok(Token::with_text(Location::new(1, 10), RightBrace, "}")),
			Ok(Token::with_text(Location::new(1, 11), RightParen, ")")),
			Ok(Token::with_text(Location::new(1, 12), EndOfFile, "")),
		];

		let actual = Tokens::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn unknown_character() {
		let input = ".,$(#";
		let expected = vec![
			Ok(Token::with_text(Location::new(1, 1), Dot, ".")),
			Ok(Token::with_text(Location::new(1, 2), Comma, ",")),
			Err(Error::new(Location::new(1, 3), UnexpectedCharacter, String::from("$"))),
			Ok(Token::with_text(Location::new(1, 4), LeftParen, "(")),
			Err(Error::new(Location::new(1, 5), UnexpectedCharacter, String::from("#"))),
			Ok(Token::with_text(Location::new(1, 6), EndOfFile, "")),
		];

		let actual = Tokens::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn operators() {
		let input = "! != = == > >= < <=";
		let expected = vec![
			Ok(Token::with_text(Location::new(1, 1), Bang, "!")),
			Ok(Token::with_text(Location::new(1, 3), BangEqual, "!=")),
			Ok(Token::with_text(Location::new(1, 6), Equal, "=")),
			Ok(Token::with_text(Location::new(1, 8), EqualEqual, "==")),
			Ok(Token::with_text(Location::new(1, 11), Greater, ">")),
			Ok(Token::with_text(Location::new(1, 13), GreaterEqual, ">=")),
			Ok(Token::with_text(Location::new(1, 16), Less, "<")),
			Ok(Token::with_text(Location::new(1, 18), LessEqual, "<=")),
			Ok(Token::with_text(Location::new(1, 20), EndOfFile, "")),
		];

		let actual = Tokens::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn comments() {
		let input = "// comment\n/";
		let expected = vec![
			Ok(Token::with_text(Location::new(1, 1), Comment, String::from("// comment"))),
			Ok(Token::with_text(Location::new(2, 1), Slash, "/")),
			Ok(Token::with_text(Location::new(2, 2), EndOfFile, "")),
		];

		let actual = Tokens::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn strings() {
		let input = "\"test\"\"test";
		let expected = vec![
			Ok(Token::with_value(
				Location::new(1, 1),
				TokenType::String,
				"\"test\"",
				Value::String("test".into()),
			)),
			Err(Error::new(Location::new(1, 7), UnterminatedString, String::from("\"test"))),
			Ok(Token::with_text(Location::new(1, 12), EndOfFile, "")),
		];

		let actual = Tokens::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn numbers() {
		let input = "420.69\n.5\n5.";
		let expected = vec![
			Ok(Token::with_value(Location::new(1, 1), Number, "420.69", Value::Number(420.69))),
			Ok(Token::with_text(Location::new(2, 1), Dot, ".")),
			Ok(Token::with_value(Location::new(2, 2), Number, "5", Value::Number(5.))),
			Ok(Token::with_value(Location::new(3, 1), Number, "5", Value::Number(5.))),
			Ok(Token::with_text(Location::new(3, 2), Dot, ".")),
			Ok(Token::with_text(Location::new(3, 3), EndOfFile, "")),
		];

		let actual = Tokens::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn identifiers() {
		let input = "orchid";
		let expected = vec![
			Ok(Token::with_text(Location::new(1, 1), Identifier, String::from("orchid"))),
			Ok(Token::with_text(Location::new(1, 7), EndOfFile, "")),
		];

		let actual = Tokens::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn keywords() {
		let input = "and class else false for fun if nil or print return super this true var while";
		let expected = vec![
			Ok(Token::with_text(Location::new(1, 1), And, "and")),
			Ok(Token::with_text(Location::new(1, 5), Class, "class")),
			Ok(Token::with_text(Location::new(1, 11), Else, "else")),
			Ok(Token::with_text(Location::new(1, 16), False, "false")),
			Ok(Token::with_text(Location::new(1, 22), For, "for")),
			Ok(Token::with_text(Location::new(1, 26), Fun, "fun")),
			Ok(Token::with_text(Location::new(1, 30), If, "if")),
			Ok(Token::with_text(Location::new(1, 33), Nil, "nil")),
			Ok(Token::with_text(Location::new(1, 37), Or, "or")),
			Ok(Token::with_text(Location::new(1, 40), Print, "print")),
			Ok(Token::with_text(Location::new(1, 46), Return, "return")),
			Ok(Token::with_text(Location::new(1, 53), Super, "super")),
			Ok(Token::with_text(Location::new(1, 59), This, "this")),
			Ok(Token::with_text(Location::new(1, 64), True, "true")),
			Ok(Token::with_text(Location::new(1, 69), Var, "var")),
			Ok(Token::with_text(Location::new(1, 73), While, "while")),
			Ok(Token::with_text(Location::new(1, 78), EndOfFile, "")),
		];

		let actual = Tokens::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}
}
