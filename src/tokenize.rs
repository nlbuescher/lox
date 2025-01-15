use std::fmt::{Display, Formatter};
use std::str::Chars;

use crate::location::Location;
use crate::value::Value;

pub type Result = std::result::Result<Token, InvalidToken>;

#[derive(Debug, Clone, PartialEq)]
pub enum ErrorKind {
	UnknownChar,
	UnterminatedString,
}

impl ErrorKind {
	fn as_str(&self) -> &'static str {
		use ErrorKind::*;
		match self {
			UnknownChar => "unknown character",
			UnterminatedString => "unterminated string",
		}
	}
}

impl Display for ErrorKind {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		f.write_str(self.as_str())
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct InvalidToken {
	pub kind: ErrorKind,
	pub text: String,
	pub location: Location,
}

impl InvalidToken {
	pub fn new(kind: ErrorKind, text: impl Into<String>, location: Location) -> InvalidToken {
		InvalidToken { kind, text: text.into(), location }
	}
}

impl Display for InvalidToken {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "{} {}", self.kind, self.text)
	}
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
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

impl TokenKind {
	pub fn as_str(&self) -> &'static str {
		use TokenKind::*;
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
			String => "STRING",

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
	}
}

impl Display for TokenKind {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		f.write_str(self.as_str())
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
	pub kind: TokenKind,
	pub text: String,
	pub value: Option<Value>,
	pub location: Location,
}

impl Token {
	pub fn with_text(kind: TokenKind, text: impl Into<String>, location: Location) -> Self {
		Token { location, kind, text: text.into(), value: None }
	}

	pub fn with_value(
		kind: TokenKind,
		text: impl Into<String>,
		value: Value,
		location: Location,
	) -> Self {
		Token { location, kind, text: text.into(), value: Some(value) }
	}
}

impl Display for Token {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		match self {
			Token { value: Some(value), .. } => {
				write!(f, "{} {} {:?}", self.kind, self.text, value)
			}
			Token { value: None, .. } => {
				write!(f, "{} {}", self.kind, self.text)
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

	fn get_error(&mut self, error_type: ErrorKind) -> InvalidToken {
		let result =
			InvalidToken::new(error_type, self.buffer.as_str(), self.start_location.clone());

		self.reset_buffer();

		result
	}

	fn get_token(&mut self, token_type: TokenKind) -> Token {
		let result =
			Token::with_text(token_type, self.buffer.as_str(), self.start_location.clone());

		self.reset_buffer();

		result
	}

	fn get_value_token(&mut self, token_type: TokenKind, value: Value) -> Token {
		let result =
			Token::with_value(token_type, self.buffer.as_str(), value, self.start_location.clone());

		self.reset_buffer();

		result
	}

	fn get_string_token(&mut self) -> Result {
		while self.peek() != Some(&'"') && self.peek() != None {
			self.advance(true);
		}

		if self.peek() == None {
			Err(self.get_error(ErrorKind::UnterminatedString))
		}
		else {
			// consume the closing quote
			self.advance(true);

			Ok(self.get_value_token(
				TokenKind::String,
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

		self.get_value_token(TokenKind::Number, Value::Number(self.buffer.parse::<f64>().unwrap()))
	}

	fn get_identifier_token(&mut self) -> Token {
		while self.peek().take_if(|it| **it == '_' || it.is_ascii_alphanumeric()) != None {
			self.advance(false);
		}

		use TokenKind::*;

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
	type Item = Result;

	fn next(&mut self) -> Option<Self::Item> {
		if !self.has_next {
			return None;
		}

		let next = self.advance(false);

		use TokenKind::*;

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

			_ => Some(Err(self.get_error(ErrorKind::UnknownChar))),
		}
	}
}

#[cfg(test)]
mod tests {
	use std::string::String;

	use ErrorKind::*;
	use TokenKind::*;

	use super::*;

	#[test]
	pub fn empty_source() {
		let input = "";
		let expected = vec![Ok(Token::with_text(EndOfFile, "", Location::new(1, 1)))];

		let actual = Tokens::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn parentheses() {
		let input = "(()";
		let expected = vec![
			Ok(Token::with_text(LeftParen, "(", Location::new(1, 1))),
			Ok(Token::with_text(LeftParen, "(", Location::new(1, 2))),
			Ok(Token::with_text(RightParen, ")", Location::new(1, 3))),
			Ok(Token::with_text(EndOfFile, "", Location::new(1, 4))),
		];

		let actual = Tokens::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn whitespace() {
		let input = "\t(  \t(";
		let expected = vec![
			Ok(Token::with_text(LeftParen, "(", Location::new(1, 5))),
			Ok(Token::with_text(LeftParen, "(", Location::new(1, 9))),
			Ok(Token::with_text(EndOfFile, "", Location::new(1, 10))),
		];

		let actual = Tokens::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn newlines() {
		let input = "\r\n\r\n  (\n";
		let expected = vec![
			Ok(Token::with_text(LeftParen, "(", Location::new(3, 3))),
			Ok(Token::with_text(EndOfFile, "", Location::new(4, 1))),
		];

		let actual = Tokens::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn braces() {
		let input = "{{}}";
		let expected = vec![
			Ok(Token::with_text(LeftBrace, "{", Location::new(1, 1))),
			Ok(Token::with_text(LeftBrace, "{", Location::new(1, 2))),
			Ok(Token::with_text(RightBrace, "}", Location::new(1, 3))),
			Ok(Token::with_text(RightBrace, "}", Location::new(1, 4))),
			Ok(Token::with_text(EndOfFile, "", Location::new(1, 5))),
		];

		let actual = Tokens::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn other_single_character_tokens() {
		let input = "({+.*,- ;})";
		let expected = vec![
			Ok(Token::with_text(LeftParen, "(", Location::new(1, 1))),
			Ok(Token::with_text(LeftBrace, "{", Location::new(1, 2))),
			Ok(Token::with_text(Plus, "+", Location::new(1, 3))),
			Ok(Token::with_text(Dot, ".", Location::new(1, 4))),
			Ok(Token::with_text(Star, "*", Location::new(1, 5))),
			Ok(Token::with_text(Comma, ",", Location::new(1, 6))),
			Ok(Token::with_text(Minus, "-", Location::new(1, 7))),
			Ok(Token::with_text(Semicolon, ";", Location::new(1, 9))),
			Ok(Token::with_text(RightBrace, "}", Location::new(1, 10))),
			Ok(Token::with_text(RightParen, ")", Location::new(1, 11))),
			Ok(Token::with_text(EndOfFile, "", Location::new(1, 12))),
		];

		let actual = Tokens::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn unknown_character() {
		let input = ".,$(#";
		let expected = vec![
			Ok(Token::with_text(Dot, ".", Location::new(1, 1))),
			Ok(Token::with_text(Comma, ",", Location::new(1, 2))),
			Err(InvalidToken::new(UnknownChar, "$", Location::new(1, 3))),
			Ok(Token::with_text(LeftParen, "(", Location::new(1, 4))),
			Err(InvalidToken::new(UnknownChar, "#", Location::new(1, 5))),
			Ok(Token::with_text(EndOfFile, "", Location::new(1, 6))),
		];

		let actual = Tokens::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn operators() {
		let input = "! != = == > >= < <=";
		let expected = vec![
			Ok(Token::with_text(Bang, "!", Location::new(1, 1))),
			Ok(Token::with_text(BangEqual, "!=", Location::new(1, 3))),
			Ok(Token::with_text(Equal, "=", Location::new(1, 6))),
			Ok(Token::with_text(EqualEqual, "==", Location::new(1, 8))),
			Ok(Token::with_text(Greater, ">", Location::new(1, 11))),
			Ok(Token::with_text(GreaterEqual, ">=", Location::new(1, 13))),
			Ok(Token::with_text(Less, "<", Location::new(1, 16))),
			Ok(Token::with_text(LessEqual, "<=", Location::new(1, 18))),
			Ok(Token::with_text(EndOfFile, "", Location::new(1, 20))),
		];

		let actual = Tokens::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn comments() {
		let input = "// comment\n/";
		let expected = vec![
			Ok(Token::with_text(Comment, String::from("// comment"), Location::new(1, 1))),
			Ok(Token::with_text(Slash, "/", Location::new(2, 1))),
			Ok(Token::with_text(EndOfFile, "", Location::new(2, 2))),
		];

		let actual = Tokens::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn strings() {
		let input = "\"test\"\"test";
		let expected = vec![
			Ok(Token::with_value(
				TokenKind::String,
				"\"test\"",
				Value::String("test".into()),
				Location::new(1, 1),
			)),
			Err(InvalidToken::new(UnterminatedString, "\"test", Location::new(1, 7))),
			Ok(Token::with_text(EndOfFile, "", Location::new(1, 12))),
		];

		let actual = Tokens::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn numbers() {
		let input = "420.69\n.5\n5.";
		let expected = vec![
			Ok(Token::with_value(Number, "420.69", Value::Number(420.69), Location::new(1, 1))),
			Ok(Token::with_text(Dot, ".", Location::new(2, 1))),
			Ok(Token::with_value(Number, "5", Value::Number(5.), Location::new(2, 2))),
			Ok(Token::with_value(Number, "5", Value::Number(5.), Location::new(3, 1))),
			Ok(Token::with_text(Dot, ".", Location::new(3, 2))),
			Ok(Token::with_text(EndOfFile, "", Location::new(3, 3))),
		];

		let actual = Tokens::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn identifiers() {
		let input = "orchid";
		let expected = vec![
			Ok(Token::with_text(Identifier, String::from("orchid"), Location::new(1, 1))),
			Ok(Token::with_text(EndOfFile, "", Location::new(1, 7))),
		];

		let actual = Tokens::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn keywords() {
		let input = "and class else false for fun if nil or print return super this true var while";
		let expected = vec![
			Ok(Token::with_text(And, "and", Location::new(1, 1))),
			Ok(Token::with_text(Class, "class", Location::new(1, 5))),
			Ok(Token::with_text(Else, "else", Location::new(1, 11))),
			Ok(Token::with_text(False, "false", Location::new(1, 16))),
			Ok(Token::with_text(For, "for", Location::new(1, 22))),
			Ok(Token::with_text(Fun, "fun", Location::new(1, 26))),
			Ok(Token::with_text(If, "if", Location::new(1, 30))),
			Ok(Token::with_text(Nil, "nil", Location::new(1, 33))),
			Ok(Token::with_text(Or, "or", Location::new(1, 37))),
			Ok(Token::with_text(Print, "print", Location::new(1, 40))),
			Ok(Token::with_text(Return, "return", Location::new(1, 46))),
			Ok(Token::with_text(Super, "super", Location::new(1, 53))),
			Ok(Token::with_text(This, "this", Location::new(1, 59))),
			Ok(Token::with_text(True, "true", Location::new(1, 64))),
			Ok(Token::with_text(Var, "var", Location::new(1, 69))),
			Ok(Token::with_text(While, "while", Location::new(1, 73))),
			Ok(Token::with_text(EndOfFile, "", Location::new(1, 78))),
		];

		let actual = Tokens::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}
}
