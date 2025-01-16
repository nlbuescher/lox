use std::fmt::{Display, Formatter};
use std::str::Chars;

use crate::location::Location;
use crate::value::Value;

pub type Result = std::result::Result<Token, Error>;

#[derive(Clone)]
pub struct Tokens<'a> {
	source: Chars<'a>,
	has_next: bool,
	current: Option<char>,
	next: Option<char>,
	current_location: Location,
	start_location: Location,
	buffer: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
	data: Box<TokenData>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Error {
	data: Box<TokenData>,
}

#[derive(Debug, Clone, PartialEq)]
struct TokenData {
	location: Location,
	kind: std::result::Result<TokenKind, ErrorKind>,
	text: String,
	value: Option<Value>,
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ErrorKind {
	UnknownChar,
	UnterminatedString,
}

/////////////////////////////////////////////////////////////////////////////
// Type implementations
/////////////////////////////////////////////////////////////////////////////

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
}

impl Token {
	fn with_text(location: Location, kind: TokenKind, text: impl Into<String>) -> Self {
		Token {
			data: Box::new(TokenData { location, kind: Ok(kind), text: text.into(), value: None }),
		}
	}

	fn with_value(
		location: Location,
		kind: TokenKind,
		text: impl Into<String>,
		value: Value,
	) -> Self {
		Token {
			data: Box::new(TokenData {
				location,
				kind: Ok(kind),
				text: text.into(),
				value: Some(value),
			}),
		}
	}

	pub fn location(&self) -> &Location {
		&self.data.location
	}

	pub fn kind(&self) -> TokenKind {
		self.data.kind.ok().unwrap()
	}

	pub fn text(&self) -> &str {
		&self.data.text
	}

	pub fn value(&self) -> Option<&Value> {
		self.data.value.as_ref()
	}
}

impl Error {
	fn new(location: Location, kind: ErrorKind, text: impl Into<String>) -> Error {
		Error {
			data: Box::new(TokenData { location, kind: Err(kind), text: text.into(), value: None }),
		}
	}

	pub fn location(&self) -> &Location {
		&self.data.location
	}

	pub fn kind(&self) -> ErrorKind {
		self.data.kind.err().unwrap()
	}

	pub fn text(&self) -> &str {
		&self.data.text
	}
}

impl TokenKind {
	fn as_str(&self) -> &'static str {
		match self {
			TokenKind::LeftParen => "LEFT_PAREN",
			TokenKind::RightParen => "RIGHT_PAREN",
			TokenKind::LeftBrace => "LEFT_BRACE",
			TokenKind::RightBrace => "RIGHT_BRACE",
			TokenKind::Comma => "COMMA",
			TokenKind::Dot => "DOT",
			TokenKind::Minus => "MINUS",
			TokenKind::Plus => "PLUS",
			TokenKind::Semicolon => "SEMICOLON",
			TokenKind::Slash => "SLASH",
			TokenKind::Star => "STAR",

			TokenKind::Bang => "BANG",
			TokenKind::BangEqual => "BANG_EQUAL",
			TokenKind::Equal => "EQUAL",
			TokenKind::EqualEqual => "EQUAL_EQUAL",
			TokenKind::Greater => "GREATER",
			TokenKind::GreaterEqual => "GREATER_EQUAL",
			TokenKind::Less => "LESS",
			TokenKind::LessEqual => "LESS_EQUAL",

			TokenKind::Identifier => "IDENTIFIER",
			TokenKind::Number => "NUMBER",
			TokenKind::String => "STRING",

			TokenKind::And => "AND",
			TokenKind::Class => "CLASS",
			TokenKind::Else => "ELSE",
			TokenKind::False => "FALSE",
			TokenKind::Fun => "FUN",
			TokenKind::For => "FOR",
			TokenKind::If => "IF",
			TokenKind::Nil => "NIL",
			TokenKind::Or => "OR",
			TokenKind::Print => "PRINT",
			TokenKind::Return => "RETURN",
			TokenKind::Super => "SUPER",
			TokenKind::This => "THIS",
			TokenKind::True => "TRUE",
			TokenKind::Var => "VAR",
			TokenKind::While => "WHILE",

			TokenKind::Comment => "COMMENT",
			TokenKind::EndOfFile => "EOF",
		}
	}
}

impl ErrorKind {
	fn as_str(&self) -> &'static str {
		match self {
			ErrorKind::UnknownChar => "unknown character",
			ErrorKind::UnterminatedString => "unterminated string",
		}
	}
}

/////////////////////////////////////////////////////////////////////////////
// Display
/////////////////////////////////////////////////////////////////////////////

impl Display for Token {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		match self.value() {
			Some(value) => {
				write!(f, "{kind} '{text}' {value:?}", kind = self.kind(), text = self.text())
			}
			None => write!(f, "{kind} '{text}'", kind = self.kind(), text = self.text()),
		}
	}
}

impl Display for Error {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		write!(f, "{} '{}'", self.kind(), self.text())
	}
}

impl Display for TokenKind {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		f.write_str(self.as_str())
	}
}

impl Display for ErrorKind {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		f.write_str(self.as_str())
	}
}

/////////////////////////////////////////////////////////////////////////////
// Tokens implementation
/////////////////////////////////////////////////////////////////////////////

impl<'a> Tokens<'a> {
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

	fn get_error(&mut self, error_type: ErrorKind) -> Error {
		let result = Error::new(self.start_location.clone(), error_type, self.buffer.as_str());

		self.reset_buffer();

		result
	}

	fn get_token(&mut self, token_type: TokenKind) -> Token {
		let result =
			Token::with_text(self.start_location.clone(), token_type, self.buffer.as_str());

		self.reset_buffer();

		result
	}

	fn get_value_token(&mut self, token_type: TokenKind, value: Value) -> Token {
		let result =
			Token::with_value(self.start_location.clone(), token_type, self.buffer.as_str(), value);

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

		match self.buffer.as_str() {
			"and" => self.get_token(TokenKind::And),
			"class" => self.get_token(TokenKind::Class),
			"else" => self.get_token(TokenKind::Else),
			"false" => self.get_token(TokenKind::False),
			"for" => self.get_token(TokenKind::For),
			"fun" => self.get_token(TokenKind::Fun),
			"if" => self.get_token(TokenKind::If),
			"nil" => self.get_token(TokenKind::Nil),
			"or" => self.get_token(TokenKind::Or),
			"print" => self.get_token(TokenKind::Print),
			"return" => self.get_token(TokenKind::Return),
			"super" => self.get_token(TokenKind::Super),
			"this" => self.get_token(TokenKind::This),
			"true" => self.get_token(TokenKind::True),
			"var" => self.get_token(TokenKind::Var),
			"while" => self.get_token(TokenKind::While),
			_ => self.get_token(TokenKind::Identifier),
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

		if next.is_none() {
			self.has_next = false;
			return Some(Ok(self.get_token(TokenKind::EndOfFile)));
		}

		match next.unwrap() {
			'(' => Some(Ok(self.get_token(TokenKind::LeftParen))),
			')' => Some(Ok(self.get_token(TokenKind::RightParen))),
			'{' => Some(Ok(self.get_token(TokenKind::LeftBrace))),
			'}' => Some(Ok(self.get_token(TokenKind::RightBrace))),
			',' => Some(Ok(self.get_token(TokenKind::Comma))),
			'.' => Some(Ok(self.get_token(TokenKind::Dot))),
			'-' => Some(Ok(self.get_token(TokenKind::Minus))),
			'+' => Some(Ok(self.get_token(TokenKind::Plus))),
			';' => Some(Ok(self.get_token(TokenKind::Semicolon))),
			'*' => Some(Ok(self.get_token(TokenKind::Star))),

			'!' => {
				let token_type =
					if self.advance_if('=') { TokenKind::BangEqual } else { TokenKind::Bang };
				Some(Ok(self.get_token(token_type)))
			}

			'=' => {
				let token_type =
					if self.advance_if('=') { TokenKind::EqualEqual } else { TokenKind::Equal };
				Some(Ok(self.get_token(token_type)))
			}

			'>' => {
				let token_type =
					if self.advance_if('=') { TokenKind::GreaterEqual } else { TokenKind::Greater };
				Some(Ok(self.get_token(token_type)))
			}

			'<' => {
				let token_type =
					if self.advance_if('=') { TokenKind::LessEqual } else { TokenKind::Less };
				Some(Ok(self.get_token(token_type)))
			}

			'/' => {
				if self.advance_if('/') {
					while self.peek() != Some(&'\n') && self.peek() != None {
						self.advance(true);
					}
					Some(Ok(self.get_token(TokenKind::Comment)))
				}
				else {
					Some(Ok(self.get_token(TokenKind::Slash)))
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

	use super::*;

	#[test]
	pub fn empty_source() {
		let input = "";
		let expected = vec![Ok(Token::with_text(Location::new(1, 1), TokenKind::EndOfFile, ""))];

		let actual = Tokens::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn parentheses() {
		let input = "(()";
		let expected = vec![
			Ok(Token::with_text(Location::new(1, 1), TokenKind::LeftParen, "(")),
			Ok(Token::with_text(Location::new(1, 2), TokenKind::LeftParen, "(")),
			Ok(Token::with_text(Location::new(1, 3), TokenKind::RightParen, ")")),
			Ok(Token::with_text(Location::new(1, 4), TokenKind::EndOfFile, "")),
		];

		let actual = Tokens::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn whitespace() {
		let input = "\t(  \t(";
		let expected = vec![
			Ok(Token::with_text(Location::new(1, 5), TokenKind::LeftParen, "(")),
			Ok(Token::with_text(Location::new(1, 9), TokenKind::LeftParen, "(")),
			Ok(Token::with_text(Location::new(1, 10), TokenKind::EndOfFile, "")),
		];

		let actual = Tokens::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn newlines() {
		let input = "\r\n\r\n  (\n";
		let expected = vec![
			Ok(Token::with_text(Location::new(3, 3), TokenKind::LeftParen, "(")),
			Ok(Token::with_text(Location::new(4, 1), TokenKind::EndOfFile, "")),
		];

		let actual = Tokens::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn braces() {
		let input = "{{}}";
		let expected = vec![
			Ok(Token::with_text(Location::new(1, 1), TokenKind::LeftBrace, "{")),
			Ok(Token::with_text(Location::new(1, 2), TokenKind::LeftBrace, "{")),
			Ok(Token::with_text(Location::new(1, 3), TokenKind::RightBrace, "}")),
			Ok(Token::with_text(Location::new(1, 4), TokenKind::RightBrace, "}")),
			Ok(Token::with_text(Location::new(1, 5), TokenKind::EndOfFile, "")),
		];

		let actual = Tokens::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn other_single_character_tokens() {
		let input = "({+.*,- ;})";
		let expected = vec![
			Ok(Token::with_text(Location::new(1, 1), TokenKind::LeftParen, "(")),
			Ok(Token::with_text(Location::new(1, 2), TokenKind::LeftBrace, "{")),
			Ok(Token::with_text(Location::new(1, 3), TokenKind::Plus, "+")),
			Ok(Token::with_text(Location::new(1, 4), TokenKind::Dot, ".")),
			Ok(Token::with_text(Location::new(1, 5), TokenKind::Star, "*")),
			Ok(Token::with_text(Location::new(1, 6), TokenKind::Comma, ",")),
			Ok(Token::with_text(Location::new(1, 7), TokenKind::Minus, "-")),
			Ok(Token::with_text(Location::new(1, 9), TokenKind::Semicolon, ";")),
			Ok(Token::with_text(Location::new(1, 10), TokenKind::RightBrace, "}")),
			Ok(Token::with_text(Location::new(1, 11), TokenKind::RightParen, ")")),
			Ok(Token::with_text(Location::new(1, 12), TokenKind::EndOfFile, "")),
		];

		let actual = Tokens::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn unknown_character() {
		let input = ".,$(#";
		let expected = vec![
			Ok(Token::with_text(Location::new(1, 1), TokenKind::Dot, ".")),
			Ok(Token::with_text(Location::new(1, 2), TokenKind::Comma, ",")),
			Err(Error::new(Location::new(1, 3), ErrorKind::UnknownChar, "$")),
			Ok(Token::with_text(Location::new(1, 4), TokenKind::LeftParen, "(")),
			Err(Error::new(Location::new(1, 5), ErrorKind::UnknownChar, "#")),
			Ok(Token::with_text(Location::new(1, 6), TokenKind::EndOfFile, "")),
		];

		let actual = Tokens::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn operators() {
		let input = "! != = == > >= < <=";
		let expected = vec![
			Ok(Token::with_text(Location::new(1, 1), TokenKind::Bang, "!")),
			Ok(Token::with_text(Location::new(1, 3), TokenKind::BangEqual, "!=")),
			Ok(Token::with_text(Location::new(1, 6), TokenKind::Equal, "=")),
			Ok(Token::with_text(Location::new(1, 8), TokenKind::EqualEqual, "==")),
			Ok(Token::with_text(Location::new(1, 11), TokenKind::Greater, ">")),
			Ok(Token::with_text(Location::new(1, 13), TokenKind::GreaterEqual, ">=")),
			Ok(Token::with_text(Location::new(1, 16), TokenKind::Less, "<")),
			Ok(Token::with_text(Location::new(1, 18), TokenKind::LessEqual, "<=")),
			Ok(Token::with_text(Location::new(1, 20), TokenKind::EndOfFile, "")),
		];

		let actual = Tokens::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn comments() {
		let input = "// comment\n/";
		let expected = vec![
			Ok(Token::with_text(
				Location::new(1, 1),
				TokenKind::Comment,
				String::from("// comment"),
			)),
			Ok(Token::with_text(Location::new(2, 1), TokenKind::Slash, "/")),
			Ok(Token::with_text(Location::new(2, 2), TokenKind::EndOfFile, "")),
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
				TokenKind::String,
				"\"test\"",
				Value::String("test".into()),
			)),
			Err(Error::new(Location::new(1, 7), ErrorKind::UnterminatedString, "\"test")),
			Ok(Token::with_text(Location::new(1, 12), TokenKind::EndOfFile, "")),
		];

		let actual = Tokens::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn numbers() {
		let input = "420.69\n.5\n5.";
		let expected = vec![
			Ok(Token::with_value(
				Location::new(1, 1),
				TokenKind::Number,
				"420.69",
				Value::Number(420.69),
			)),
			Ok(Token::with_text(Location::new(2, 1), TokenKind::Dot, ".")),
			Ok(Token::with_value(Location::new(2, 2), TokenKind::Number, "5", Value::Number(5.))),
			Ok(Token::with_value(Location::new(3, 1), TokenKind::Number, "5", Value::Number(5.))),
			Ok(Token::with_text(Location::new(3, 2), TokenKind::Dot, ".")),
			Ok(Token::with_text(Location::new(3, 3), TokenKind::EndOfFile, "")),
		];

		let actual = Tokens::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn identifiers() {
		let input = "orchid";
		let expected = vec![
			Ok(Token::with_text(
				Location::new(1, 1),
				TokenKind::Identifier,
				String::from("orchid"),
			)),
			Ok(Token::with_text(Location::new(1, 7), TokenKind::EndOfFile, "")),
		];

		let actual = Tokens::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn keywords() {
		let input = "and class else false for fun if nil or print return super this true var while";
		let expected = vec![
			Ok(Token::with_text(Location::new(1, 1), TokenKind::And, "and")),
			Ok(Token::with_text(Location::new(1, 5), TokenKind::Class, "class")),
			Ok(Token::with_text(Location::new(1, 11), TokenKind::Else, "else")),
			Ok(Token::with_text(Location::new(1, 16), TokenKind::False, "false")),
			Ok(Token::with_text(Location::new(1, 22), TokenKind::For, "for")),
			Ok(Token::with_text(Location::new(1, 26), TokenKind::Fun, "fun")),
			Ok(Token::with_text(Location::new(1, 30), TokenKind::If, "if")),
			Ok(Token::with_text(Location::new(1, 33), TokenKind::Nil, "nil")),
			Ok(Token::with_text(Location::new(1, 37), TokenKind::Or, "or")),
			Ok(Token::with_text(Location::new(1, 40), TokenKind::Print, "print")),
			Ok(Token::with_text(Location::new(1, 46), TokenKind::Return, "return")),
			Ok(Token::with_text(Location::new(1, 53), TokenKind::Super, "super")),
			Ok(Token::with_text(Location::new(1, 59), TokenKind::This, "this")),
			Ok(Token::with_text(Location::new(1, 64), TokenKind::True, "true")),
			Ok(Token::with_text(Location::new(1, 69), TokenKind::Var, "var")),
			Ok(Token::with_text(Location::new(1, 73), TokenKind::While, "while")),
			Ok(Token::with_text(Location::new(1, 78), TokenKind::EndOfFile, "")),
		];

		let actual = Tokens::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}
}
