use std::fmt::{Display, Formatter};

use crate::location::{Locatable, Location};
use crate::value::Value;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
	location: Location,
	pub kind: TokenKind,
	pub text: String,
	pub value: Option<Value>,
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

impl Token {
	pub(crate) fn with_text(location: Location, kind: TokenKind, text: impl Into<String>) -> Self {
		Token { location, kind, text: text.into(), value: None }
	}

	pub(crate) fn with_value(
		location: Location,
		kind: TokenKind,
		text: impl Into<String>,
		value: Value,
	) -> Self {
		Token { location, kind, text: text.into(), value: Some(value) }
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

impl Locatable for Token {
	fn location(&self) -> &Location {
		&self.location
	}
}

impl Display for Token {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		let Token { location, kind, text, .. } = self;

		if f.alternate() {
			write!(f, "{location} ")?;
		}

		write!(f, "{kind}")?;

		if !text.is_empty() {
			write!(f, " '{text}'")?;
		}

		Ok(())
	}
}

impl Display for TokenKind {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		f.write_str(self.as_str())
	}
}
