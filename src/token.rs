use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub struct Location {
	pub line: usize,
	pub column: usize,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType {
	Unknown,

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
	String,
	Number,

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
}

impl Display for TokenType {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(
			f,
			"{}",
			match self {
				TokenType::LeftParen => "LEFT_PAREN",
				TokenType::RightParen => "RIGHT_PAREN",
				TokenType::LeftBrace => "LEFT_BRACE",
				TokenType::RightBrace => "RIGHT_BRACE",
				TokenType::Comma => "COMMA",
				TokenType::Dot => "DOT",
				TokenType::Minus => "MINUS",
				TokenType::Plus => "PLUS",
				TokenType::Semicolon => "SEMICOLON",
				TokenType::Slash => "SLASH",
				TokenType::Star => "STAR",

				TokenType::Bang => "BANG",
				TokenType::BangEqual => "BANG_EQUAL",
				TokenType::Equal => "EQUAL",
				TokenType::EqualEqual => "EQUAL_EQUAL",
				TokenType::Greater => "GREATER",
				TokenType::GreaterEqual => "GREATER_EQUAL",
				TokenType::Less => "LESS",
				TokenType::LessEqual => "LESS_EQUAL",

				TokenType::Identifier => "IDENTIFIER",
				TokenType::String => "STRING",
				TokenType::Number => "NUMBER",

				TokenType::And => "AND",
				TokenType::Class => "CLASS",
				TokenType::Else => "ELSE",
				TokenType::False => "FALSE",
				TokenType::Fun => "FUN",
				TokenType::For => "FOR",
				TokenType::If => "IF",
				TokenType::Nil => "NIL",
				TokenType::Or => "OR",
				TokenType::Print => "PRINT",
				TokenType::Return => "RETURN",
				TokenType::Super => "SUPER",
				TokenType::This => "THIS",
				TokenType::True => "TRUE",
				TokenType::Var => "VAR",
				TokenType::While => "WHILE",

				TokenType::Unknown => "ERROR",
			}
		)
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
	pub token_type: TokenType,
	pub value: String,
	pub location: Location,
}

impl Token {
	pub fn new(token_type: TokenType, value: String, location: Location) -> Token {
		Token {
			token_type,
			value,
			location,
		}
	}
}

impl Display for Token {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let token_type = self.token_type;
		let value = match self.token_type {
			TokenType::Unknown => format!(
				"[{}:{}]: Unexpected character: {}",
				self.value, self.location.line, self.location.column
			),
			_ => self.value.clone(),
		};
		write!(f, "{token_type} {value}")
	}
}
