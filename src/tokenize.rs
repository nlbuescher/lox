use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
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
	Identifier { text: String },
	String { text: String, value: String },
	Number { text: String, value: f64 },

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
	Comment { text: String },

	// Error
	UnknownChar { text: String },
	UnterminatedString { text: String },
}

impl Token {
	fn name(&self) -> &str {
		match self {
			Token::LeftParen { .. } => "LEFT_PAREN",
			Token::RightParen { .. } => "RIGHT_PAREN",
			Token::LeftBrace { .. } => "LEFT_BRACE",
			Token::RightBrace { .. } => "RIGHT_BRACE",
			Token::Comma { .. } => "COMMA",
			Token::Dot { .. } => "DOT",
			Token::Minus { .. } => "MINUS",
			Token::Plus { .. } => "PLUS",
			Token::Semicolon { .. } => "SEMICOLON",
			Token::Slash { .. } => "SLASH",
			Token::Star { .. } => "STAR",

			Token::Bang { .. } => "BANG",
			Token::BangEqual { .. } => "BANG_EQUAL",
			Token::Equal { .. } => "EQUAL",
			Token::EqualEqual { .. } => "EQUAL_EQUAL",
			Token::Greater { .. } => "GREATER",
			Token::GreaterEqual { .. } => "GREATER_EQUAL",
			Token::Less { .. } => "LESS",
			Token::LessEqual { .. } => "LESS_EQUAL",

			Token::Identifier { .. } => "IDENTIFIER",
			Token::String { .. } => "STRING",
			Token::Number { .. } => "NUMBER",

			Token::And { .. } => "AND",
			Token::Class { .. } => "CLASS",
			Token::Else { .. } => "ELSE",
			Token::False { .. } => "FALSE",
			Token::Fun { .. } => "FUN",
			Token::For { .. } => "FOR",
			Token::If { .. } => "IF",
			Token::Nil { .. } => "NIL",
			Token::Or { .. } => "OR",
			Token::Print { .. } => "PRINT",
			Token::Return { .. } => "RETURN",
			Token::Super { .. } => "SUPER",
			Token::This { .. } => "THIS",
			Token::True { .. } => "TRUE",
			Token::Var { .. } => "VAR",
			Token::While { .. } => "WHILE",

			Token::Comment { .. } => "COMMENT",

			Token::UnknownChar { .. } => "UNKNOWN_CHAR",
			Token::UnterminatedString { .. } => "UNTERMINATED_STRING",
		}
	}

	pub fn text(&self) -> &str {
		match self {
			Token::LeftParen { .. } => "(",
			Token::RightParen { .. } => ")",
			Token::LeftBrace { .. } => "{",
			Token::RightBrace { .. } => "}",
			Token::Comma { .. } => "",
			Token::Dot { .. } => ".",
			Token::Minus { .. } => "-",
			Token::Plus { .. } => "+",
			Token::Semicolon { .. } => ";",
			Token::Slash { .. } => "/",
			Token::Star { .. } => "*",

			Token::Bang { .. } => "!",
			Token::BangEqual { .. } => "!=",
			Token::Equal { .. } => "=",
			Token::EqualEqual { .. } => "==",
			Token::Greater { .. } => ">",
			Token::GreaterEqual { .. } => ">=",
			Token::Less { .. } => "<",
			Token::LessEqual { .. } => "<=",

			Token::Identifier { text, .. } => text,
			Token::String { text, .. } => text,
			Token::Number { text, .. } => text,

			Token::And { .. } => "and",
			Token::Class { .. } => "class",
			Token::Else { .. } => "else",
			Token::False { .. } => "false",
			Token::Fun { .. } => "fun",
			Token::For { .. } => "for",
			Token::If { .. } => "if",
			Token::Nil { .. } => "nil",
			Token::Or { .. } => "or",
			Token::Print { .. } => "print",
			Token::Return { .. } => "return",
			Token::Super { .. } => "super",
			Token::This { .. } => "this",
			Token::True { .. } => "true",
			Token::Var { .. } => "var",
			Token::While { .. } => "while",

			Token::Comment { text, .. } => text,

			Token::UnknownChar { text, .. } => text,
			Token::UnterminatedString { text, .. } => text,
		}
	}
}

impl Display for Token {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Token::String { value, .. } => write!(f, "{} {} {}", self.name(), self.text(), value),
			Token::Number { value, .. } => write!(f, "{} {} {}", self.name(), self.text(), value),
			_ => write!(f, "{} {} null", self.name(), self.text()),
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct Location {
	pub line: usize,
	pub column: usize,
}

impl Display for Location {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "[{}:{}]", self.line, self.column)
	}
}

pub struct AnnotatedToken {
	pub token: Token,
	pub location: Location,
}

impl Display for AnnotatedToken {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{} {}", self.location, self.token)
	}
}

struct Tokenizer {
	source: Vec<char>,
	position: usize,
	buffer: String,
	start_location: Location,
	current_location: Location,
}

impl Tokenizer {
	pub fn new(source: &str) -> Self {
		Tokenizer {
			source: source.chars().collect(),
			position: 0,
			buffer: String::new(),
			start_location: Location { line: 1, column: 1 },
			current_location: Location { line: 1, column: 1 },
		}
	}

	fn peek(&mut self) -> Option<char> {
		if self.position < self.source.len() {
			Some(self.source[self.position])
		}
		else {
			None
		}
	}

	fn peek_next(&mut self) -> Option<char> {
		if self.position + 1 < self.source.len() {
			Some(self.source[self.position + 1])
		}
		else {
			None
		}
	}

	fn advance(&mut self, include_whitespace: bool) -> Option<char> {
		let next;

		if self.position < self.source.len() {
			next = Some(self.source[self.position]);
			self.position += 1;
		}
		else {
			next = None;
		};

		if let Some(c) = next {
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
						((self.current_location.column + 4) & !0b11usize) + 1;
				}
				_ => self.current_location.column += 1,
			}

			if !include_whitespace && c.is_whitespace() {
				self.start_location = self.current_location.clone();
				self.buffer.clear();
			}
		}

		next
	}

	/// returns: whether the tokenizer advanced
	fn advance_if(&mut self, expected: char) -> bool {
		match self.peek() {
			None => false,
			Some(c) => {
				if c != expected {
					false
				}
				else {
					self.advance(false);
					true
				}
			}
		}
	}

	pub fn reset(&mut self) {
		self.buffer.clear();
		self.start_location = self.current_location.clone();
	}

	fn annotate_token(&mut self, token: Token) -> AnnotatedToken {
		let result = AnnotatedToken { token, location: self.start_location.clone() };

		self.reset();

		result
	}

	fn get_string_token(&mut self) -> AnnotatedToken {
		while self.peek() != Some('"') && self.peek() != None {
			self.advance(true);
		}

		if self.peek() == None {
			self.annotate_token(Token::UnterminatedString { text: self.buffer.clone() })
		}
		else {
			// consume the closing quote
			self.advance(true);

			self.annotate_token(Token::String {
				text: self.buffer.clone(),
				value: self.buffer[1..self.buffer.len() - 1].to_string(),
			})
		}
	}

	fn get_number_token(&mut self) -> AnnotatedToken {
		while self.peek().take_if(|it| it.is_ascii_digit()) != None {
			self.advance(false);
		}

		if self.peek() == Some('.') && self.peek_next().take_if(|it| it.is_ascii_digit()) != None {
			// Consume the decimal point
			self.advance(false);

			while self.peek().take_if(|it| it.is_ascii_digit()) != None {
				self.advance(false);
			}
		}

		self.annotate_token(Token::Number {
			text: self.buffer.clone(),
			value: self.buffer.parse::<f64>().unwrap(),
		})
	}

	fn get_identifier_token(&mut self) -> AnnotatedToken {
		while self.peek().take_if(|&mut it| it == '_' || it.is_ascii_alphanumeric()) != None {
			self.advance(false);
		}

		match self.buffer.as_str() {
			"and" => self.annotate_token(Token::And),
			"class" => self.annotate_token(Token::Class),
			"else" => self.annotate_token(Token::Else),
			"false" => self.annotate_token(Token::False),
			"for" => self.annotate_token(Token::For),
			"fun" => self.annotate_token(Token::Fun),
			"if" => self.annotate_token(Token::If),
			"nil" => self.annotate_token(Token::Nil),
			"or" => self.annotate_token(Token::Or),
			"print" => self.annotate_token(Token::Print),
			"return" => self.annotate_token(Token::Return),
			"super" => self.annotate_token(Token::Super),
			"this" => self.annotate_token(Token::This),
			"true" => self.annotate_token(Token::True),
			"var" => self.annotate_token(Token::Var),
			"while" => self.annotate_token(Token::While),
			_ => self.annotate_token(Token::Identifier { text: self.buffer.clone() }),
		}
	}
}

impl Iterator for Tokenizer {
	type Item = AnnotatedToken;

	fn next(&mut self) -> Option<Self::Item> {
		self.advance(false).and_then(|c| match c {
			'(' => Some(self.annotate_token(Token::LeftParen)),
			')' => Some(self.annotate_token(Token::RightParen)),
			'{' => Some(self.annotate_token(Token::LeftBrace)),
			'}' => Some(self.annotate_token(Token::RightBrace)),
			',' => Some(self.annotate_token(Token::Comma)),
			'.' => Some(self.annotate_token(Token::Dot)),
			'-' => Some(self.annotate_token(Token::Minus)),
			'+' => Some(self.annotate_token(Token::Plus)),
			';' => Some(self.annotate_token(Token::Semicolon)),
			'*' => Some(self.annotate_token(Token::Star)),

			'!' => {
				let token = if self.advance_if('=') { Token::BangEqual } else { Token::Bang };
				Some(self.annotate_token(token))
			}

			'=' => {
				let token = if self.advance_if('=') { Token::EqualEqual } else { Token::Equal };
				Some(self.annotate_token(token))
			}

			'>' => {
				let token = if self.advance_if('=') { Token::GreaterEqual } else { Token::Greater };
				Some(self.annotate_token(token))
			}

			'<' => {
				let token = if self.advance_if('=') { Token::LessEqual } else { Token::Less };
				Some(self.annotate_token(token))
			}

			'/' => {
				if self.advance_if('/') {
					while self.peek() != Some('\n') && self.peek() != None {
						self.advance(true);
					}
					Some(self.annotate_token(Token::Comment { text: self.buffer.clone() }))
				}
				else {
					Some(self.annotate_token(Token::Slash))
				}
			}

			'"' => Some(self.get_string_token()),

			_ if c.is_ascii_whitespace() => self.next(),

			_ if c.is_ascii_digit() => Some(self.get_number_token()),

			_ if c == '_' || c.is_ascii_alphabetic() => Some(self.get_identifier_token()),

			_ => Some(self.annotate_token(Token::UnknownChar { text: self.buffer.clone() })),
		})
	}
}

pub fn tokenize(source: &str) -> Vec<AnnotatedToken> {
	Tokenizer::new(source).collect::<Vec<AnnotatedToken>>()
}

mod tests {
	use super::*;

	#[test]
	pub fn empty_source() {
		let input = "";
		let expected = Vec::<Token>::new();

		let actual = tokenize(input).into_iter().map(|it| it.token).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn parentheses() {
		let input = "(()";
		let expected = vec![Token::LeftParen, Token::LeftParen, Token::RightParen];

		let actual = tokenize(input).into_iter().map(|it| it.token).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn whitespace() {
		let input = " \t(";
		let expected = vec![Token::LeftParen];

		let actual = tokenize(input).into_iter().map(|it| it.token).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn newlines() {
		let input = "\r\n\r\n  (";
		let expected = vec![Token::LeftParen];

		let actual = tokenize(input).into_iter().map(|it| it.token).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn braces() {
		let input = "{{}}";
		let expected =
			vec![Token::LeftBrace, Token::LeftBrace, Token::RightBrace, Token::RightBrace];

		let actual = tokenize(input).into_iter().map(|it| it.token).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn other_single_character_tokens() {
		let input = "({+.*,- ;})";
		let expected = vec![
			Token::LeftParen,
			Token::LeftBrace,
			Token::Plus,
			Token::Dot,
			Token::Star,
			Token::Comma,
			Token::Minus,
			Token::Semicolon,
			Token::RightBrace,
			Token::RightParen,
		];

		let actual = tokenize(input).into_iter().map(|it| it.token).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn unknown_character() {
		let input = ".,$(#";
		let expected = vec![
			Token::Dot,
			Token::Comma,
			Token::UnknownChar { text: String::from("$") },
			Token::LeftParen,
			Token::UnknownChar { text: String::from("#") },
		];

		let actual = tokenize(input).into_iter().map(|it| it.token).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn operators() {
		let input = "! != = == > >= < <=";
		let expected = vec![
			Token::Bang,
			Token::BangEqual,
			Token::Equal,
			Token::EqualEqual,
			Token::Greater,
			Token::GreaterEqual,
			Token::Less,
			Token::LessEqual,
		];

		let actual = tokenize(input).into_iter().map(|it| it.token).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn comments() {
		let input = "// comment\n/";
		let expected = vec![Token::Comment { text: String::from("// comment") }, Token::Slash];

		let actual = tokenize(input).into_iter().map(|it| it.token).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn strings() {
		let input = "\"test\"\"test";
		let expected = vec![
			Token::String { text: String::from("\"test\""), value: String::from("test") },
			Token::UnterminatedString { text: String::from("\"test") },
		];

		let actual = tokenize(input).into_iter().map(|it| it.token).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn numbers() {
		let input = "420.69\n.5\n5.";
		let expected = vec![
			Token::Number { text: String::from("420.69"), value: 420.69 },
			Token::Dot,
			Token::Number { text: String::from("5"), value: 5. },
			Token::Number { text: String::from("5"), value: 5. },
			Token::Dot,
		];

		let actual = tokenize(input).into_iter().map(|it| it.token).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn identifiers() {
		let input = "orchid";
		let expected = vec![Token::Identifier { text: String::from("orchid") }];

		let actual = tokenize(input).into_iter().map(|it| it.token).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn keywords() {
		let input = "and class else false for fun if nil or print return super this true var while";
		let expected = vec![
			Token::And,
			Token::Class,
			Token::Else,
			Token::False,
			Token::For,
			Token::Fun,
			Token::If,
			Token::Nil,
			Token::Or,
			Token::Print,
			Token::Return,
			Token::Super,
			Token::This,
			Token::True,
			Token::Var,
			Token::While,
		];

		let actual = tokenize(input).into_iter().map(|it| it.token).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}
}
