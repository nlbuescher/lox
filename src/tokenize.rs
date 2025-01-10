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
	String { value: String, text: String },
	Number { value: f64, text: String },

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
		}
	}
}

impl Display for Token {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Token::String { value, .. } => write!(f, "{} {} {}", self.name(), self.text(), value),
			Token::Number { value, .. } => write!(f, "{} {} {}", self.name(), self.text(), value),
			_ => write!(f, "{} {}", self.name(), self.text()),
		}
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
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "[{}:{}]", self.line, self.column)
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct AnnotatedToken {
	pub token: Token,
	pub location: Location,
}

impl Display for AnnotatedToken {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{} {}", self.location, self.token)
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
	UnexpectedCharacter,
	UnterminatedString,
}

impl Display for Error {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Error::UnexpectedCharacter => write!(f, "Unexpected character"),
			Error::UnterminatedString => write!(f, "Unterminated string"),
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct AnnotatedError {
	error: Error,
	text: String,
	location: Location,
}

impl Display for AnnotatedError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{} {}: {}", self.location, self.error, self.text)
	}
}

pub struct Tokenizer {
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
			start_location: Location::new(1, 1),
			current_location: Location::new(1, 1),
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
						((self.current_location.column + 3) & !0b11usize) + 1;
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

	fn annotate_error(&mut self, error: Error) -> AnnotatedError {
		let result = AnnotatedError {
			error,
			text: self.buffer.clone(),
			location: self.start_location.clone(),
		};

		self.reset();

		result
	}

	fn get_string_token(&mut self) -> Result<AnnotatedToken, AnnotatedError> {
		while self.peek() != Some('"') && self.peek() != None {
			self.advance(true);
		}

		if self.peek() == None {
			Err(self.annotate_error(Error::UnterminatedString))
		}
		else {
			// consume the closing quote
			self.advance(true);

			Ok(self.annotate_token(Token::String {
				value: self.buffer[1..self.buffer.len() - 1].to_string(),
				text: self.buffer.clone(),
			}))
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
			value: self.buffer.parse::<f64>().unwrap(),
			text: self.buffer.clone(),
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
	type Item = Result<AnnotatedToken, AnnotatedError>;

	fn next(&mut self) -> Option<Self::Item> {
		self.advance(false).and_then(|c| match c {
			'(' => Some(Ok(self.annotate_token(Token::LeftParen))),
			')' => Some(Ok(self.annotate_token(Token::RightParen))),
			'{' => Some(Ok(self.annotate_token(Token::LeftBrace))),
			'}' => Some(Ok(self.annotate_token(Token::RightBrace))),
			',' => Some(Ok(self.annotate_token(Token::Comma))),
			'.' => Some(Ok(self.annotate_token(Token::Dot))),
			'-' => Some(Ok(self.annotate_token(Token::Minus))),
			'+' => Some(Ok(self.annotate_token(Token::Plus))),
			';' => Some(Ok(self.annotate_token(Token::Semicolon))),
			'*' => Some(Ok(self.annotate_token(Token::Star))),

			'!' => {
				let token = if self.advance_if('=') { Token::BangEqual } else { Token::Bang };
				Some(Ok(self.annotate_token(token)))
			}

			'=' => {
				let token = if self.advance_if('=') { Token::EqualEqual } else { Token::Equal };
				Some(Ok(self.annotate_token(token)))
			}

			'>' => {
				let token = if self.advance_if('=') { Token::GreaterEqual } else { Token::Greater };
				Some(Ok(self.annotate_token(token)))
			}

			'<' => {
				let token = if self.advance_if('=') { Token::LessEqual } else { Token::Less };
				Some(Ok(self.annotate_token(token)))
			}

			'/' => {
				if self.advance_if('/') {
					while self.peek() != Some('\n') && self.peek() != None {
						self.advance(true);
					}
					Some(Ok(self.annotate_token(Token::Comment { text: self.buffer.clone() })))
				}
				else {
					Some(Ok(self.annotate_token(Token::Slash)))
				}
			}

			'"' => Some(self.get_string_token()),

			_ if c.is_ascii_whitespace() => self.next(),

			_ if c.is_ascii_digit() => Some(Ok(self.get_number_token())),

			_ if c.is_ascii_alphabetic() || c == '_' => Some(Ok(self.get_identifier_token())),

			_ => Some(Err(self.annotate_error(Error::UnexpectedCharacter))),
		})
	}
}

mod tests {
	use super::*;

	#[test]
	pub fn empty_source() {
		let input = "";
		let expected: Vec<Result<AnnotatedToken, AnnotatedError>> = Vec::new();

		let actual = Tokenizer::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn parentheses() {
		let input = "(()";
		let expected = vec![
			Ok(AnnotatedToken { token: Token::LeftParen, location: Location::new(1, 1) }),
			Ok(AnnotatedToken { token: Token::LeftParen, location: Location::new(1, 2) }),
			Ok(AnnotatedToken { token: Token::RightParen, location: Location::new(1, 3) }),
		];

		let actual = Tokenizer::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn whitespace() {
		let input = "\t(  \t(";
		let expected =
			vec![
				Ok(AnnotatedToken { token: Token::LeftParen, location: Location::new(1, 5) }),
				Ok(AnnotatedToken { token: Token::LeftParen, location: Location::new(1, 9) }),
			];

		let actual = Tokenizer::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn newlines() {
		let input = "\r\n\r\n  (";
		let expected =
			vec![Ok(AnnotatedToken { token: Token::LeftParen, location: Location::new(3, 3) })];

		let actual = Tokenizer::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn braces() {
		let input = "{{}}";
		let expected = vec![
			Ok(AnnotatedToken { token: Token::LeftBrace, location: Location::new(1, 1) }),
			Ok(AnnotatedToken { token: Token::LeftBrace, location: Location::new(1, 2) }),
			Ok(AnnotatedToken { token: Token::RightBrace, location: Location::new(1, 3) }),
			Ok(AnnotatedToken { token: Token::RightBrace, location: Location::new(1, 4) }),
		];

		let actual = Tokenizer::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn other_single_character_tokens() {
		let input = "({+.*,- ;})";
		let expected = vec![
			Ok(AnnotatedToken { token: Token::LeftParen, location: Location::new(1, 1) }),
			Ok(AnnotatedToken { token: Token::LeftBrace, location: Location::new(1, 2) }),
			Ok(AnnotatedToken { token: Token::Plus, location: Location::new(1, 3) }),
			Ok(AnnotatedToken { token: Token::Dot, location: Location::new(1, 4) }),
			Ok(AnnotatedToken { token: Token::Star, location: Location::new(1, 5) }),
			Ok(AnnotatedToken { token: Token::Comma, location: Location::new(1, 6) }),
			Ok(AnnotatedToken { token: Token::Minus, location: Location::new(1, 7) }),
			Ok(AnnotatedToken { token: Token::Semicolon, location: Location::new(1, 9) }),
			Ok(AnnotatedToken { token: Token::RightBrace, location: Location::new(1, 10) }),
			Ok(AnnotatedToken { token: Token::RightParen, location: Location::new(1, 11) }),
		];

		let actual = Tokenizer::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn unknown_character() {
		let input = ".,$(#";
		let expected = vec![
			Ok(AnnotatedToken { token: Token::Dot, location: Location::new(1, 1) }),
			Ok(AnnotatedToken { token: Token::Comma, location: Location::new(1, 2) }),
			Err(AnnotatedError {
				error: Error::UnexpectedCharacter,
				text: String::from("$"),
				location: Location::new(1, 3),
			}),
			Ok(AnnotatedToken { token: Token::LeftParen, location: Location::new(1, 4) }),
			Err(AnnotatedError {
				error: Error::UnexpectedCharacter,
				text: String::from("#"),
				location: Location::new(1, 5),
			}),
		];

		let actual = Tokenizer::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn operators() {
		let input = "! != = == > >= < <=";
		let expected = vec![
			Ok(AnnotatedToken { token: Token::Bang, location: Location::new(1, 1) }),
			Ok(AnnotatedToken { token: Token::BangEqual, location: Location::new(1, 3) }),
			Ok(AnnotatedToken { token: Token::Equal, location: Location::new(1, 6) }),
			Ok(AnnotatedToken { token: Token::EqualEqual, location: Location::new(1, 8) }),
			Ok(AnnotatedToken { token: Token::Greater, location: Location::new(1, 11) }),
			Ok(AnnotatedToken { token: Token::GreaterEqual, location: Location::new(1, 13) }),
			Ok(AnnotatedToken { token: Token::Less, location: Location::new(1, 16) }),
			Ok(AnnotatedToken { token: Token::LessEqual, location: Location::new(1, 18) }),
		];

		let actual = Tokenizer::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn comments() {
		let input = "// comment\n/";
		let expected = vec![
			Ok(AnnotatedToken {
				token: Token::Comment { text: String::from("// comment") },
				location: Location::new(1, 1),
			}),
			Ok(AnnotatedToken { token: Token::Slash, location: Location::new(2, 1) }),
		];

		let actual = Tokenizer::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn strings() {
		let input = "\"test\"\"test";
		let expected = vec![
			Ok(AnnotatedToken {
				token: Token::String {
					text: String::from("\"test\""),
					value: String::from("test"),
				},
				location: Location::new(1, 1),
			}),
			Err(AnnotatedError {
				error: Error::UnterminatedString,
				text: String::from("\"test"),
				location: Location::new(1, 7),
			}),
		];

		let actual = Tokenizer::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn numbers() {
		let input = "420.69\n.5\n5.";
		let expected = vec![
			Ok(AnnotatedToken {
				token: Token::Number { text: String::from("420.69"), value: 420.69 },
				location: Location::new(1, 1),
			}),
			Ok(AnnotatedToken { token: Token::Dot, location: Location::new(2, 1) }),
			Ok(AnnotatedToken {
				token: Token::Number { text: String::from("5"), value: 5. },
				location: Location::new(2, 2),
			}),
			Ok(AnnotatedToken {
				token: Token::Number { text: String::from("5"), value: 5. },
				location: Location::new(3, 1),
			}),
			Ok(AnnotatedToken { token: Token::Dot, location: Location::new(3, 2) }),
		];

		let actual = Tokenizer::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn identifiers() {
		let input = "orchid";
		let expected = vec![Ok(AnnotatedToken {
			token: Token::Identifier { text: String::from("orchid") },
			location: Location::new(1, 1),
		})];

		let actual = Tokenizer::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn keywords() {
		let input = "and class else false for fun if nil or print return super this true var while";
		let expected = vec![
			Ok(AnnotatedToken { token: Token::And, location: Location::new(1, 1) }),
			Ok(AnnotatedToken { token: Token::Class, location: Location::new(1, 5) }),
			Ok(AnnotatedToken { token: Token::Else, location: Location::new(1, 11) }),
			Ok(AnnotatedToken { token: Token::False, location: Location::new(1, 16) }),
			Ok(AnnotatedToken { token: Token::For, location: Location::new(1, 22) }),
			Ok(AnnotatedToken { token: Token::Fun, location: Location::new(1, 26) }),
			Ok(AnnotatedToken { token: Token::If, location: Location::new(1, 30) }),
			Ok(AnnotatedToken { token: Token::Nil, location: Location::new(1, 33) }),
			Ok(AnnotatedToken { token: Token::Or, location: Location::new(1, 37) }),
			Ok(AnnotatedToken { token: Token::Print, location: Location::new(1, 40) }),
			Ok(AnnotatedToken { token: Token::Return, location: Location::new(1, 46) }),
			Ok(AnnotatedToken { token: Token::Super, location: Location::new(1, 53) }),
			Ok(AnnotatedToken { token: Token::This, location: Location::new(1, 59) }),
			Ok(AnnotatedToken { token: Token::True, location: Location::new(1, 64) }),
			Ok(AnnotatedToken { token: Token::Var, location: Location::new(1, 69) }),
			Ok(AnnotatedToken { token: Token::While, location: Location::new(1, 73) }),
		];

		let actual = Tokenizer::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}
}
