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
	EndOfFile,
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
			Token::EndOfFile => "EOF",
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
			Token::EndOfFile => "",
		}
	}
}

impl Display for Token {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Token::EndOfFile => write!(f, "{}", self.name()),
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

impl AnnotatedToken {
	pub fn new(location: Location, token: Token) -> AnnotatedToken {
		AnnotatedToken { token, location }
	}
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
			Error::UnexpectedCharacter => write!(f, "ERROR: Unexpected character"),
			Error::UnterminatedString => write!(f, "ERROR: Unterminated string"),
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct AnnotatedError {
	location: Location,
	error: Error,
	text: String,
}

impl AnnotatedError {
	pub fn new(location: Location, error: Error, text: impl Into<String>) -> AnnotatedError {
		AnnotatedError { error, text: text.into(), location }
	}
}

impl Display for AnnotatedError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{} {}: {}", self.location, self.error, self.text)
	}
}

pub struct Tokenizer {
	source: Vec<char>,
	position: usize,
	has_next: bool,
	buffer: String,
	start_location: Location,
	current_location: Location,
}

impl Tokenizer {
	pub fn new(source: &str) -> Self {
		Tokenizer {
			source: source.chars().collect(),
			position: 0,
			has_next: true,
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
		if !self.has_next {
			return None;
		}

		let next = self.advance(false);

		if next.is_none() {
			self.has_next = false;
			return Some(Ok(self.annotate_token(Token::EndOfFile)));
		}

		match next.unwrap() {
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

			c if c.is_ascii_whitespace() => self.next(),

			c if c.is_ascii_digit() => Some(Ok(self.get_number_token())),

			c if c.is_ascii_alphabetic() || c == '_' => Some(Ok(self.get_identifier_token())),

			_ => Some(Err(self.annotate_error(Error::UnexpectedCharacter))),
		}
	}
}

mod tests {
	use super::*;

	#[test]
	pub fn empty_source() {
		let input = "";
		let expected = vec![Ok(AnnotatedToken::new(Location::new(1, 1), Token::EndOfFile))];

		let actual = Tokenizer::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn parentheses() {
		let input = "(()";
		let expected = vec![
			Ok(AnnotatedToken::new(Location::new(1, 1), Token::LeftParen)),
			Ok(AnnotatedToken::new(Location::new(1, 2), Token::LeftParen)),
			Ok(AnnotatedToken::new(Location::new(1, 3), Token::RightParen)),
			Ok(AnnotatedToken::new(Location::new(1, 4), Token::EndOfFile)),
		];

		let actual = Tokenizer::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn whitespace() {
		let input = "\t(  \t(";
		let expected = vec![
			Ok(AnnotatedToken::new(Location::new(1, 5), Token::LeftParen)),
			Ok(AnnotatedToken::new(Location::new(1, 9), Token::LeftParen)),
			Ok(AnnotatedToken::new(Location::new(1, 10), Token::EndOfFile)),
		];

		let actual = Tokenizer::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn newlines() {
		let input = "\r\n\r\n  (\n";
		let expected = vec![
			Ok(AnnotatedToken::new(Location::new(3, 3), Token::LeftParen)),
			Ok(AnnotatedToken::new(Location::new(4, 1), Token::EndOfFile)),
		];

		let actual = Tokenizer::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn braces() {
		let input = "{{}}";
		let expected = vec![
			Ok(AnnotatedToken::new(Location::new(1, 1), Token::LeftBrace)),
			Ok(AnnotatedToken::new(Location::new(1, 2), Token::LeftBrace)),
			Ok(AnnotatedToken::new(Location::new(1, 3), Token::RightBrace)),
			Ok(AnnotatedToken::new(Location::new(1, 4), Token::RightBrace)),
			Ok(AnnotatedToken::new(Location::new(1, 5), Token::EndOfFile)),
		];

		let actual = Tokenizer::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn other_single_character_tokens() {
		let input = "({+.*,- ;})";
		let expected = vec![
			Ok(AnnotatedToken::new(Location::new(1, 1), Token::LeftParen)),
			Ok(AnnotatedToken::new(Location::new(1, 2), Token::LeftBrace)),
			Ok(AnnotatedToken::new(Location::new(1, 3), Token::Plus)),
			Ok(AnnotatedToken::new(Location::new(1, 4), Token::Dot)),
			Ok(AnnotatedToken::new(Location::new(1, 5), Token::Star)),
			Ok(AnnotatedToken::new(Location::new(1, 6), Token::Comma)),
			Ok(AnnotatedToken::new(Location::new(1, 7), Token::Minus)),
			Ok(AnnotatedToken::new(Location::new(1, 9), Token::Semicolon)),
			Ok(AnnotatedToken::new(Location::new(1, 10), Token::RightBrace)),
			Ok(AnnotatedToken::new(Location::new(1, 11), Token::RightParen)),
			Ok(AnnotatedToken::new(Location::new(1, 12), Token::EndOfFile)),
		];

		let actual = Tokenizer::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn unknown_character() {
		let input = ".,$(#";
		let expected = vec![
			Ok(AnnotatedToken::new(Location::new(1, 1), Token::Dot)),
			Ok(AnnotatedToken::new(Location::new(1, 2), Token::Comma)),
			Err(AnnotatedError::new(
				Location::new(1, 3),
				Error::UnexpectedCharacter,
				String::from("$"),
			)),
			Ok(AnnotatedToken::new(Location::new(1, 4), Token::LeftParen)),
			Err(AnnotatedError::new(
				Location::new(1, 5),
				Error::UnexpectedCharacter,
				String::from("#"),
			)),
			Ok(AnnotatedToken::new(Location::new(1, 6), Token::EndOfFile)),
		];

		let actual = Tokenizer::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn operators() {
		let input = "! != = == > >= < <=";
		let expected = vec![
			Ok(AnnotatedToken::new(Location::new(1, 1), Token::Bang)),
			Ok(AnnotatedToken::new(Location::new(1, 3), Token::BangEqual)),
			Ok(AnnotatedToken::new(Location::new(1, 6), Token::Equal)),
			Ok(AnnotatedToken::new(Location::new(1, 8), Token::EqualEqual)),
			Ok(AnnotatedToken::new(Location::new(1, 11), Token::Greater)),
			Ok(AnnotatedToken::new(Location::new(1, 13), Token::GreaterEqual)),
			Ok(AnnotatedToken::new(Location::new(1, 16), Token::Less)),
			Ok(AnnotatedToken::new(Location::new(1, 18), Token::LessEqual)),
			Ok(AnnotatedToken::new(Location::new(1, 20), Token::EndOfFile)),
		];

		let actual = Tokenizer::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn comments() {
		let input = "// comment\n/";
		let expected = vec![
			Ok(AnnotatedToken::new(
				Location::new(1, 1),
				Token::Comment { text: String::from("// comment") },
			)),
			Ok(AnnotatedToken::new(Location::new(2, 1), Token::Slash)),
			Ok(AnnotatedToken::new(Location::new(2, 2), Token::EndOfFile)),
		];

		let actual = Tokenizer::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn strings() {
		let input = "\"test\"\"test";
		let expected = vec![
			Ok(AnnotatedToken::new(
				Location::new(1, 1),
				Token::String { text: "\"test\"".into(), value: String::from("test") },
			)),
			Err(AnnotatedError::new(
				Location::new(1, 7),
				Error::UnterminatedString,
				String::from("\"test"),
			)),
			Ok(AnnotatedToken::new(Location::new(1, 12), Token::EndOfFile)),
		];

		let actual = Tokenizer::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn numbers() {
		let input = "420.69\n.5\n5.";
		let expected = vec![
			Ok(AnnotatedToken::new(
				Location::new(1, 1),
				Token::Number { text: "420.69".into(), value: 420.69 },
			)),
			Ok(AnnotatedToken::new(Location::new(2, 1), Token::Dot)),
			Ok(AnnotatedToken::new(
				Location::new(2, 2),
				Token::Number { text: "5".into(), value: 5. },
			)),
			Ok(AnnotatedToken::new(
				Location::new(3, 1),
				Token::Number { text: "5".into(), value: 5. },
			)),
			Ok(AnnotatedToken::new(Location::new(3, 2), Token::Dot)),
			Ok(AnnotatedToken::new(Location::new(3, 3), Token::EndOfFile)),
		];

		let actual = Tokenizer::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn identifiers() {
		let input = "orchid";
		let expected = vec![
			Ok(AnnotatedToken::new(
				Location::new(1, 1),
				Token::Identifier { text: String::from("orchid") },
			)),
			Ok(AnnotatedToken::new(Location::new(1, 7), Token::EndOfFile)),
		];

		let actual = Tokenizer::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn keywords() {
		let input = "and class else false for fun if nil or print return super this true var while";
		let expected = vec![
			Ok(AnnotatedToken::new(Location::new(1, 1), Token::And)),
			Ok(AnnotatedToken::new(Location::new(1, 5), Token::Class)),
			Ok(AnnotatedToken::new(Location::new(1, 11), Token::Else)),
			Ok(AnnotatedToken::new(Location::new(1, 16), Token::False)),
			Ok(AnnotatedToken::new(Location::new(1, 22), Token::For)),
			Ok(AnnotatedToken::new(Location::new(1, 26), Token::Fun)),
			Ok(AnnotatedToken::new(Location::new(1, 30), Token::If)),
			Ok(AnnotatedToken::new(Location::new(1, 33), Token::Nil)),
			Ok(AnnotatedToken::new(Location::new(1, 37), Token::Or)),
			Ok(AnnotatedToken::new(Location::new(1, 40), Token::Print)),
			Ok(AnnotatedToken::new(Location::new(1, 46), Token::Return)),
			Ok(AnnotatedToken::new(Location::new(1, 53), Token::Super)),
			Ok(AnnotatedToken::new(Location::new(1, 59), Token::This)),
			Ok(AnnotatedToken::new(Location::new(1, 64), Token::True)),
			Ok(AnnotatedToken::new(Location::new(1, 69), Token::Var)),
			Ok(AnnotatedToken::new(Location::new(1, 73), Token::While)),
			Ok(AnnotatedToken::new(Location::new(1, 78), Token::EndOfFile)),
		];

		let actual = Tokenizer::new(input).collect::<Vec<_>>();

		assert_eq!(expected, actual);
	}
}
