use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub struct Location {
	pub line: usize,
	pub column: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
	// Single-character tokens
	LeftParen { location: Location },
	RightParen { location: Location },
	LeftBrace { location: Location },
	RightBrace { location: Location },
	Comma { location: Location },
	Dot { location: Location },
	Minus { location: Location },
	Plus { location: Location },
	Semicolon { location: Location },
	Slash { location: Location },
	Star { location: Location },

	// One- or two-character tokens
	Bang { location: Location },
	BangEqual { location: Location },
	Equal { location: Location },
	EqualEqual { location: Location },
	Greater { location: Location },
	GreaterEqual { location: Location },
	Less { location: Location },
	LessEqual { location: Location },

	// Literals
	Identifier { location: Location, text: String },
	String { location: Location, text: String, value: String },
	Number { location: Location, text: String, value: f64 },

	// Keywords
	And { location: Location },
	Class { location: Location },
	Else { location: Location },
	False { location: Location },
	Fun { location: Location },
	For { location: Location },
	If { location: Location },
	Nil { location: Location },
	Or { location: Location },
	Print { location: Location },
	Return { location: Location },
	Super { location: Location },
	This { location: Location },
	True { location: Location },
	Var { location: Location },
	While { location: Location },

	// Other
	Comment { location: Location, text: String },

	// Error
	UnknownChar { location: Location, text: String },
	UnterminatedString { location: Location, text: String },
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

	pub fn location(&self) -> &Location {
		match self {
			Token::LeftParen { location, .. } => location,
			Token::RightParen { location, .. } => location,
			Token::LeftBrace { location, .. } => location,
			Token::RightBrace { location, .. } => location,
			Token::Comma { location, .. } => location,
			Token::Dot { location, .. } => location,
			Token::Minus { location, .. } => location,
			Token::Plus { location, .. } => location,
			Token::Semicolon { location, .. } => location,
			Token::Slash { location, .. } => location,
			Token::Star { location, .. } => location,

			Token::Bang { location, .. } => location,
			Token::BangEqual { location, .. } => location,
			Token::Equal { location, .. } => location,
			Token::EqualEqual { location, .. } => location,
			Token::Greater { location, .. } => location,
			Token::GreaterEqual { location, .. } => location,
			Token::Less { location, .. } => location,
			Token::LessEqual { location, .. } => location,

			Token::Identifier { location, .. } => location,
			Token::String { location, .. } => location,
			Token::Number { location, .. } => location,

			Token::And { location, .. } => location,
			Token::Class { location, .. } => location,
			Token::Else { location, .. } => location,
			Token::False { location, .. } => location,
			Token::Fun { location, .. } => location,
			Token::For { location, .. } => location,
			Token::If { location, .. } => location,
			Token::Nil { location, .. } => location,
			Token::Or { location, .. } => location,
			Token::Print { location, .. } => location,
			Token::Return { location, .. } => location,
			Token::Super { location, .. } => location,
			Token::This { location, .. } => location,
			Token::True { location, .. } => location,
			Token::Var { location, .. } => location,
			Token::While { location, .. } => location,

			Token::Comment { location, .. } => location,

			Token::UnknownChar { location, .. } => location,
			Token::UnterminatedString { location, .. } => location,
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

	fn advance(&mut self, ignore_whitespace: bool) -> Option<char> {
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

			if ignore_whitespace && c.is_whitespace() {
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
					self.advance(true);
					true
				}
			}
		}
	}

	pub fn reset(&mut self) {
		self.buffer.clear();
		self.start_location = self.current_location.clone();
	}

	fn get_token(&mut self, constructor: impl FnOnce(Location) -> Token) -> Token {
		let token = constructor(self.start_location.clone());

		self.reset();

		token
	}

	fn get_token_with_text(
		&mut self,
		constructor: impl FnOnce(Location, String) -> Token,
	) -> Token {
		let token = constructor(self.start_location.clone(), self.buffer.clone());

		self.reset();

		token
	}

	fn get_string_token(&mut self) -> Token {
		while self.peek() != Some('"') && self.peek() != None {
			self.advance(false);
		}

		if self.peek() == None {
			self.get_token_with_text(|location, text| Token::UnterminatedString { location, text })
		}
		else {
			// consume the closing quote
			self.advance(false);

			let token = Token::String {
				location: self.start_location.clone(),
				text: self.buffer.clone(),
				value: self.buffer[1..self.buffer.len() - 1].to_string(),
			};

			self.reset();

			token
		}
	}

	fn get_number_token(&mut self) -> Token {
		while self.peek().take_if(|it| it.is_ascii_digit()) != None {
			self.advance(true);
		}

		if self.peek() == Some('.') && self.peek_next().take_if(|it| it.is_ascii_digit()) != None {
			// Consume the decimal point
			self.advance(true);

			while self.peek().take_if(|it| it.is_ascii_digit()) != None {
				self.advance(true);
			}
		}

		let token = Token::Number {
			location: self.start_location.clone(),
			text: self.buffer.clone(),
			value: self.buffer.parse::<f64>().unwrap(),
		};

		self.reset();

		token
	}

	fn get_identifier_token(&mut self) -> Token {
		while self.peek().take_if(|&mut it| it == '_' || it.is_ascii_alphanumeric()) != None {
			self.advance(true);
		}

		match self.buffer.as_str() {
			"and" => self.get_token(|location| Token::And { location }),
			"class" => self.get_token(|location| Token::Class { location }),
			"else" => self.get_token(|location| Token::Else { location }),
			"false" => self.get_token(|location| Token::False { location }),
			"for" => self.get_token(|location| Token::For { location }),
			"fun" => self.get_token(|location| Token::Fun { location }),
			"if" => self.get_token(|location| Token::If { location }),
			"nil" => self.get_token(|location| Token::Nil { location }),
			"or" => self.get_token(|location| Token::Or { location }),
			"print" => self.get_token(|location| Token::Print { location }),
			"return" => self.get_token(|location| Token::Return { location }),
			"super" => self.get_token(|location| Token::Super { location }),
			"this" => self.get_token(|location| Token::This { location }),
			"true" => self.get_token(|location| Token::True { location }),
			"var" => self.get_token(|location| Token::Var { location }),
			"while" => self.get_token(|location| Token::While { location }),
			_ => self.get_token_with_text(|location, text| Token::Identifier { location, text }),
		}
	}
}

impl Iterator for Tokenizer {
	type Item = Token;

	fn next(&mut self) -> Option<Self::Item> {
		self.advance(true).and_then(|c| match c {
			'(' => Some(self.get_token(|location| Token::LeftParen { location })),
			')' => Some(self.get_token(|location| Token::RightParen { location })),
			'{' => Some(self.get_token(|location| Token::LeftBrace { location })),
			'}' => Some(self.get_token(|location| Token::RightBrace { location })),
			',' => Some(self.get_token(|location| Token::Comma { location })),
			'.' => Some(self.get_token(|location| Token::Dot { location })),
			'-' => Some(self.get_token(|location| Token::Minus { location })),
			'+' => Some(self.get_token(|location| Token::Plus { location })),
			';' => Some(self.get_token(|location| Token::Semicolon { location })),
			'*' => Some(self.get_token(|location| Token::Star { location })),

			'!' => {
				let token_fun = if self.advance_if('=') {
					|location| Token::BangEqual { location }
				}
				else {
					|location| Token::Bang { location }
				};

				Some(self.get_token(token_fun))
			}

			'=' => {
				let token_fun = if self.advance_if('=') {
					|location| Token::EqualEqual { location }
				}
				else {
					|location| Token::Equal { location }
				};

				Some(self.get_token(token_fun))
			}

			'>' => {
				let token_fun = if self.advance_if('=') {
					|location| Token::GreaterEqual { location }
				}
				else {
					|location| Token::Greater { location }
				};

				Some(self.get_token(token_fun))
			}

			'<' => {
				let token_fun = if self.advance_if('=') {
					|location| Token::LessEqual { location }
				}
				else {
					|location| Token::Less { location }
				};

				Some(self.get_token(token_fun))
			}

			'/' => {
				if self.advance_if('/') {
					while self.peek() != Some('\n') && self.peek() != None {
						self.advance(false);
					}
					Some(
						self.get_token_with_text(|location, text| Token::Comment {
							location,
							text,
						}),
					)
				}
				else {
					Some(self.get_token(|location| Token::Slash { location }))
				}
			}

			'"' => Some(self.get_string_token()),

			_ if c.is_ascii_whitespace() => self.next(),

			_ if c.is_ascii_digit() => Some(self.get_number_token()),

			_ if c == '_' || c.is_ascii_alphabetic() => Some(self.get_identifier_token()),

			_ => Some(
				self.get_token_with_text(|location, text| Token::UnknownChar { location, text }),
			),
		})
	}
}

pub fn tokenize(source: &str) -> Vec<Token> {
	Tokenizer::new(source).collect::<Vec<Token>>()
}

mod tests {
	use super::*;

	#[test]
	pub fn empty_source() {
		let input = "";
		let expected = Vec::<Token>::new();

		let actual = tokenize(input);

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn parentheses() {
		let input = "(()";
		let expected = vec![
			Token::LeftParen { location: Location { line: 1, column: 1 } },
			Token::LeftParen { location: Location { line: 1, column: 2 } },
			Token::RightParen { location: Location { line: 1, column: 3 } },
		];

		let actual = tokenize(input);

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn whitespace() {
		let input = " \t(";
		let expected = vec![Token::LeftParen { location: Location { line: 1, column: 5 } }];

		let actual = tokenize(input);

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn newlines() {
		let input = "\r\n\r\n  (";
		let expected = vec![Token::LeftParen { location: Location { line: 3, column: 3 } }];

		let actual = tokenize(input);

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn braces() {
		let input = "{{}}";
		let expected = vec![
			Token::LeftBrace { location: Location { line: 1, column: 1 } },
			Token::LeftBrace { location: Location { line: 1, column: 2 } },
			Token::RightBrace { location: Location { line: 1, column: 3 } },
			Token::RightBrace { location: Location { line: 1, column: 4 } },
		];

		let actual = tokenize(input);

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn other_single_character_tokens() {
		let input = "({+.*,- ;})";
		let expected = vec![
			Token::LeftParen { location: Location { line: 1, column: 1 } },
			Token::LeftBrace { location: Location { line: 1, column: 2 } },
			Token::Plus { location: Location { line: 1, column: 3 } },
			Token::Dot { location: Location { line: 1, column: 4 } },
			Token::Star { location: Location { line: 1, column: 5 } },
			Token::Comma { location: Location { line: 1, column: 6 } },
			Token::Minus { location: Location { line: 1, column: 7 } },
			Token::Semicolon { location: Location { line: 1, column: 9 } },
			Token::RightBrace { location: Location { line: 1, column: 10 } },
			Token::RightParen { location: Location { line: 1, column: 11 } },
		];

		let actual = tokenize(input);

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn unknown_character() {
		let input = ".,$(#";
		let expected = vec![
			Token::Dot { location: Location { line: 1, column: 1 } },
			Token::Comma { location: Location { line: 1, column: 2 } },
			Token::UnknownChar {
				location: Location { line: 1, column: 3 },
				text: String::from("$"),
			},
			Token::LeftParen { location: Location { line: 1, column: 4 } },
			Token::UnknownChar {
				location: Location { line: 1, column: 5 },
				text: String::from("#"),
			},
		];

		let actual = tokenize(input);

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn operators() {
		let input = "! != = == > >= < <=";
		let expected = vec![
			Token::Bang { location: Location { line: 1, column: 1 } },
			Token::BangEqual { location: Location { line: 1, column: 3 } },
			Token::Equal { location: Location { line: 1, column: 6 } },
			Token::EqualEqual { location: Location { line: 1, column: 8 } },
			Token::Greater { location: Location { line: 1, column: 11 } },
			Token::GreaterEqual { location: Location { line: 1, column: 13 } },
			Token::Less { location: Location { line: 1, column: 16 } },
			Token::LessEqual { location: Location { line: 1, column: 18 } },
		];

		let actual = tokenize(input);

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn comments() {
		let input = "// comment\n/";
		let expected = vec![
			Token::Comment {
				location: Location { line: 1, column: 1 },
				text: String::from("// comment"),
			},
			Token::Slash { location: Location { line: 2, column: 1 } },
		];

		let actual = tokenize(input);

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn strings() {
		let input = "\"test\"\"test";
		let expected = vec![
			Token::String {
				location: Location { line: 1, column: 1 },
				text: String::from("\"test\""),
				value: String::from("test"),
			},
			Token::UnterminatedString {
				location: Location { line: 1, column: 7 },
				text: String::from("\"test"),
			},
		];

		let actual = tokenize(input);

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn numbers() {
		let input = "420.69\n.5\n5.";
		let expected = vec![
			Token::Number {
				location: Location { line: 1, column: 1 },
				text: String::from("420.69"),
				value: 420.69,
			},
			Token::Dot { location: Location { line: 2, column: 1 } },
			Token::Number {
				location: Location { line: 2, column: 2 },
				text: String::from("5"),
				value: 5.,
			},
			Token::Number {
				location: Location { line: 3, column: 1 },
				text: String::from("5"),
				value: 5.,
			},
			Token::Dot { location: Location { line: 3, column: 2 } },
		];

		let actual = tokenize(input);

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn identifiers() {
		let input = "orchid";
		let expected = vec![Token::Identifier {
			location: Location { line: 1, column: 1 },
			text: String::from("orchid"),
		}];

		let actual = tokenize(input);

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn keywords() {
		let input = "and class else false for fun if nil or print return super this true var while";
		let expected = vec![
			Token::And { location: Location { line: 1, column: 1 } },
			Token::Class { location: Location { line: 1, column: 5 } },
			Token::Else { location: Location { line: 1, column: 11 } },
			Token::False { location: Location { line: 1, column: 16 } },
			Token::For { location: Location { line: 1, column: 22 } },
			Token::Fun { location: Location { line: 1, column: 26 } },
			Token::If { location: Location { line: 1, column: 30 } },
			Token::Nil { location: Location { line: 1, column: 33 } },
			Token::Or { location: Location { line: 1, column: 37 } },
			Token::Print { location: Location { line: 1, column: 40 } },
			Token::Return { location: Location { line: 1, column: 46 } },
			Token::Super { location: Location { line: 1, column: 53 } },
			Token::This { location: Location { line: 1, column: 59 } },
			Token::True { location: Location { line: 1, column: 64 } },
			Token::Var { location: Location { line: 1, column: 69 } },
			Token::While { location: Location { line: 1, column: 73 } },
		];

		let actual = tokenize(input);

		assert_eq!(expected, actual);
	}
}
