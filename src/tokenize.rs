use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub struct Location {
	pub line: usize,
	pub column: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
	// Single-character tokens
	LeftParen {
		location: Location,
	},
	RightParen {
		location: Location,
	},
	LeftBrace {
		location: Location,
	},
	RightBrace {
		location: Location,
	},
	Comma {
		location: Location,
	},
	Dot {
		location: Location,
	},
	Minus {
		location: Location,
	},
	Plus {
		location: Location,
	},
	Semicolon {
		location: Location,
	},
	Slash {
		location: Location,
	},
	Star {
		location: Location,
	},

	// One- or two-character tokens
	Bang {
		location: Location,
	},
	BangEqual {
		location: Location,
	},
	Equal {
		location: Location,
	},
	EqualEqual {
		location: Location,
	},
	Greater {
		location: Location,
	},
	GreaterEqual {
		location: Location,
	},
	Less {
		location: Location,
	},
	LessEqual {
		location: Location,
	},

	// Literals
	Identifier {
		location: Location,
		text: String,
	},
	String {
		location: Location,
		text: String,
		value: String,
	},
	Number {
		location: Location,
		text: String,
		value: f64,
	},

	// Keywords
	And {
		location: Location,
	},
	Class {
		location: Location,
	},
	Else {
		location: Location,
	},
	False {
		location: Location,
	},
	Fun {
		location: Location,
	},
	For {
		location: Location,
	},
	If {
		location: Location,
	},
	Nil {
		location: Location,
	},
	Or {
		location: Location,
	},
	Print {
		location: Location,
	},
	Return {
		location: Location,
	},
	Super {
		location: Location,
	},
	This {
		location: Location,
	},
	True {
		location: Location,
	},
	Var {
		location: Location,
	},
	While {
		location: Location,
	},

	// Other
	Comment {
		location: Location,
		text: String,
	},

	// Error
	UnknownChar {
		location: Location,
		text: String,
	},
	UnterminatedString {
		location: Location,
		text: String,
	},
}

impl Token {
	pub fn left_paren(location: Location) -> Token {
		Token::LeftParen { location }
	}

	pub fn right_paren(location: Location) -> Token {
		Token::RightParen { location }
	}

	pub fn left_brace(location: Location) -> Token {
		Token::LeftBrace { location }
	}

	pub fn right_brace(location: Location) -> Token {
		Token::RightBrace { location }
	}

	pub fn comma(location: Location) -> Token {
		Token::Comma { location }
	}

	pub fn dot(location: Location) -> Token {
		Token::Dot { location }
	}

	pub fn minus(location: Location) -> Token {
		Token::Minus { location }
	}

	pub fn plus(location: Location) -> Token {
		Token::Plus { location }
	}

	pub fn semicolon(location: Location) -> Token {
		Token::Semicolon { location }
	}

	pub fn slash(location: Location) -> Token {
		Token::Slash { location }
	}

	pub fn star(location: Location) -> Token {
		Token::Star { location }
	}

	pub fn bang(location: Location) -> Token {
		Token::Bang { location }
	}

	pub fn bang_equal(location: Location) -> Token {
		Token::BangEqual { location }
	}

	pub fn equal(location: Location) -> Token {
		Token::Equal { location }
	}

	pub fn equal_equal(location: Location) -> Token {
		Token::EqualEqual { location }
	}

	pub fn greater(location: Location) -> Token {
		Token::Greater { location }
	}

	pub fn greater_equal(location: Location) -> Token {
		Token::GreaterEqual { location }
	}

	pub fn less(location: Location) -> Token {
		Token::Less { location }
	}

	pub fn less_equal(location: Location) -> Token {
		Token::LessEqual { location }
	}

	pub fn identifier(location: Location, text: String) -> Token {
		Token::Identifier { location, text }
	}

	pub fn string(location: Location, text: String, value: String) -> Token {
		Token::String {
			location,
			text,
			value,
		}
	}

	pub fn number(location: Location, text: String, value: f64) -> Token {
		Token::Number {
			location,
			text,
			value,
		}
	}

	pub fn and(location: Location) -> Token {
		Token::And { location }
	}

	pub fn class(location: Location) -> Token {
		Token::Class { location }
	}

	pub fn else_(location: Location) -> Token {
		Token::Else { location }
	}

	pub fn false_(location: Location) -> Token {
		Token::False { location }
	}

	pub fn fun(location: Location) -> Token {
		Token::Fun { location }
	}

	pub fn for_(location: Location) -> Token {
		Token::For { location }
	}

	pub fn if_(location: Location) -> Token {
		Token::If { location }
	}

	pub fn nil(location: Location) -> Token {
		Token::Nil { location }
	}

	pub fn or(location: Location) -> Token {
		Token::Or { location }
	}

	pub fn print(location: Location) -> Token {
		Token::Print { location }
	}

	pub fn return_(location: Location) -> Token {
		Token::Return { location }
	}

	pub fn super_(location: Location) -> Token {
		Token::Super { location }
	}

	pub fn this(location: Location) -> Token {
		Token::This { location }
	}

	pub fn true_(location: Location) -> Token {
		Token::True { location }
	}

	pub fn var(location: Location) -> Token {
		Token::Var { location }
	}

	pub fn while_(location: Location) -> Token {
		Token::While { location }
	}

	pub fn comment(location: Location, text: String) -> Token {
		Token::Comment { text, location }
	}

	pub fn unknown_char(location: Location, text: String) -> Token {
		Token::UnknownChar { text, location }
	}

	pub fn unterminated_string(location: Location, text: String) -> Token {
		Token::UnterminatedString { text, location }
	}

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
		} else {
			None
		}
	}

	fn peek_next(&mut self) -> Option<char> {
		if self.position + 1 < self.source.len() {
			Some(self.source[self.position + 1])
		} else {
			None
		}
	}

	fn advance(&mut self, ignore_whitespace: bool) -> Option<char> {
		let next;

		if self.position < self.source.len() {
			next = Some(self.source[self.position]);
			self.position += 1;
		} else {
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

	/// returns: whether the tokenizer advanced or not
	fn advance_if(&mut self, expected: char) -> bool {
		match self.peek() {
			None => false,
			Some(c) => {
				if c != expected {
					false
				} else {
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
			self.get_token_with_text(Token::unterminated_string)
		} else {
			// consume the closing quote
			self.advance(false);

			let token = Token::string(
				self.start_location.clone(),
				self.buffer.clone(),
				self.buffer[1..self.buffer.len() - 1].to_string(),
			);

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

		let token = Token::number(
			self.start_location.clone(),
			self.buffer.clone(),
			self.buffer.parse::<f64>().unwrap(),
		);

		self.reset();

		token
	}

	fn get_identifier_token(&mut self) -> Token {
		while self
			.peek()
			.take_if(|&mut it| it == '_' || it.is_ascii_alphanumeric())
			!= None
		{
			self.advance(true);
		}

		match self.buffer.as_str() {
			"and" => self.get_token(Token::and),
			"class" => self.get_token(Token::class),
			"else" => self.get_token(Token::else_),
			"false" => self.get_token(Token::false_),
			"for" => self.get_token(Token::for_),
			"fun" => self.get_token(Token::fun),
			"if" => self.get_token(Token::if_),
			"nil" => self.get_token(Token::nil),
			"or" => self.get_token(Token::or),
			"print" => self.get_token(Token::print),
			"return" => self.get_token(Token::return_),
			"super" => self.get_token(Token::super_),
			"this" => self.get_token(Token::this),
			"true" => self.get_token(Token::true_),
			"var" => self.get_token(Token::var),
			"while" => self.get_token(Token::while_),
			_ => self.get_token_with_text(Token::identifier),
		}
	}
}

impl Iterator for Tokenizer {
	type Item = Token;

	fn next(&mut self) -> Option<Self::Item> {
		self.advance(true).and_then(|c| match c {
			'(' => Some(self.get_token(Token::left_paren)),
			')' => Some(self.get_token(Token::right_paren)),
			'{' => Some(self.get_token(Token::left_brace)),
			'}' => Some(self.get_token(Token::right_brace)),
			',' => Some(self.get_token(Token::comma)),
			'.' => Some(self.get_token(Token::dot)),
			'-' => Some(self.get_token(Token::minus)),
			'+' => Some(self.get_token(Token::plus)),
			';' => Some(self.get_token(Token::semicolon)),
			'*' => Some(self.get_token(Token::star)),

			'!' => {
				let token_type = if self.advance_if('=') {
					Token::bang_equal
				} else {
					Token::bang
				};
				Some(self.get_token(token_type))
			}

			'=' => {
				let token_type = if self.advance_if('=') {
					Token::equal_equal
				} else {
					Token::equal
				};
				Some(self.get_token(token_type))
			}

			'<' => {
				let token_type = if self.advance_if('=') {
					Token::less_equal
				} else {
					Token::less
				};
				Some(self.get_token(token_type))
			}

			'>' => {
				let token_type = if self.advance_if('=') {
					Token::greater_equal
				} else {
					Token::greater
				};
				Some(self.get_token(token_type))
			}

			'/' => {
				if self.advance_if('/') {
					while self.peek() != Some('\n') && self.peek() != None {
						self.advance(false);
					}
					Some(self.get_token_with_text(Token::comment))
				} else {
					Some(self.get_token(Token::slash))
				}
			}

			'"' => Some(self.get_string_token()),

			_ if c.is_ascii_whitespace() => self.next(),

			_ if c.is_ascii_digit() => Some(self.get_number_token()),

			_ if c == '_' || c.is_ascii_alphabetic() => Some(self.get_identifier_token()),

			_ => Some(self.get_token_with_text(Token::unknown_char)),
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
			Token::left_paren(Location { line: 1, column: 1 }),
			Token::left_paren(Location { line: 1, column: 2 }),
			Token::right_paren(Location { line: 1, column: 3 }),
		];

		let actual = tokenize(input);

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn whitespace() {
		let input = " \t(";
		let expected = vec![Token::left_paren(Location { line: 1, column: 5 })];

		let actual = tokenize(input);

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn newlines() {
		let input = "\r\n\r\n  (";
		let expected = vec![Token::left_paren(Location { line: 3, column: 3 })];

		let actual = tokenize(input);

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn braces() {
		let input = "{{}}";
		let expected = vec![
			Token::left_brace(Location { line: 1, column: 1 }),
			Token::left_brace(Location { line: 1, column: 2 }),
			Token::right_brace(Location { line: 1, column: 3 }),
			Token::right_brace(Location { line: 1, column: 4 }),
		];

		let actual = tokenize(input);

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn other_single_character_tokens() {
		let input = "({+.*,- ;})";
		let expected = vec![
			Token::left_paren(Location { line: 1, column: 1 }),
			Token::left_brace(Location { line: 1, column: 2 }),
			Token::plus(Location { line: 1, column: 3 }),
			Token::dot(Location { line: 1, column: 4 }),
			Token::star(Location { line: 1, column: 5 }),
			Token::comma(Location { line: 1, column: 6 }),
			Token::minus(Location { line: 1, column: 7 }),
			Token::semicolon(Location { line: 1, column: 9 }),
			Token::right_brace(Location {
				line: 1,
				column: 10,
			}),
			Token::right_paren(Location {
				line: 1,
				column: 11,
			}),
		];

		let actual = tokenize(input);

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn unknown_character() {
		let input = ".,$(#";
		let expected = vec![
			Token::dot(Location { line: 1, column: 1 }),
			Token::comma(Location { line: 1, column: 2 }),
			Token::unknown_char(Location { line: 1, column: 3 }, String::from("$")),
			Token::left_paren(Location { line: 1, column: 4 }),
			Token::unknown_char(Location { line: 1, column: 5 }, String::from("#")),
		];

		let actual = tokenize(input);

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn operators() {
		let input = "! != = == > >= < <=";
		let expected = vec![
			Token::bang(Location { line: 1, column: 1 }),
			Token::bang_equal(Location { line: 1, column: 3 }),
			Token::equal(Location { line: 1, column: 6 }),
			Token::equal_equal(Location { line: 1, column: 8 }),
			Token::greater(Location {
				line: 1,
				column: 11,
			}),
			Token::greater_equal(Location {
				line: 1,
				column: 13,
			}),
			Token::less(Location {
				line: 1,
				column: 16,
			}),
			Token::less_equal(Location {
				line: 1,
				column: 18,
			}),
		];

		let actual = tokenize(input);

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn comments() {
		let input = "// comment\n/";
		let expected = vec![
			Token::comment(Location { line: 1, column: 1 }, String::from("// comment")),
			Token::slash(Location { line: 2, column: 1 }),
		];

		let actual = tokenize(input);

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn strings() {
		let input = "\"test\"\"test";
		let expected = vec![
			Token::string(
				Location { line: 1, column: 1 },
				String::from("\"test\""),
				String::from("test"),
			),
			Token::unterminated_string(Location { line: 1, column: 7 }, String::from("\"test")),
		];

		let actual = tokenize(input);

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn numbers() {
		let input = "420.69\n.5\n5.";
		let expected = vec![
			Token::number(
				Location { line: 1, column: 1 },
				String::from("420.69"),
				420.69,
			),
			Token::dot(Location { line: 2, column: 1 }),
			Token::number(Location { line: 2, column: 2 }, String::from("5"), 5.),
			Token::number(Location { line: 3, column: 1 }, String::from("5"), 5.),
			Token::dot(Location { line: 3, column: 2 }),
		];

		let actual = tokenize(input);

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn identifiers() {
		let input = "orchid";
		let expected = vec![Token::identifier(
			Location { line: 1, column: 1 },
			String::from("orchid"),
		)];

		let actual = tokenize(input);

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn keywords() {
		let input = "and class else false for fun if nil or print return super this true var while";
		let expected = vec![
			Token::and(Location { line: 1, column: 1 }),
			Token::class(Location { line: 1, column: 5 }),
			Token::else_(Location {
				line: 1,
				column: 11,
			}),
			Token::false_(Location {
				line: 1,
				column: 16,
			}),
			Token::for_(Location {
				line: 1,
				column: 22,
			}),
			Token::fun(Location {
				line: 1,
				column: 26,
			}),
			Token::if_(Location {
				line: 1,
				column: 30,
			}),
			Token::nil(Location {
				line: 1,
				column: 33,
			}),
			Token::or(Location {
				line: 1,
				column: 37,
			}),
			Token::print(Location {
				line: 1,
				column: 40,
			}),
			Token::return_(Location {
				line: 1,
				column: 46,
			}),
			Token::super_(Location {
				line: 1,
				column: 53,
			}),
			Token::this(Location {
				line: 1,
				column: 59,
			}),
			Token::true_(Location {
				line: 1,
				column: 64,
			}),
			Token::var(Location {
				line: 1,
				column: 69,
			}),
			Token::while_(Location {
				line: 1,
				column: 73,
			}),
		];

		let actual = tokenize(input);

		assert_eq!(expected, actual);
	}
}
