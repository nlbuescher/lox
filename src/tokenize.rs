use crate::token::{Location, Token, TokenType};

pub fn tokenize(source: &str) -> Vec<Token> {
	Tokenizer::new(source).collect::<Vec<Token>>()
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

	fn get_token(&mut self, token_type: TokenType) -> Token {
		let token = Token::new(token_type, self.buffer.clone(), self.start_location.clone());

		self.buffer.clear();
		self.start_location = self.current_location.clone();

		token
	}

	fn get_string_token(&mut self) -> Token {
		while self.peek() != Some('"') && self.peek() != None {
			self.advance(false);
		}

		if self.peek() == None {
			self.get_token(TokenType::UnterminatedString)
		} else {
			// consume the closing quote
			self.advance(false);
			self.get_token(TokenType::String)
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

		self.get_token(TokenType::Number)
	}

	fn get_identifier_token(&mut self) -> Token {
		while self.peek().take_if(|&mut it| it == '_' || it.is_ascii_alphanumeric()) != None {
			self.advance(true);
		}

		let token_type = match self.buffer.as_str() {
			"and" => TokenType::And,
			"class" => TokenType::Class,
			"else" => TokenType::Else,
			"false" => TokenType::False,
			"for" => TokenType::For,
			"fun" => TokenType::Fun,
			"if" => TokenType::If,
			"nil" => TokenType::Nil,
			"or" => TokenType::Or,
			"print" => TokenType::Print,
			"return" => TokenType::Return,
			"super" => TokenType::Super,
			"this" => TokenType::This,
			"true" => TokenType::True,
			"var" => TokenType::Var,
			"while" => TokenType::While,
			_ => TokenType::Identifier,
		};

		self.get_token(token_type)
	}
}

impl Iterator for Tokenizer {
	type Item = Token;

	fn next(&mut self) -> Option<Self::Item> {
		self.advance(true).and_then(|c| match c {
			'(' => Some(self.get_token(TokenType::LeftParen)),
			')' => Some(self.get_token(TokenType::RightParen)),
			'{' => Some(self.get_token(TokenType::LeftBrace)),
			'}' => Some(self.get_token(TokenType::RightBrace)),
			',' => Some(self.get_token(TokenType::Comma)),
			'.' => Some(self.get_token(TokenType::Dot)),
			'-' => Some(self.get_token(TokenType::Minus)),
			'+' => Some(self.get_token(TokenType::Plus)),
			';' => Some(self.get_token(TokenType::Semicolon)),
			'*' => Some(self.get_token(TokenType::Star)),

			'!' => {
				let token_type = if self.advance_if('=') {
					TokenType::BangEqual
				} else {
					TokenType::Bang
				};
				Some(self.get_token(token_type))
			}

			'=' => {
				let token_type = if self.advance_if('=') {
					TokenType::EqualEqual
				} else {
					TokenType::Equal
				};
				Some(self.get_token(token_type))
			}

			'<' => {
				let token_type = if self.advance_if('=') {
					TokenType::LessEqual
				} else {
					TokenType::Less
				};
				Some(self.get_token(token_type))
			}

			'>' => {
				let token_type = if self.advance_if('=') {
					TokenType::GreaterEqual
				} else {
					TokenType::Greater
				};
				Some(self.get_token(token_type))
			}

			'/' => {
				if self.advance_if('/') {
					while self.peek() != Some('\n') && self.peek() != None {
						self.advance(false);
					}
					Some(self.get_token(TokenType::Comment))
				} else {
					Some(self.get_token(TokenType::Slash))
				}
			}

			'"' => Some(self.get_string_token()),

			_ if c.is_ascii_whitespace() => self.next(),

			_ if c.is_ascii_digit() => Some(self.get_number_token()),

			_ if c == '_' || c.is_ascii_alphabetic() => Some(self.get_identifier_token()),

			_ => Some(self.get_token(TokenType::UnknownChar)),
		})
	}
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
			Token::new(
				TokenType::LeftParen,
				String::from("("),
				Location { line: 1, column: 1 },
			),
			Token::new(
				TokenType::LeftParen,
				String::from("("),
				Location { line: 1, column: 2 },
			),
			Token::new(
				TokenType::RightParen,
				String::from(")"),
				Location { line: 1, column: 3 },
			),
		];

		let actual = tokenize(input);

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn whitespace() {
		let input = " \t(";
		let expected = vec![Token::new(
			TokenType::LeftParen,
			String::from("("),
			Location { line: 1, column: 5 },
		)];

		let actual = tokenize(input);

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn newlines() {
		let input = "\r\n\r\n  (";
		let expected = vec![Token::new(
			TokenType::LeftParen,
			String::from("("),
			Location { line: 3, column: 3 },
		)];

		let actual = tokenize(input);

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn braces() {
		let input = "{{}}";
		let expected = vec![
			Token::new(
				TokenType::LeftBrace,
				String::from("{"),
				Location { line: 1, column: 1 },
			),
			Token::new(
				TokenType::LeftBrace,
				String::from("{"),
				Location { line: 1, column: 2 },
			),
			Token::new(
				TokenType::RightBrace,
				String::from("}"),
				Location { line: 1, column: 3 },
			),
			Token::new(
				TokenType::RightBrace,
				String::from("}"),
				Location { line: 1, column: 4 },
			),
		];

		let actual = tokenize(input);

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn other_single_character_tokens() {
		let input = "({+.*,- ;})";
		let expected = vec![
			Token::new(
				TokenType::LeftParen,
				String::from("("),
				Location { line: 1, column: 1 },
			),
			Token::new(
				TokenType::LeftBrace,
				String::from("{"),
				Location { line: 1, column: 2 },
			),
			Token::new(
				TokenType::Plus,
				String::from("+"),
				Location { line: 1, column: 3 },
			),
			Token::new(
				TokenType::Dot,
				String::from("."),
				Location { line: 1, column: 4 },
			),
			Token::new(
				TokenType::Star,
				String::from("*"),
				Location { line: 1, column: 5 },
			),
			Token::new(
				TokenType::Comma,
				String::from(","),
				Location { line: 1, column: 6 },
			),
			Token::new(
				TokenType::Minus,
				String::from("-"),
				Location { line: 1, column: 7 },
			),
			Token::new(
				TokenType::Semicolon,
				String::from(";"),
				Location { line: 1, column: 9 },
			),
			Token::new(
				TokenType::RightBrace,
				String::from("}"),
				Location {
					line: 1,
					column: 10,
				},
			),
			Token::new(
				TokenType::RightParen,
				String::from(")"),
				Location {
					line: 1,
					column: 11,
				},
			),
		];

		let actual = tokenize(input);

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn unknown_character() {
		let input = ".,$(#";
		let expected = vec![
			Token::new(
				TokenType::Dot,
				String::from("."),
				Location { line: 1, column: 1 },
			),
			Token::new(
				TokenType::Comma,
				String::from(","),
				Location { line: 1, column: 2 },
			),
			Token::new(
				TokenType::UnknownChar,
				String::from("$"),
				Location { line: 1, column: 3 },
			),
			Token::new(
				TokenType::LeftParen,
				String::from("("),
				Location { line: 1, column: 4 },
			),
			Token::new(
				TokenType::UnknownChar,
				String::from("#"),
				Location { line: 1, column: 5 },
			),
		];

		let actual = tokenize(input);

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn operators() {
		let input = "! != = == < <= > >=";
		let expected = vec![
			Token::new(
				TokenType::Bang,
				String::from("!"),
				Location { line: 1, column: 1 },
			),
			Token::new(
				TokenType::BangEqual,
				String::from("!="),
				Location { line: 1, column: 3 },
			),
			Token::new(
				TokenType::Equal,
				String::from("="),
				Location { line: 1, column: 6 },
			),
			Token::new(
				TokenType::EqualEqual,
				String::from("=="),
				Location { line: 1, column: 8 },
			),
			Token::new(
				TokenType::Less,
				String::from("<"),
				Location {
					line: 1,
					column: 11,
				},
			),
			Token::new(
				TokenType::LessEqual,
				String::from("<="),
				Location {
					line: 1,
					column: 13,
				},
			),
			Token::new(
				TokenType::Greater,
				String::from(">"),
				Location {
					line: 1,
					column: 16,
				},
			),
			Token::new(
				TokenType::GreaterEqual,
				String::from(">="),
				Location {
					line: 1,
					column: 18,
				},
			),
		];

		let actual = tokenize(input);

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn comments() {
		let input = "// comment\n/";
		let expected = vec![
			Token::new(
				TokenType::Comment,
				String::from("// comment"),
				Location { line: 1, column: 1 },
			),
			Token::new(
				TokenType::Slash,
				String::from("/"),
				Location { line: 2, column: 1 },
			),
		];

		let actual = tokenize(input);

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn strings() {
		let input = "\"test\"\"test";
		let expected = vec![
			Token::new(
				TokenType::String,
				String::from("\"test\""),
				Location { line: 1, column: 1 },
			),
			Token::new(
				TokenType::UnterminatedString,
				String::from("\"test"),
				Location { line: 1, column: 7 },
			),
		];

		let actual = tokenize(input);

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn numbers() {
		let input = "420.69\n.5\n5.";
		let expected = vec![
			Token::new(
				TokenType::Number,
				String::from("420.69"),
				Location { line: 1, column: 1 },
			),
			Token::new(
				TokenType::Dot,
				String::from("."),
				Location { line: 2, column: 1 },
			),
			Token::new(
				TokenType::Number,
				String::from("5"),
				Location { line: 2, column: 2 },
			),
			Token::new(
				TokenType::Number,
				String::from("5"),
				Location { line: 3, column: 1 },
			),
			Token::new(
				TokenType::Dot,
				String::from("."),
				Location { line: 3, column: 2 },
			),
		];

		let actual = tokenize(input);

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn identifiers() {
		let input = "orchid";
		let expected = vec![
			Token::new(TokenType::Identifier, String::from("orchid"), Location { line: 1, column: 1 }),
		];

		let actual = tokenize(input);

		assert_eq!(expected, actual);
	}

	#[test]
	pub fn keywords() {
		let input = "and class else false for fun if nil or print return super this true var while";
		let expected = vec![
			Token::new(TokenType::And, String::from("and"), Location { line: 1, column: 1 }),
			Token::new(TokenType::Class, String::from("class"), Location { line: 1, column: 5 }),
			Token::new(TokenType::Else, String::from("else"), Location { line: 1, column: 11 }),
			Token::new(TokenType::False, String::from("false"), Location { line: 1, column: 16 }),
			Token::new(TokenType::For, String::from("for"), Location { line: 1, column: 22 }),
			Token::new(TokenType::Fun, String::from("fun"), Location { line: 1, column: 26 }),
			Token::new(TokenType::If, String::from("if"), Location { line: 1, column: 30 }),
			Token::new(TokenType::Nil, String::from("nil"), Location { line: 1, column: 33 }),
			Token::new(TokenType::Or, String::from("or"), Location { line: 1, column: 37 }),
			Token::new(TokenType::Print, String::from("print"), Location { line: 1, column: 40 }),
			Token::new(TokenType::Return, String::from("return"), Location { line: 1, column: 46 }),
			Token::new(TokenType::Super, String::from("super"), Location { line: 1, column: 53 }),
			Token::new(TokenType::This, String::from("this"), Location { line: 1, column: 59 }),
			Token::new(TokenType::True, String::from("true"), Location { line: 1, column: 64 }),
			Token::new(TokenType::Var, String::from("var"), Location { line: 1, column: 69 }),
			Token::new(TokenType::While, String::from("while"), Location { line: 1, column: 73 }),
		];

		let actual = tokenize(input);

		assert_eq!(expected, actual);
	}
}
