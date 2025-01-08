use crate::token::{Location, Token, TokenType};
use std::iter::Peekable;
use tap::{Tap, TapOptional};

pub fn tokenize(source: &str) -> Vec<Token> {
	Tokenizer::new(source.chars().peekable()).collect::<Vec<Token>>()
}

struct Tokenizer<Source: Iterator<Item = char>> {
	source: Peekable<Source>,
	buffer: String,
	start_location: Location,
	current_location: Location,
}

impl<Source: Iterator<Item = char>> Tokenizer<Source> {
	pub fn new(source: Peekable<Source>) -> Self {
		Tokenizer {
			source,
			buffer: String::new(),
			start_location: Location { line: 1, column: 1 },
			current_location: Location { line: 1, column: 1 },
		}
	}
	
	fn peek(&mut self) -> Option<&char> {
		self.source.peek()
	}
	
	fn advance(&mut self) -> Option<char> {
		self.source.next().tap_some(|c| {
			// add char to buffer
			self.buffer.push(*c);
			
			// update location
			match c {
				'\n' => {
					self.current_location.line += 1;
					self.current_location.column = 1;
				}
				'\t' => {
					self.current_location.column = ((self.current_location.column + 4) & !0b11usize) + 1;
				}
				_ => self.current_location.column += 1,
			}
			
			// disregard whitespace
			if c.is_whitespace() {
				self.start_location = self.current_location.clone();
				self.buffer.clear();
			}
		})
	}
	
	/// returns: whether the tokenizer advanced or not 
	fn advance_if(&mut self, expected: char) -> bool {
		match self.peek() {
			None => false,
			Some(&c) => {
				(c == expected).tap(|&it| if it { self.advance(); })
			}
		}
	}
	
	fn get_token(&mut self, token_type: TokenType) -> Token {
		Token::new(token_type, self.buffer.clone(), self.start_location.clone())
			.tap(|_| {
				self.buffer.clear();
				self.start_location = self.current_location.clone();
			})
	}
}

impl <Source: Iterator<Item=char>> Iterator for Tokenizer<Source> {
	type Item = Token;

	fn next(&mut self) -> Option<Self::Item> {
		self.advance().and_then(|c| {
			match c {
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
					let token_type = if self.advance_if('=') { TokenType::BangEqual } else { TokenType::Bang };
					Some(self.get_token(token_type))
				}
				'=' => {
					let token_type = if self.advance_if('=') { TokenType::EqualEqual } else { TokenType::Equal };
					Some(self.get_token(token_type))
				}
				'<' => {
					let token_type = if self.advance_if('=') { TokenType::LessEqual } else { TokenType::Less };
					Some(self.get_token(token_type))
				}
				'>' => {
					let token_type = if self.advance_if('=') { TokenType::GreaterEqual } else { TokenType::Greater };
					Some(self.get_token(token_type))
				}
				
				_ if c.is_whitespace() => self.next(),
				
				_ => Some(self.get_token(TokenType::Unknown)),
			}
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
			Token::new(TokenType::LeftParen, String::from("("), Location { line: 1, column: 1 }),
			Token::new(TokenType::LeftParen, String::from("("), Location { line: 1, column: 2 }),
			Token::new(TokenType::RightParen, String::from(")"), Location { line: 1, column: 3 }),
		];

		let actual = tokenize(input);

		assert_eq!(expected, actual);
	}
	
	#[test]
	pub fn whitespace() {
		let input = " \t(";
		let expected = vec![
			Token::new(TokenType::LeftParen, String::from("("), Location { line: 1, column: 5 }),
		];
		
		let actual = tokenize(input);
		
		assert_eq!(expected, actual);
	}
	
	#[test]
	pub fn newlines() {
		let input = "\r\n\r\n  (";
		let expected = vec![
			Token::new(TokenType::LeftParen, String::from("("), Location { line: 3, column: 3 }),
		];
		
		let actual = tokenize(input);
		
		assert_eq!(expected, actual);
	}
	
	#[test]
	pub fn braces() {
		let input = "{{}}";
		let expected = vec![
			Token::new(TokenType::LeftBrace, String::from("{"), Location { line: 1, column: 1 }),
			Token::new(TokenType::LeftBrace, String::from("{"), Location { line: 1, column: 2 }),
			Token::new(TokenType::RightBrace, String::from("}"), Location { line: 1, column: 3 }),
			Token::new(TokenType::RightBrace, String::from("}"), Location { line: 1, column: 4 }),
		];
		
		let actual = tokenize(input);
		
		assert_eq!(expected, actual);
	}
	
	#[test]
	pub fn other_single_character_tokens() {
		let input = "({+.*,- ;})";
		let expected = vec![
			Token::new(TokenType::LeftParen, String::from("("), Location { line: 1, column: 1 }),
			Token::new(TokenType::LeftBrace, String::from("{"), Location { line: 1, column: 2 }),
			Token::new(TokenType::Plus, String::from("+"), Location { line: 1, column: 3 }),
			Token::new(TokenType::Dot, String::from("."), Location { line: 1, column: 4 }),
			Token::new(TokenType::Star, String::from("*"), Location { line: 1, column: 5 }),
			Token::new(TokenType::Comma, String::from(","), Location { line: 1, column: 6 }),
			Token::new(TokenType::Minus, String::from("-"), Location { line: 1, column: 7 }),
			Token::new(TokenType::Semicolon, String::from(";"), Location { line: 1, column: 9 }),
			Token::new(TokenType::RightBrace, String::from("}"), Location { line: 1, column: 10 }),
			Token::new(TokenType::RightParen, String::from(")"), Location { line: 1, column: 11 }),
		];
		
		let actual = tokenize(input);
		
		assert_eq!(expected, actual);
	}
	
	#[test]
	pub fn errors() {
		let input = ".,$(#";
		let expected = vec![
			Token::new(TokenType::Dot, String::from("."), Location { line: 1, column: 1 }),
			Token::new(TokenType::Comma, String::from(","), Location { line: 1, column: 2 }),
			Token::new(TokenType::Unknown, String::from("$"), Location { line: 1, column: 3 }),
			Token::new(TokenType::LeftParen, String::from("("), Location { line: 1, column: 4 }),
			Token::new(TokenType::Unknown, String::from("#"), Location { line: 1, column: 5 }),
		];
		
		let actual = tokenize(input);
		
		assert_eq!(expected, actual);
	}
	
	#[test]
	pub fn operators() {
		let input = "! != = == < <= > >=";
		let expected = vec![
			Token::new(TokenType::Bang, String::from("!"), Location { line: 1, column: 1 }),
			Token::new(TokenType::BangEqual, String::from("!="), Location { line: 1, column: 3 }),
			Token::new(TokenType::Equal, String::from("="), Location { line: 1, column: 6 }),
			Token::new(TokenType::EqualEqual, String::from("=="), Location { line: 1, column: 8 }),
			Token::new(TokenType::Less, String::from("<"), Location { line: 1, column: 11 }),
			Token::new(TokenType::LessEqual, String::from("<="), Location { line: 1, column: 13 }),
			Token::new(TokenType::Greater, String::from(">"), Location { line: 1, column: 16 }),
			Token::new(TokenType::GreaterEqual, String::from(">="), Location { line: 1, column: 18 }),
		];
		
		let actual = tokenize(input);
		
		assert_eq!(expected, actual);
	}
}
