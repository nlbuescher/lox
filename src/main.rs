use tokenize::tokenize;
use crate::token::{TokenType};

mod tokenize;
mod token;

pub fn main() {
	let args = std::env::args().collect::<Vec<String>>();

	if args.len() < 3 {
		eprintln!("Usage: {} tokenize <filename>", args[0]);
		return;
	}

	let command = &args[1];
	
	match command.as_str() {
		"tokenize" => {
			let filename = &args[2];
			
			let source = std::fs::read_to_string(filename).unwrap_or_else(|error| {
				eprintln!("Unable to read file {filename}: {error}");
				String::new()
			});
			
			for token in tokenize(&source) {
				if token.token_type == TokenType::Unknown {
					eprintln!("{token}");
				}
				else {
					println!("{token}");
				}
			}
		}
		_ => {
			eprintln!("Unknown command: {}", command);
		}
	}
}
