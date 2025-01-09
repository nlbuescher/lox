use crate::tokenize::*;

mod tokenize;

pub fn main() {
	let args = std::env::args().collect::<Vec<String>>();

	if args.len() < 3 {
		eprintln!("Usage: {} tokenize <filename>", args[0]);
		return;
	}

	let filename = &args[2];

	let source = std::fs::read_to_string(filename).unwrap_or_else(|error| {
		eprintln!("Unable to read file {filename}: {error}");
		String::new()
	});

	let command = &args[1];

	match command.as_str() {
		"tokenize" => {
			for token in tokenize(&source) {
				match token {
					AnnotatedToken {
						token: Token::UnknownChar { .. } | Token::UnterminatedString { .. },
						..
					} => {
						eprintln!("{token}")
					}

					_ => println!("{token}"),
				}
			}
		}

		_ => {
			eprintln!("Unknown command: {}", command);
		}
	}
}
