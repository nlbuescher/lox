use std::io::{stdin, stdout, BufRead, Write};

use crate::error::Error;
use crate::parse::Parser;
use crate::tokenize::*;

mod error;
mod parse;
mod tokenize;

fn run(source: &str) -> Result<(), Error> {
	println!("Source:");
	println!("{source}");
	println!();

	let tokens = Tokens::new(source);

	println!("Tokenize:");
	for token in tokens.clone() {
		match token {
			Ok(token) => println!("{token}"),
			Err(error) => println!("{error}"),
		}
	}
	println!();

	let mut parser = Parser::new(tokens);
	let expression = parser.parse()?;

	println!("Parse: {expression}");
	print!("Evaluate: ");
	println!("{:?}", expression.evaluate()?);

	Ok(())
}

fn run_prompt() -> Result<(), Error> {
	print!("> ");
	stdout().flush()?;

	for line in stdin().lock().lines() {
		run(&line?)?;
		print!("> ");
		stdout().flush()?;
	}

	Ok(())
}

fn run_file(filename: &str) -> Result<(), Error> {
	let source = std::fs::read_to_string(filename).inspect_err(|error| {
		eprintln!("Unable to read file {filename}: {error}");
	})?;

	run(&source)
}

pub fn main() -> Result<(), Error> {
	let args = std::env::args().collect::<Vec<String>>();

	match args.len() {
		1 => run_prompt(),
		2 => run_file(&args[1]),
		_ => Err(Error::Usage(args[0].clone())),
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	pub fn test() {
		let input = "(\"1\" + \"2\") != \"3\"";

		if let Err(error) = run(input) {
			println!("{error}");
		}
	}
}
