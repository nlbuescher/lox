use std::io::{stdin, stdout, BufRead, Write};

use crate::error::Error;
use crate::interpret::Environment;
use crate::parse::Parser;
use crate::tokenize::Tokens;

mod error;
mod interpret;
mod location;
mod parse;
mod tokenize;
mod value;

pub fn main() {
	let args = std::env::args().collect::<Vec<String>>();

	let result = match args.len() {
		1 => run_prompt(),
		2 => run_file(&args[1]),
		_ => {
			eprintln!("Usage: {} <filename>", args[0]);
			std::process::exit(64)
		}
	};

	if let Err(error) = result {
		eprintln!("{error:#}");

		match error {
			Error::Io(_) | Error::Parse(_) => std::process::exit(65),
			Error::Runtime(_) => std::process::exit(70),
		}
	}
}

fn run_file(filename: &str) -> Result<(), Error> {
	let source = std::fs::read_to_string(filename)?;
	let mut interpreter = Environment::new();
	run(&source, &mut interpreter)
}

fn run_prompt() -> Result<(), Error> {
	let mut environment = Environment::new();

	print!("> ");
	stdout().flush()?;

	for line in stdin().lock().lines() {
		run(&line?, &mut environment)?;
		print!("> ");
		stdout().flush()?;
	}

	Ok(())
}

fn run(source: &str, environment: &mut Environment) -> Result<(), Error> {
	let width = 7;

	println!("Source:");
	println!("{source}");
	println!();

	let tokens = Tokens::new(source);

	println!("Tokenize:");
	for token in tokens.clone() {
		match token {
			Ok(token) => println!("{token:#width$}"),
			Err(error) => println!("{error:#width$}"),
		}
	}
	println!();

	let parser = Parser::new(tokens);

	println!("Parse:");
	for statement in parser.clone() {
		println!("{statement:#width$}", statement = statement?);
	}
	println!();

	println!("Interpret:");
	for statement in parser {
		environment.execute(&statement?)?;
	}
	println!();

	Ok(())
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	pub fn test() {
		let input = "{ { var a = \"420\"; var b = \"69\"; print a + b; } } print a;";

		let mut environment = Environment::new();

		if let Err(error) = run(input, &mut environment) {
			println!("{error:#}");
		}
	}
}
