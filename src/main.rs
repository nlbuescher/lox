use std::io::{stdin, stdout, BufRead, Write};
use std::process::ExitCode;

use crate::error::Error;
use crate::interpret::Environment;
use crate::location::Locatable;
use crate::parse::Parser;
use crate::tokenize::Tokens;

mod error;
mod interpret;
mod location;
mod parse;
mod tokenize;
mod value;

pub fn main() -> ExitCode {
	let args = std::env::args().collect::<Vec<String>>();

	let result = match args.len() {
		2 => run_file(&args[1]),
		1 => run_prompt(),
		_ => Err(Error::BadUsage(args)),
	};

	result.map_or_else(
		|error| {
			eprintln!("{location} {error}", location = error.location());

			match error {
				Error::BadUsage(_) => ExitCode::from(64),
				Error::Io(_) | Error::Parse(_) => ExitCode::from(65),
				Error::Runtime(_) => ExitCode::from(70),
			}
		},
		|_| ExitCode::SUCCESS,
	)
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
	println!("Source:");
	println!("{source}");
	println!();

	let tokens = Tokens::new(source);

	println!("Tokenize:");
	for token in tokens.clone() {
		match token {
			Ok(token) => println!("{location} {token}", location = token.location()),
			Err(error) => println!("{location} {error}", location = error.location()),
		}
	}
	println!();

	let parser = Parser::new(tokens);

	println!("Parse:");
	for statement in parser.clone() {
		match statement {
			Ok(statement) => println!("{location} {statement}", location = statement.location()),
			Err(error) => {
				println!("{location} {error}\n", location = error.location());
				return Ok(());
			}
		}
	}
	println!();

	println!("Interpret:");
	for statement in parser {
		let result = statement
			.map_err(Error::from)
			.map(|statement| environment.execute(&statement).map_err(Error::from));

		if let Err(error) = result {
			println!("{location} {error}", location = error.location());
			break;
		}
	}
	println!();

	Ok(())
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	pub fn test() {
		let input = "var a = \"420\"; var b = \"69\"; print a + b;";

		let mut environment = Environment::new();

		if let Err(error) = run(input, &mut environment) {
			println!("{location} {error}", location = error.location());
		}
	}
}
