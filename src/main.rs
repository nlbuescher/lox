use std::io::{stdin, stdout, BufRead, Write};
use std::process::ExitCode;

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

pub fn main() -> ExitCode {
	let args = std::env::args().collect::<Vec<String>>();

	let result = match args.len() {
		2 => run_file(&args[1]),
		1 => run_prompt(),
		_ => Err(Error::BadUsage(args)),
	};

	result.map_or_else(
		|error| {
			eprintln!("{error}");

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
			Ok(token) => println!("{token}"),
			Err(error) => println!("{error}"),
		}
	}
	println!();

	let parser = Parser::new(tokens);

	println!("Parse:");
	for statement in parser.clone() {
		match statement {
			Ok(statement) => println!("{statement}"),
			Err(error) => {
				println!("{error}\n");
				return Ok(());
			}
		}
	}
	println!();

	println!("Interpret:");
	for statement in parser {
		let result = statement.map_or_else(
			|error| Err(Error::from(error)),
			|statement| Ok(environment.execute(&statement)?),
		);

		if let Err(error) = result {
			println!("{error}");
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
		let input = "var a = 60; var b = 9; print a + b;";

		let mut environment = Environment::new();

		if let Err(error) = run(input, &mut environment) {
			println!("{error}");
		}
	}
}
