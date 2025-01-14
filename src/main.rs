use std::io::{stdin, stdout, BufRead, Write};
use std::process::ExitCode;

use crate::error::Error;
use crate::interpret::Interpreter;
use crate::parse::Parser;
use crate::tokenize::*;

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
	let mut interpreter = Interpreter::new();
	run(&source, &mut interpreter)
}

fn run_prompt() -> Result<(), Error> {
	let mut interpreter = Interpreter::new();

	print!("> ");
	stdout().flush()?;

	for line in stdin().lock().lines() {
		run(&line?, &mut interpreter)?;
		print!("> ");
		stdout().flush()?;
	}

	Ok(())
}

fn run(source: &str, interpreter: &mut Interpreter) -> Result<(), Error> {
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
			Err(error) => println!("{error}"),
		}
	}
	println!();

	println!("Interpret:");
	for statement in parser {
		let statement = statement?;
		interpreter.execute(&statement)?;
	}
	println!();

	Ok(())
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	pub fn test() {
		let input = "print 5; \"test\"; print((\"1\" + \"2\") == \"12\");";

		let mut interpreter = Interpreter::new();

		if let Err(error) = run(input, &mut interpreter) {
			println!("{error}");
		}
	}
}
