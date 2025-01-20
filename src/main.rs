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
	let args = std::env::args().map(Box::from).collect::<Vec<Box<str>>>();

	let result = match args.len() {
		1 => run_prompt(false),
		2 => run_file(&args[1], false),
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

fn run_file(filename: &str, verbose: bool) -> Result<(), Error> {
	let source = std::fs::read_to_string(filename)?;
	let mut environment = Environment::new();
	run(&source, &mut environment, verbose)
}

fn run_prompt(verbose: bool) -> Result<(), Error> {
	let mut environment = Environment::new();

	print!("> ");
	stdout().flush()?;

	for line in stdin().lock().lines() {
		run(&line?, &mut environment, verbose)?;
		print!("> ");
		stdout().flush()?;
	}

	Ok(())
}

fn run(source: &str, environment: &mut Environment, verbose: bool) -> Result<(), Error> {
	if verbose {
		println!("Source:");
		println!("{source}");
		println!();
	}

	let tokens = Tokens::new(source);

	if verbose {
		println!("Tokenize:");
		for token in tokens.clone() {
			match token {
				Ok(token) => println!("{token:#}"),
				Err(error) => println!("{error:#}"),
			}
		}
		println!();
	}

	let parser = Parser::new(tokens);

	if verbose {
		println!("Parse:");
		for statement in parser.clone() {
			println!("{statement:#}", statement = statement?);
		}
		println!();

		println!("Interpret:");
	}

	for statement in parser {
		environment.execute(&statement?)?;
	}

	Ok(())
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	pub fn test() {
		let input = r#"
for (var i = 0; i < 10; i = i + 1) {		
	print i;
}
print clock();"#;

		let mut environment = Environment::new();

		if let Err(error) = run(input, &mut environment, true) {
			println!("{error:#}");
		}
	}
}
