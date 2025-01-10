use crate::error::Error;
use crate::tokenize::*;
use std::io::BufRead;

mod error;
mod tokenize;

pub fn main() -> Result<(), Error> {
	let args = std::env::args().collect::<Vec<String>>();

	match args.len() {
		1 => run_prompt(),
		2 => run_file(&args[1]),
		_ => Err(Error::Usage(args[0].clone())),
	}
}

fn run_prompt() -> Result<(), Error> {
	for line in std::io::stdin().lock().lines() {
		run(&line?)?
	}

	Ok(())
}

fn run_file(filename: &str) -> Result<(), Error> {
	let source = std::fs::read_to_string(filename).inspect_err(|error| {
		eprintln!("Unable to read file {filename}: {error}");
	})?;

	run(&source)
}

fn run(source: &str) -> Result<(), Error> {
	for token in Tokenizer::new(source) {
		match token {
			Ok(token) => println!("{token}"),
			Err(error) => eprintln!("{error}"),
		}
	}

	Ok(())
}

mod tests {
	use super::*;

	#[test]
	pub fn test() -> Result<(), Error> {
		let input = "#\r\n   \t(()";
		println!("input: \n```\n{input}\n```");
		run(input)
	}
}
