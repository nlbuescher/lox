use clap::Parser as ArgParser;
use lox::error::Error;
use lox::{run_file, run_prompt};

#[derive(Debug, clap::Parser)]
#[command(name = "lox", about = "Lox Interpreter", version)]
struct Args {
	// File to run
	#[arg(short, long)]
	filename: Option<String>,

	// Print tokens and AST
	#[arg(short, long)]
	verbose: bool,
}

pub fn main() -> Result<(), Error> {
	let args = Args::parse();

	if let Some(ref filename) = args.filename {
		run_file(filename, args.verbose)
	} else {
		run_prompt(false)
	}
}
