mod callable;
mod class;
mod environment;
mod error;
mod function;
mod scope;
mod visit;

use std::fmt::{Debug, Display, Formatter};
use std::io::{stdin, BufRead, Write};

pub use callable::Callable;
pub use class::{Class, Instance};
pub use environment::Environment;
pub use error::RuntimeError;
pub use scope::Scope;

use crate::error::Error;
use crate::location::Locatable;
use crate::parse::{Expression, Parser};
use crate::tokenize::Tokens;
use crate::value::Value;

pub fn run_file(filename: &str, verbose: bool) -> Result<(), Error> {
	let source = std::fs::read_to_string(filename)?;
	let mut environment = Environment::default();
	run(&source, &mut environment, verbose)
}

pub fn run_prompt(verbose: bool) -> Result<(), Error> {
	let mut env = Environment::default();

	write!(env, "> ")?;
	env.flush()?;

	for line in stdin().lock().lines() {
		run(&line?, &mut env, verbose)?;
		write!(env, "> ")?;
		env.flush()?;
	}

	Ok(())
}

pub fn run(source: &str, env: &mut Environment, verbose: bool) -> Result<(), Error> {
	if verbose {
		writeln!(env, "Source:")?;
		writeln!(env, "{source}")?;
		writeln!(env)?;
	}

	let tokens = Tokens::new(source);

	if verbose {
		writeln!(env, "Tokenize:")?;
		for token in tokens.clone() {
			match token {
				Ok(token) => writeln!(env, "{token:#}")?,
				Err(error) => writeln!(env, "{error:#}")?,
			}
		}
		writeln!(env)?;
	}

	let parser = Parser::new(tokens);

	if verbose {
		writeln!(env, "Parse:")?;
		for statement in parser.clone() {
			writeln!(env, "{statement:#}", statement = statement?)?;
		}
		writeln!(env)?;

		writeln!(env, "Interpret:")?;
	}

	for statement in parser {
		env.execute(&statement?)?;
	}

	Ok(())
}

#[derive(Debug, Clone)]
pub enum TypeKind {
	Nothing,
	Bool,
	Number,
	String,
	Callable,
	Class,
	Instance(String),
}

impl Value {
	fn type_kind(&self) -> TypeKind {
		match self {
			Value::Nil => TypeKind::Nothing,
			Value::Bool(_) => TypeKind::Bool,
			Value::Number(_) => TypeKind::Number,
			Value::String(_) => TypeKind::String,
			Value::Function(_) => TypeKind::Callable,
			Value::Class(_) => TypeKind::Class,
			Value::Instance(Instance { class_name, .. }) => TypeKind::Instance(class_name.clone()),
		}
	}

	fn is_truthy(&self) -> bool {
		match self {
			Value::Nil => false,
			Value::Bool(value) => *value,
			Value::Number(value) => *value != 0.0,
			Value::String(value) => !value.is_empty(),
			Value::Function(_) => true,
			Value::Class(_) => true,
			Value::Instance(_) => true,
		}
	}

	fn as_number(&self, expression: &Expression) -> Result<&f64, RuntimeError> {
		match self {
			Value::Number(n) => Ok(n),
			_ => Err(RuntimeError::TypeError {
				location: expression.location().clone(),
				expected: TypeKind::Number,
				actual: self.type_kind(),
			}),
		}
	}

	fn as_string(&self, expression: &Expression) -> Result<&String, RuntimeError> {
		match self {
			Value::String(s) => Ok(s),
			_ => Err(RuntimeError::TypeError {
				location: expression.location().clone(),
				expected: TypeKind::String,
				actual: self.type_kind(),
			}),
		}
	}
}

impl TypeKind {
	fn as_str(&self) -> &str {
		match self {
			TypeKind::Nothing => "Nothing",
			TypeKind::Bool => "Bool",
			TypeKind::Number => "Number",
			TypeKind::String => "String",
			TypeKind::Callable => "Callable",
			TypeKind::Class => "Class",
			TypeKind::Instance(class_name) => class_name.as_str(),
		}
	}
}

impl Display for TypeKind {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		f.write_str(self.as_str())
	}
}

#[cfg(test)]
mod tests {
	use std::cell::RefCell;
	use std::rc::Rc;

	use super::*;
	use crate::error::Error;

	#[test]
	fn anonymous_functions() -> Result<(), Error> {
		let input = "\
fun thrice(fn) {
	for (var i = 1; i <= 3; i = i + 1) {
		fn(i);
	}
}

thrice(fun(a) {
	print a;
});
";
		let expected = "1\n2\n3\n";

		let output = Rc::new(RefCell::new(Vec::new()));
		let mut env = Environment::with_output(output.clone());
		run(input, &mut env, false)?;
		let actual = String::from_utf8(output.borrow().clone()).unwrap();

		assert_eq!(expected, actual);

		Ok(())
	}

	#[test]
	fn class() -> Result<(), Error> {
		let input = "\
class DevonshireCream {
  serveOn() {
    return \"Scones\";
  }
}

print DevonshireCream;
";
		let expected = "DevonshireCream\n";

		let output = Rc::new(RefCell::new(Vec::new()));
		let mut env = Environment::with_output(output.clone());
		run(input, &mut env, false)?;
		let actual = String::from_utf8(output.borrow().clone()).unwrap();

		assert_eq!(expected, actual);

		Ok(())
	}

	#[test]
	fn instance() -> Result<(), Error> {
		let input = "\
class Bagel {}
var bagel = Bagel();
print bagel;
";
		let expected = "Bagel instance\n";

		let output = Rc::new(RefCell::new(Vec::new()));
		let mut env = Environment::with_output(output.clone());
		run(input, &mut env, false)?;
		let actual = String::from_utf8(output.borrow().clone()).unwrap();

		assert_eq!(expected, actual);

		Ok(())
	}
}
