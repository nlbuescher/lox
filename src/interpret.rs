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
pub use function::Function;
pub use scope::Scope;

use crate::error::Error;
use crate::location::Locate;
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
		env.execute(&*statement?)?;
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
			Value::Instance(instance) => TypeKind::Instance(instance.borrow().class.name.clone()),
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

	fn as_number(&self, expression: &Expression) -> Result<&f64, Error> {
		match self {
			Value::Number(n) => Ok(n),
			_ => Err(Error::type_error(expression.locate(), TypeKind::Number, self.type_kind())),
		}
	}

	fn as_string(&self, expression: &Expression) -> Result<&String, Error> {
		match self {
			Value::String(s) => Ok(s),
			_ => Err(Error::type_error(expression.locate(), TypeKind::String, self.type_kind())),
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

	fn capture_run(input: &str) -> String {
		let output = Rc::new(RefCell::new(Vec::new()));
		let mut env = Environment::with_output(output.clone());
		let result = run(input, &mut env, false);
		if let Err(error) = result {
			writeln!(output.borrow_mut(), "{error:#}").unwrap();
		}
		let output = output.borrow().clone();
		String::from_utf8(output).unwrap()
	}

	#[test]
	fn anonymous_functions() {
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

		let actual = capture_run(input);

		assert_eq!(expected, actual);
	}

	#[test]
	fn class() {
		let input = "\
class DevonshireCream {
  serveOn() {
    return \"Scones\";
  }
}

print DevonshireCream;
";
		let expected = "DevonshireCream\n";

		let actual = capture_run(input);

		assert_eq!(expected, actual);
	}

	#[test]
	fn instance() {
		let input = "\
class Bagel {}
var bagel = Bagel();
print bagel;
";
		let expected = "Bagel instance\n";

		let actual = capture_run(input);

		assert_eq!(expected, actual);
	}

	#[test]
	fn fields() {
		let input = "\
class Bagel {}
var bagel = Bagel();
bagel.toppings = 2;
print bagel.toppings;
";
		let expected = "2\n";

		let actual = capture_run(input);

		assert_eq!(expected, actual);
	}

	#[test]
	fn methods() {
		let input = "\
class Bacon {
	eat() {
		print \"Crunch crunch crunch!\";
	}
}

Bacon().eat();
";
		let expected = "Crunch crunch crunch!\n";

		let actual = capture_run(input);

		assert_eq!(expected, actual);
	}

	#[test]
	fn this() {
		let input = "\
class Thing {
  getCallback() {
    fun localFunction() {
      print this;
    }

    return localFunction;
  }
}

var callback = Thing().getCallback();
callback();
";
		let expected = "Thing instance\n";

		let actual = capture_run(input);

		assert_eq!(expected, actual);
	}
}
