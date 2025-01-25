use std::fmt::{Display, Formatter};
use std::io::{stdout, Write};
use std::rc::Rc;

use crate::location::{Locatable, Location};
use crate::parse::Expression;
use crate::tokenize::Token;

#[derive(Debug, Clone)]
pub enum Statement {
	Block {
		start_location: Location,
		end_location: Location,
		statements: Vec<Statement>,
	},
	Expression(Box<Expression>),
	For {
		location: Location,
		initializer: Option<Box<Statement>>,
		condition: Option<Box<Expression>>,
		increment: Option<Box<Statement>>,
		body: Box<Statement>,
	},
	FunctionDeclaration {
		location: Location,
		name: Token,
		parameters: Vec<Token>,
		body: Rc<Statement>,
	},
	If {
		if_location: Location,
		condition: Box<Expression>,
		then_branch: Box<Statement>,
		else_branch: Option<(Location, Box<Statement>)>,
	},
	Print {
		location: Location,
		expression: Box<Expression>,
	},
	Return {
		location: Location,
		expression: Option<Box<Expression>>,
	},
	VariableDeclaration {
		location: Location,
		name: Token,
		initializer: Option<Box<Expression>>,
	},
	While {
		location: Location,
		condition: Box<Expression>,
		body: Box<Statement>,
	},
}

impl Display for Statement {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		let width = f.width().unwrap_or(0);

		match self {
			Statement::Block { start_location, end_location, statements, .. } => {
				if f.alternate() {
					write!(f, "{start_location:width$} ")?;
				}
				write!(f, "{{\n")?;
				stdout().flush().unwrap();

				{
					let width = width + 1;
					for statement in statements {
						if f.alternate() {
							write!(f, "{statement:#width$}\n")?;
						} else {
							write!(f, "{statement:width$}\n")?;
						}
					}
				}

				if f.alternate() {
					write!(f, "{end_location:width$} ")?;
				}
				write!(f, "}}")
			}

			Statement::Expression(expression) => {
				if f.alternate() {
					write!(f, "{location:width$} ", location = expression.location())?;
				}
				write!(f, "(expr {expression})")
			}

			Statement::For { location, initializer, condition, increment, body } => {
				let initializer = initializer.as_ref().map_or(String::new(), |it| it.to_string());
				let condition = condition.as_ref().map_or(String::new(), |it| it.to_string());
				let increment = increment.as_ref().map_or(String::new(), |it| it.to_string());

				if f.alternate() {
					write!(f, "{location:width$} ")?;
				}

				write!(f, "for ({initializer}; {condition}; {increment})\n")?;

				if f.alternate() {
					write!(f, "{body:#width$}")
				} else {
					write!(f, "{body:width$}")
				}
			}

			Statement::FunctionDeclaration {
				location,
				name: Token { text: name, .. },
				parameters,
				body,
			} => {
				if f.alternate() {
					write!(f, "{location:width$} ")?;
				}

				write!(f, "fun {name}(")?;

				for (index, Token { text: parameter, .. }) in parameters.iter().enumerate() {
					if index != 0 {
						write!(f, ", ")?;
					}

					write!(f, "{parameter}")?;
				}

				write!(f, ")\n")?;

				if f.alternate() {
					write!(f, "{body:#width$}")
				} else {
					write!(f, "{body:width$}")
				}
			}

			Statement::If { if_location, condition, then_branch, else_branch } => {
				if f.alternate() {
					write!(f, "{if_location:width$} ")?;
				}

				write!(f, "if {condition}\n")?;

				if f.alternate() {
					write!(f, "{then_branch:#width$}")?;
				} else {
					write!(f, "{then_branch:width$}")?;
				}

				if let Some((else_location, else_branch)) = else_branch {
					if f.alternate() {
						write!(f, "\n{else_location:width$} ")?;
					}

					write!(f, "\nelse\n")?;

					if f.alternate() {
						write!(f, "{else_branch:#width$}")?;
					} else {
						write!(f, "{else_branch:width$}")?;
					}
				}
				Ok(())
			}

			Statement::Print { location, expression } => {
				if f.alternate() {
					write!(f, "{location:width$} ")?;
				}
				write!(f, "(print {expression})")
			}

			Statement::Return { location, expression: value } => {
				if f.alternate() {
					write!(f, "{location:width$} ")?;
				}
				if let Some(value) = value {
					write!(f, "(return {value})")
				} else {
					write!(f, "(return)")
				}
			}

			Statement::VariableDeclaration {
				location,
				name: Token { text: name, .. },
				initializer,
			} => {
				if f.alternate() {
					write!(f, "{location:width$} ")?;
				}
				match initializer {
					Some(expression) => {
						write!(f, "(vardecl {name} = {expression})")
					}

					None => write!(f, "(vardecl {name})"),
				}
			}

			Statement::While { location, condition, body } => {
				if f.alternate() {
					write!(f, "{location:width$} ")?;
				}

				write!(f, "while {condition}\n")?;

				if f.alternate() {
					write!(f, "{body:#width$}")
				} else {
					write!(f, "{body:width$}")
				}
			}
		}
	}
}

impl Locatable for Statement {
	fn location(&self) -> &Location {
		match self {
			Statement::Block { start_location, .. } => start_location,
			Statement::Expression(expression) => expression.location(),
			Statement::For { location, .. } => location,
			Statement::FunctionDeclaration { location, .. } => location,
			Statement::If { if_location: location, .. } => location,
			Statement::Print { location, .. } => location,
			Statement::Return { location, .. } => location,
			Statement::VariableDeclaration { name, .. } => name.location(),
			Statement::While { location, .. } => location,
		}
	}
}
