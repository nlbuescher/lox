use std::fmt::{Display, Formatter};
use std::rc::Rc;

use crate::location::{Locate, Location};
use crate::parse::Expression;
use crate::tokenize::Token;

#[derive(Debug, Clone)]
pub enum Statement {
	Block {
		open_brace: Box<Token>,
		statements: Vec<Statement>,
		close_brace: Box<Token>,
	},
	ClassDeclaration {
		keyword: Box<Token>,
		name: Box<Token>,
		open_brace: Box<Token>,
		methods: Vec<Statement>,
		close_brace: Box<Token>,
	},
	Expression(Box<Expression>),
	For {
		keyword: Box<Token>,
		initializer: Option<Box<Statement>>,
		condition: Option<Box<Expression>>,
		increment: Option<Box<Statement>>,
		body: Box<Statement>,
	},
	FunctionDeclaration {
		keyword: Box<Token>,
		name: Box<Token>,
		parameters: Vec<Token>,
		body: Rc<Statement>,
	},
	If {
		keyword: Box<Token>,
		condition: Box<Expression>,
		then_branch: Box<Statement>,
		else_branch: Option<(Token, Box<Statement>)>,
	},
	Print {
		keyword: Box<Token>,
		expression: Box<Expression>,
	},
	Return {
		keyword: Box<Token>,
		expression: Option<Box<Expression>>,
	},
	VariableDeclaration {
		keyword: Box<Token>,
		name: Box<Token>,
		initializer: Option<Box<Expression>>,
	},
	While {
		keyword: Box<Token>,
		condition: Box<Expression>,
		body: Box<Statement>,
	},
}

impl Display for Statement {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		const PAD: &str = "";
		let width = f.width().unwrap_or(0);

		match self {
			Statement::Block { open_brace, statements, close_brace, .. } => {
				if f.alternate() {
					write!(f, "{location} ", location = open_brace.locate())?;
				}
				writeln!(f, "{PAD:width$}{{")?;

				{
					let width = width + 1;
					for statement in statements {
						if f.alternate() {
							writeln!(f, "{statement:#width$}")?;
						} else {
							writeln!(f, "{statement:width$}")?;
						}
					}
				}

				if f.alternate() {
					write!(f, "{location} ", location = close_brace.locate())?;
				}
				write!(f, "{PAD:width$}}}")
			}

			Statement::ClassDeclaration { keyword, name, open_brace, methods, close_brace } => {
				if f.alternate() {
					write!(f, "{location} ", location = keyword.locate())?;
				}

				writeln!(f, "{PAD:width$}class {name}", name = name.text)?;

				if f.alternate() {
					write!(f, "{location} ", location = open_brace.locate())?;
				}
				writeln!(f, "{PAD:width$}{{")?;

				{
					let width = width + 1;

					for method in methods {
						writeln!(f, "{method:width$}")?;
					}
				}

				if f.alternate() {
					write!(f, "{location} ", location = close_brace.locate())?;
				}
				write!(f, "{PAD:width$}}}")
			}

			Statement::Expression(expression) => {
				if f.alternate() {
					write!(f, "{location} ", location = expression.locate())?;
				}
				write!(f, "{PAD:width$}(expr {expression})")
			}

			Statement::For { keyword, initializer, condition, increment, body } => {
				let initializer = initializer.as_ref().map_or(String::new(), |it| it.to_string());
				let condition = condition.as_ref().map_or(String::new(), |it| it.to_string());
				let increment = increment.as_ref().map_or(String::new(), |it| it.to_string());

				if f.alternate() {
					write!(f, "{location} ", location = keyword.locate())?;
				}

				write!(f, "{PAD:width$}for({initializer}; {condition}; {increment})")?;

				if f.alternate() {
					write!(f, "{body:#width$}")
				} else {
					write!(f, "{body:width$}")
				}
			}

			Statement::FunctionDeclaration { keyword, name, parameters, body } => {
				if f.alternate() {
					write!(f, "{location} ", location = keyword.locate())?;
				}
				write!(f, "{PAD:width$}fun {name}(", name = name.text)?;

				for (index, parameter) in parameters.iter().enumerate() {
					if index != 0 {
						write!(f, ", ")?;
					}
					write!(f, "{parameter}", parameter = parameter.text)?;
				}

				writeln!(f, ")")?;

				if f.alternate() {
					write!(f, "{body:#width$}")
				} else {
					write!(f, "{body:width$}")
				}
			}

			Statement::If { keyword, condition, then_branch, else_branch } => {
				if f.alternate() {
					write!(f, "{location} ", location = keyword.locate())?;
				}

				writeln!(f, "{PAD:width$}if {condition}")?;

				if f.alternate() {
					write!(f, "{then_branch:#width$}")?;
				} else {
					write!(f, "{then_branch:width$}")?;
				}

				if let Some((keyword, else_branch)) = else_branch {
					writeln!(f)?;

					if f.alternate() {
						write!(f, "{location}", location = keyword.locate())?;
					}

					writeln!(f, "{PAD:width$}else")?;

					if f.alternate() {
						write!(f, "{else_branch:#width$}")?;
					} else {
						write!(f, "{else_branch:width$}")?;
					}
				}
				Ok(())
			}

			Statement::Print { keyword, expression } => {
				if f.alternate() {
					write!(f, "{location} ", location = keyword.locate())?;
				}
				write!(f, "{PAD:width$}(print {expression})")
			}

			Statement::Return { keyword, expression } => {
				if f.alternate() {
					write!(f, "{location} ", location = keyword.locate())?;
				}

				write!(f, "{PAD:width$}")?;

				if let Some(expression) = expression {
					write!(f, "(return {expression})")
				} else {
					write!(f, "(return)")
				}
			}

			Statement::VariableDeclaration { keyword, name, initializer } => {
				if f.alternate() {
					write!(f, "{location} ", location = keyword.locate())?;
				}

				write!(f, "{PAD:width$}")?;

				let name = &name.text;
				match initializer {
					Some(expression) => {
						write!(f, "(vardecl {name} = {expression})")
					}

					None => write!(f, "(vardecl {name})"),
				}
			}

			Statement::While { keyword, condition, body } => {
				if f.alternate() {
					write!(f, "{location} ", location = keyword.locate())?;
				}

				writeln!(f, "{PAD:width$}while {condition}")?;

				if f.alternate() {
					write!(f, "{body:#width$}")
				} else {
					write!(f, "{body:width$}")
				}
			}
		}
	}
}

impl Locate for Statement {
	fn locate(&self) -> &Location {
		match self {
			Statement::Block { open_brace, .. } => open_brace.locate(),
			Statement::ClassDeclaration { keyword, .. } => keyword.locate(),
			Statement::Expression(expression) => expression.locate(),
			Statement::For { keyword, .. } => keyword.locate(),
			Statement::FunctionDeclaration { keyword, .. } => keyword.locate(),
			Statement::If { keyword, .. } => keyword.locate(),
			Statement::Print { keyword, .. } => keyword.locate(),
			Statement::Return { keyword, .. } => keyword.locate(),
			Statement::VariableDeclaration { name, .. } => name.locate(),
			Statement::While { keyword, .. } => keyword.locate(),
		}
	}
}
