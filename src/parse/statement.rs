use std::fmt::{Display, Formatter};
use std::rc::Rc;

use crate::location::{Locatable, Location};
use crate::parse::Expression;
use crate::tokenize::Token;

#[derive(Debug, Clone)]
pub enum Statement {
	Block {
		open_brace: Token,
		statements: Vec<Statement>,
		close_brace: Token,
	},
	ClassDeclaration {
		keyword: Token,
		name: Token,
		open_brace: Token,
		methods: Vec<Statement>,
		close_brace: Token,
	},
	Expression(Box<Expression>),
	For {
		keyword: Token,
		initializer: Option<Box<Statement>>,
		condition: Option<Box<Expression>>,
		increment: Option<Box<Statement>>,
		body: Box<Statement>,
	},
	FunctionDeclaration {
		keyword: Token,
		name: Token,
		parameters: Vec<Token>,
		body: Rc<Statement>,
	},
	If {
		keyword: Token,
		condition: Box<Expression>,
		then_branch: Box<Statement>,
		else_branch: Option<(Token, Box<Statement>)>,
	},
	Print {
		keyword: Token,
		expression: Box<Expression>,
	},
	Return {
		keyword: Token,
		expression: Option<Box<Expression>>,
	},
	VariableDeclaration {
		keyword: Token,
		name: Token,
		initializer: Option<Box<Expression>>,
	},
	While {
		keyword: Token,
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
					write!(f, "{location} ", location = open_brace.location())?;
				}
				write!(f, "{PAD:width$}{{\n")?;

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
					write!(f, "{location} ", location = close_brace.location())?;
				}
				write!(f, "{PAD:width$}}}")
			}

			Statement::ClassDeclaration {
				keyword,
				name: Token { text: name, .. },
				open_brace,
				methods,
				close_brace,
			} => {
				if f.alternate() {
					write!(f, "{location} ", location = keyword.location())?;
				}

				write!(f, "{PAD:width$}class {name}")?;

				if f.alternate() {
					write!(f, "{location} ", location = open_brace.location())?;
				}
				write!(f, "{PAD:width$}{{\n")?;

				{
					let width = width + 1;

					for method in methods {
						write!(f, "{method:width$}\n")?;
					}
				}

				if f.alternate() {
					write!(f, "{location} ", location = close_brace.location())?;
				}
				write!(f, "{PAD:width$}}}")
			}

			Statement::Expression(expression) => {
				if f.alternate() {
					write!(f, "{location} ", location = expression.location())?;
				}
				write!(f, "{PAD:width$}(expr {expression})")
			}

			Statement::For { keyword, initializer, condition, increment, body } => {
				let initializer = initializer.as_ref().map_or(String::new(), |it| it.to_string());
				let condition = condition.as_ref().map_or(String::new(), |it| it.to_string());
				let increment = increment.as_ref().map_or(String::new(), |it| it.to_string());

				if f.alternate() {
					write!(f, "{location} ", location = keyword.location())?;
				}

				write!(f, "{PAD:width$}for({initializer}; {condition}; {increment})")?;

				if f.alternate() {
					write!(f, "{body:#width$}")
				} else {
					write!(f, "{body:width$}")
				}
			}

			Statement::FunctionDeclaration {
				keyword,
				name: Token { text: name, .. },
				parameters,
				body,
			} => {
				if f.alternate() {
					write!(f, "{location} ", location = keyword.location())?;
				}
				write!(f, "{PAD:width$}fun {name}(")?;

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

			Statement::If { keyword, condition, then_branch, else_branch } => {
				if f.alternate() {
					write!(f, "{location} ", location = keyword.location())?;
				}

				write!(f, "{PAD:width$}if {condition}\n")?;

				if f.alternate() {
					write!(f, "{then_branch:#width$}")?;
				} else {
					write!(f, "{then_branch:width$}")?;
				}

				if let Some((keyword, else_branch)) = else_branch {
					write!(f, "\n")?;

					if f.alternate() {
						write!(f, "{location}", location = keyword.location())?;
					}

					write!(f, "{PAD:width$}else\n")?;

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
					write!(f, "{location} ", location = keyword.location())?;
				}
				write!(f, "{PAD:width$}(print {expression})")
			}

			Statement::Return { keyword, expression } => {
				if f.alternate() {
					write!(f, "{location} ", location = keyword.location())?;
				}

				write!(f, "{PAD:width$}")?;

				if let Some(expression) = expression {
					write!(f, "(return {expression})")
				} else {
					write!(f, "(return)")
				}
			}

			Statement::VariableDeclaration {
				keyword,
				name: Token { text: name, .. },
				initializer,
			} => {
				if f.alternate() {
					write!(f, "{location} ", location = keyword.location())?;
				}

				write!(f, "{PAD:width$}")?;

				match initializer {
					Some(expression) => {
						write!(f, "(vardecl {name} = {expression})")
					}

					None => write!(f, "(vardecl {name})"),
				}
			}

			Statement::While { keyword, condition, body } => {
				if f.alternate() {
					write!(f, "{location} ", location = keyword.location())?;
				}

				write!(f, "{PAD:width$}while {condition}\n")?;

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
			Statement::Block { open_brace, .. } => open_brace.location(),
			Statement::ClassDeclaration { keyword, .. } => keyword.location(),
			Statement::Expression(expression) => expression.location(),
			Statement::For { keyword, .. } => keyword.location(),
			Statement::FunctionDeclaration { keyword, .. } => keyword.location(),
			Statement::If { keyword, .. } => keyword.location(),
			Statement::Print { keyword, .. } => keyword.location(),
			Statement::Return { keyword, .. } => keyword.location(),
			Statement::VariableDeclaration { name, .. } => name.location(),
			Statement::While { keyword, .. } => keyword.location(),
		}
	}
}
