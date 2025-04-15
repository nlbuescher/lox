use std::fmt::{Display, Formatter};
use std::rc::Rc;

use crate::location::{Locate, Location};
use crate::parse::Expression;
use crate::tokenize::Token;

/////////////////////////////////////////////////////////////////////////////
// Struct declarations
/////////////////////////////////////////////////////////////////////////////

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

	Expression {
		expression: Box<Expression>,
	},

	For {
		keyword: Token,
		initializer: Option<Box<Statement>>,
		condition: Option<Box<Expression>>,
		increment: Option<Box<Statement>>,
		body: Box<Statement>,
	},

	FunctionDeclaration {
		keyword: Option<Token>,
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

/////////////////////////////////////////////////////////////////////////////
// Display
/////////////////////////////////////////////////////////////////////////////

const PAD: &str = "";

impl Display for Statement {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		let width = f.width().unwrap_or(0);

		if f.alternate() {
			write!(f, "{location} ", location = self.locate())?;
		}

		match self {
			Statement::Block { statements, close_brace, .. } => {
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

			Statement::ClassDeclaration {
				name: Token { text: name, .. },
				open_brace,
				methods,
				close_brace,
				..
			} => {
				writeln!(f, "{PAD:width$}class {name}")?;

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

			Statement::Expression { expression } => {
				write!(f, "{PAD:width$}(expr {expression})")
			}

			Statement::For { initializer, condition, increment, body, .. } => {
				let initializer = initializer.as_ref().map_or(String::new(), |it| it.to_string());
				let condition = condition.as_ref().map_or(String::new(), |it| it.to_string());
				let increment = increment.as_ref().map_or(String::new(), |it| it.to_string());

				write!(f, "{PAD:width$}for({initializer}; {condition}; {increment})")?;

				if f.alternate() {
					write!(f, "{body:#width$}")
				} else {
					write!(f, "{body:width$}")
				}
			}

			Statement::FunctionDeclaration {
				name: Token { text: name, .. },
				parameters,
				body,
				..
			} => {
				write!(f, "{PAD:width$}fun {name}(")?;

				for (index, Token { text: parameter, .. }) in parameters.iter().enumerate() {
					if index != 0 {
						write!(f, ", ")?;
					}
					write!(f, "{parameter}")?;
				}

				writeln!(f, ")")?;

				if f.alternate() {
					write!(f, "{body:#width$}")
				} else {
					write!(f, "{body:width$}")
				}
			}

			Statement::If { condition, then_branch, else_branch, .. } => {
				writeln!(f, "{PAD:width$}if {condition}")?;

				if f.alternate() {
					write!(f, "{then_branch:#width$}")?;
				} else {
					write!(f, "{then_branch:width$}")?;
				}

				if let Some((keyword, else_block)) = &else_branch {
					writeln!(f)?;

					if f.alternate() {
						write!(f, "{location}", location = keyword.locate())?;
					}

					writeln!(f, "{PAD:width$}else")?;

					if f.alternate() {
						write!(f, "{else_block:#width$}")?;
					} else {
						write!(f, "{else_block:width$}")?;
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

				if let Some(expression) = &expression {
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
				match &initializer {
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
			Statement::Expression { expression } => expression.locate(),
			Statement::For { keyword, .. } => keyword.locate(),
			Statement::If { keyword, .. } => keyword.locate(),
			Statement::FunctionDeclaration { keyword, name, .. } => {
				if let Some(keyword) = keyword {
					keyword.locate()
				} else {
					name.locate()
				}
			}
			Statement::Print { keyword, .. } => keyword.locate(),
			Statement::Return { keyword, .. } => keyword.locate(),
			Statement::VariableDeclaration { keyword, .. } => keyword.locate(),
			Statement::While { keyword, .. } => keyword.locate(),
		}
	}
}
