use std::fmt::{Display, Formatter};
use std::rc::Rc;

use crate::location::{Locatable, Location};
use crate::parse::Statement;
use crate::tokenize::Token;

#[derive(Debug, Clone)]
pub enum Expression {
	Assignment {
		name: Token,
		expression: Box<Expression>,
	},
	Binary {
		left: Box<Expression>,
		operator: Token,
		right: Box<Expression>,
	},
	Call {
		callee: Box<Expression>,
		open_paren: Token,
		arguments: Vec<Expression>,
		close_paren: Token,
	},
	Function {
		keyword: Token,
		open_paren: Token,
		parameters: Vec<Token>,
		close_paren: Token,
		body: Rc<Statement>,
	},
	Grouping(Box<Expression>),
	Literal(Token),
	Unary {
		operator: Token,
		expression: Box<Expression>,
	},
	Variable(Token),
}

impl Display for Expression {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		if f.alternate() {
			write!(f, "{location} ", location = self.location())?;
		}
		match self {
			Expression::Assignment { name: Token { text: name, .. }, expression } => {
				write!(f, "(assign {name} = {expression})")
			}

			Expression::Binary { left, operator: Token { text: operator, .. }, right } => {
				write!(f, "({operator} {left} {right})")
			}

			Expression::Call { callee, arguments, .. } => {
				write!(f, "(call {callee}(")?;
				for (index, argument) in arguments.iter().enumerate() {
					if index != 0 {
						write!(f, ", ")?;
					}
					write!(f, "{argument}")?;
				}
				write!(f, "))")
			}

			Expression::Function { parameters, .. } => {
				write!(f, "(fun (")?;
				for (index, Token { text: parameter, .. }) in parameters.iter().enumerate() {
					if index != 0 {
						write!(f, ", ")?;
					}
					write!(f, "{parameter}")?;
				}
				write!(f, "))")
			}

			Expression::Literal(Token { text: literal, .. }) => {
				write!(f, "{literal}")
			}

			Expression::Grouping(expression) => write!(f, "(group {expression})"),

			Expression::Unary { operator: Token { text: operator, .. }, expression } => {
				write!(f, "({operator} {expression})")
			}

			Expression::Variable(Token { text: name, .. }) => {
				write!(f, "(var {name})")
			}
		}
	}
}

impl Locatable for Expression {
	fn location(&self) -> &Location {
		match self {
			Expression::Assignment { name, .. } => name.location(),
			Expression::Binary { left, .. } => left.location(),
			Expression::Call { callee, .. } => callee.location(),
			Expression::Function { keyword, .. } => keyword.location(),
			Expression::Grouping(expression) => expression.location(),
			Expression::Literal(literal) => literal.location(),
			Expression::Unary { operator, .. } => operator.location(),
			Expression::Variable(name) => name.location(),
		}
	}
}
