use std::fmt::{Display, Formatter};
use std::rc::Rc;

use crate::location::{Locate, Location};
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
	Get {
		object: Box<Expression>,
		property: Token,
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
			write!(f, "{location} ", location = self.locate())?;
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

			Expression::Get { object, property: Token { text: property, .. } } => {
				write!(f, "{object}.{property}")
			}

			Expression::Grouping(expression) => write!(f, "(group {expression})"),

			Expression::Literal(Token { text: literal, .. }) => {
				write!(f, "{literal}")
			}

			Expression::Unary { operator: Token { text: operator, .. }, expression } => {
				write!(f, "({operator} {expression})")
			}

			Expression::Variable(Token { text: name, .. }) => {
				write!(f, "(var {name})")
			}
		}
	}
}

impl Locate for Expression {
	fn locate(&self) -> &Location {
		match self {
			Expression::Assignment { name, .. } => name.locate(),
			Expression::Binary { left, .. } => left.locate(),
			Expression::Call { callee, .. } => callee.locate(),
			Expression::Function { keyword, .. } => keyword.locate(),
			Expression::Get { property, .. } => property.locate(),
			Expression::Grouping(expression) => expression.locate(),
			Expression::Literal(literal) => literal.locate(),
			Expression::Unary { operator, .. } => operator.locate(),
			Expression::Variable(name) => name.locate(),
		}
	}
}
