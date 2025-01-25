use std::fmt::{Display, Formatter};
use std::rc::Rc;

use crate::location::{Locatable, Location};
use crate::parse::Statement;
use crate::tokenize::Token;

#[derive(Debug, Clone)]
pub enum Expression {
	Assignment { name: Token, value: Box<Expression> },
	Binary { left: Box<Expression>, operator: Token, right: Box<Expression> },
	Call { callee: Box<Expression>, arguments_start_location: Location, arguments: Vec<Expression> },
	Function { location: Location, parameters: Vec<Token>, body: Rc<Statement> },
	Grouping(Box<Expression>),
	Literal(Token),
	Unary { operator: Token, right: Box<Expression> },
	Variable(Token),
}

impl Display for Expression {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		if f.alternate() {
			write!(f, "{location} ", location = self.location())?;
		}
		match self {
			Expression::Assignment { name: Token { text: name, .. }, value } => {
				write!(f, "(assign {name} = {value})")
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
				write!(f, "(fn (")?;
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

			Expression::Unary { operator: Token { text: operator, .. }, right } => {
				write!(f, "({operator} {right})")
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
			Expression::Function { location, .. } => location,
			Expression::Grouping(expression) => expression.location(),
			Expression::Literal(token) => token.location(),
			Expression::Unary { operator, .. } => operator.location(),
			Expression::Variable(token) => token.location(),
		}
	}
}
