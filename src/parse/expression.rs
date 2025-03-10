use std::fmt::{Display, Formatter};
use std::rc::Rc;

use crate::location::{Locate, Location};
use crate::parse::BlockStatement;
use crate::tokenize::Token;

#[derive(Clone)]
pub enum Expression {
	Assignment {
		name: Box<Token>,
		expression: Box<Expression>,
	},
	Binary {
		left: Box<Expression>,
		operator: Box<Token>,
		right: Box<Expression>,
	},
	Call {
		callee: Box<Expression>,
		open_paren: Box<Token>,
		arguments: Vec<Expression>,
		close_paren: Box<Token>,
	},
	Function {
		keyword: Box<Token>,
		open_paren: Box<Token>,
		parameters: Vec<Token>,
		close_paren: Box<Token>,
		body: Rc<BlockStatement>,
	},
	Get {
		object: Box<Expression>,
		property: Box<Token>,
	},
	Grouping(Box<Expression>),
	Literal(Box<Token>),
	Set {
		object: Box<Expression>,
		property: Box<Token>,
		value: Box<Expression>,
	},
	This(Box<Token>),
	Unary {
		operator: Box<Token>,
		expression: Box<Expression>,
	},
	Variable(Box<Token>),
}

impl Display for Expression {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		if f.alternate() {
			write!(f, "{location} ", location = self.locate())?;
		}
		match self {
			Expression::Assignment { name, expression } => {
				write!(f, "(assign {name} = {expression})", name = name.text)
			}

			Expression::Binary { left, operator, right } => {
				write!(f, "({operator} {left} {right})", operator = operator.text)
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

			Expression::Get { object, property } => {
				write!(f, "{object}.{property}", property = property.text)
			}

			Expression::Grouping(expression) => write!(f, "(group {expression})"),

			Expression::Literal(literal) => {
				write!(f, "{literal}", literal = literal.text)
			}

			Expression::Set { object, property, value } => {
				write!(f, "{object}.{property} = {value}", property = property.text)
			}

			Expression::This(_) => {
				write!(f, "(this)")
			}

			Expression::Unary { operator, expression } => {
				write!(f, "({operator} {expression})", operator = operator.text)
			}

			Expression::Variable(name) => {
				write!(f, "(var {name})", name = name.text)
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
			Expression::Set { property, .. } => property.locate(),
			Expression::This(keyword) => keyword.locate(),
			Expression::Unary { operator, .. } => operator.locate(),
			Expression::Variable(name) => name.locate(),
		}
	}
}
