use std::fmt::{Display, Formatter};
use std::rc::Rc;

use crate::location::{Locate, Location};
use crate::parse::BlockStatement;
use crate::tokenize::Token;

#[derive(Clone)]
pub enum Expression {
	Assignment(AssignmentExpression),
	Binary(BinaryExpression),
	Call(CallExpression),
	Function(FunctionExpression),
	Get(GetExpression),
	Grouping(GroupingExpression),
	Literal(LiteralExpression),
	Set(SetExpression),
	Super(SuperExpression),
	This(ThisExpression),
	Unary(UnaryExpression),
	Variable(VariableExpression),
}

#[derive(Clone)]
pub struct AssignmentExpression {
	pub name: Token,
	pub expression: Box<Expression>,
}

#[derive(Clone)]
pub struct BinaryExpression {
	pub left: Box<Expression>,
	pub operator: Token,
	pub right: Box<Expression>,
}

#[derive(Clone)]
pub struct CallExpression {
	pub callee: Box<Expression>,
	pub open_paren: Token,
	pub arguments: Vec<Expression>,
	pub close_paren: Token,
}

#[derive(Clone)]
pub struct FunctionExpression {
	pub keyword: Token,
	pub open_paren: Token,
	pub parameters: Vec<Token>,
	pub close_paren: Token,
	pub body: Rc<BlockStatement>,
}

#[derive(Clone)]
pub struct GetExpression {
	pub object: Box<Expression>,
	pub property: Token,
}

#[derive(Clone)]
pub struct GroupingExpression(pub Box<Expression>);

#[derive(Clone)]
pub struct LiteralExpression(pub Token);

#[derive(Clone)]
pub struct SetExpression {
	pub object: Box<Expression>,
	pub property: Token,
	pub value: Box<Expression>,
}

#[derive(Clone)]
pub struct SuperExpression {
	pub keyword: Token,
	pub method: Token,
}

#[derive(Clone)]
pub struct ThisExpression(pub Token);

#[derive(Clone)]
pub struct UnaryExpression {
	pub operator: Token,
	pub expression: Box<Expression>,
}

#[derive(Clone)]
pub struct VariableExpression(pub Token);

impl Display for Expression {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		match self {
			Expression::Assignment(expression) => expression.fmt(f),
			Expression::Binary(expression) => expression.fmt(f),
			Expression::Call(expression) => expression.fmt(f),
			Expression::Function(expression) => expression.fmt(f),
			Expression::Get(expression) => expression.fmt(f),
			Expression::Grouping(expression) => expression.fmt(f),
			Expression::Literal(expression) => expression.fmt(f),
			Expression::Set(expression) => expression.fmt(f),
			Expression::Super(expression) => expression.fmt(f),
			Expression::This(expression) => expression.fmt(f),
			Expression::Unary(expression) => expression.fmt(f),
			Expression::Variable(expression) => expression.fmt(f),
		}
	}
}

impl Display for AssignmentExpression {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		let AssignmentExpression { name: Token { text: name, .. }, expression } = self;

		if f.alternate() {
			write!(f, "{location} ", location = self.locate())?;
		}

		write!(f, "(assign {name} = {expression})")
	}
}

impl Display for BinaryExpression {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		let BinaryExpression { left, operator: Token { text: operator, .. }, right } = self;

		if f.alternate() {
			write!(f, "{location} ", location = self.locate())?;
		}

		write!(f, "({operator} {left} {right})")
	}
}

impl Display for CallExpression {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		let CallExpression { callee, arguments, .. } = self;

		if f.alternate() {
			write!(f, "{location} ", location = self.locate())?;
		}

		write!(f, "(call {callee}(")?;

		for (index, argument) in arguments.iter().enumerate() {
			if index != 0 {
				write!(f, ", ")?;
			}
			write!(f, "{argument}")?;
		}
		write!(f, "))")
	}
}

impl Display for FunctionExpression {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		let FunctionExpression { parameters, .. } = self;

		if f.alternate() {
			write!(f, "{location} ", location = self.locate())?;
		}

		write!(f, "(fun (")?;
		for (index, Token { text: parameter, .. }) in parameters.iter().enumerate() {
			if index != 0 {
				write!(f, ", ")?;
			}
			write!(f, "{parameter}")?;
		}
		write!(f, "))")
	}
}

impl Display for GetExpression {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		let GetExpression { object, property } = self;

		if f.alternate() {
			write!(f, "{location} ", location = self.locate())?;
		}

		write!(f, "({object}.{property})")
	}
}

impl Display for GroupingExpression {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		let GroupingExpression(expression) = self;

		if f.alternate() {
			write!(f, "{location} ", location = self.locate())?;
		}

		write!(f, "(group {expression})")
	}
}

impl Display for LiteralExpression {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		let LiteralExpression(Token { text: literal, .. }) = self;

		if f.alternate() {
			write!(f, "{location} ", location = self.locate())?;
		}

		write!(f, "{literal}")
	}
}

impl Display for SetExpression {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		let SetExpression { object, property, value } = self;

		if f.alternate() {
			write!(f, "{location} ", location = self.locate())?;
		}

		write!(f, "({object}.{property} = {value})")
	}
}

impl Display for SuperExpression {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		let SuperExpression { method, .. } = self;

		if f.alternate() {
			write!(f, "{location} ", location = self.locate())?;
		}

		write!(f, "(super.{method})")
	}
}

impl Display for ThisExpression {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		if f.alternate() {
			write!(f, "{location} ", location = self.locate())?;
		}

		write!(f, "(this)")
	}
}

impl Display for UnaryExpression {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		let UnaryExpression { operator: Token { text: operator, .. }, expression } = self;

		if f.alternate() {
			write!(f, "{location} ", location = self.locate())?;
		}

		write!(f, "({operator} {expression})")
	}
}

impl Display for VariableExpression {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		let VariableExpression(Token { text: name, .. }) = self;

		if f.alternate() {
			write!(f, "{location} ", location = self.locate())?;
		}

		write!(f, "(var {name})")
	}
}

impl Locate for Expression {
	fn locate(&self) -> &Location {
		match self {
			Expression::Assignment(expression) => expression.locate(),
			Expression::Binary(expression) => expression.locate(),
			Expression::Call(expression) => expression.locate(),
			Expression::Function(expression) => expression.locate(),
			Expression::Get(expression) => expression.locate(),
			Expression::Grouping(expression) => expression.locate(),
			Expression::Literal(expression) => expression.locate(),
			Expression::Set(expression) => expression.locate(),
			Expression::Super(expression) => expression.locate(),
			Expression::This(expression) => expression.locate(),
			Expression::Unary(expression) => expression.locate(),
			Expression::Variable(expression) => expression.locate(),
		}
	}
}

impl Locate for AssignmentExpression {
	fn locate(&self) -> &Location {
		self.name.locate()
	}
}
impl Locate for BinaryExpression {
	fn locate(&self) -> &Location {
		self.left.locate()
	}
}
impl Locate for CallExpression {
	fn locate(&self) -> &Location {
		self.callee.locate()
	}
}
impl Locate for FunctionExpression {
	fn locate(&self) -> &Location {
		self.keyword.locate()
	}
}
impl Locate for GetExpression {
	fn locate(&self) -> &Location {
		self.property.locate()
	}
}
impl Locate for GroupingExpression {
	fn locate(&self) -> &Location {
		self.0.locate()
	}
}
impl Locate for LiteralExpression {
	fn locate(&self) -> &Location {
		self.0.locate()
	}
}
impl Locate for SetExpression {
	fn locate(&self) -> &Location {
		self.property.locate()
	}
}
impl Locate for SuperExpression {
	fn locate(&self) -> &Location {
		self.keyword.locate()
	}
}
impl Locate for ThisExpression {
	fn locate(&self) -> &Location {
		self.0.locate()
	}
}
impl Locate for UnaryExpression {
	fn locate(&self) -> &Location {
		self.operator.locate()
	}
}
impl Locate for VariableExpression {
	fn locate(&self) -> &Location {
		self.0.locate()
	}
}
