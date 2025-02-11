use std::fmt::{Display, Formatter};
use std::rc::Rc;

use crate::location::{Locate, Location};
use crate::parse::Expression;
use crate::tokenize::Token;

pub enum StatementKind {
	Block,
	ClassDeclaration,
	Expression,
	For,
	FunctionDeclaration,
	If,
	Print,
	Return,
	VariableDeclaration,
	While,
}

pub trait Statement: Display + Locate {
	fn statement_kind(&self) -> StatementKind;

	fn as_block(&self) -> Option<&BlockStatement> {
		None
	}

	fn as_class_declaration(&self) -> Option<&ClassDeclarationStatement> {
		None
	}

	fn as_expression(&self) -> Option<&ExpressionStatement> {
		None
	}

	fn as_for(&self) -> Option<&ForStatement> {
		None
	}

	fn as_function_declaration(&self) -> Option<&FunctionDeclarationStatement> {
		None
	}

	fn as_if(&self) -> Option<&IfStatement> {
		None
	}

	fn as_print(&self) -> Option<&PrintStatement> {
		None
	}

	fn as_return(&self) -> Option<&ReturnStatement> {
		None
	}

	fn as_variable_declaration(&self) -> Option<&VariableDeclarationStatement> {
		None
	}

	fn as_while(&self) -> Option<&WhileStatement> {
		None
	}
}

/////////////////////////////////////////////////////////////////////////////
// Struct declarations
/////////////////////////////////////////////////////////////////////////////

pub struct BlockStatement {
	pub open_brace: Box<Token>,
	pub statements: Vec<Box<dyn Statement>>,
	pub close_brace: Box<Token>,
}

pub struct ClassDeclarationStatement {
	pub keyword: Box<Token>,
	pub name: Box<Token>,
	pub open_brace: Box<Token>,
	pub methods: Vec<FunctionDeclarationStatement>,
	pub close_brace: Box<Token>,
}

pub struct ExpressionStatement {
	pub expression: Box<Expression>,
}

pub struct ForStatement {
	pub keyword: Box<Token>,
	pub initializer: Option<Box<dyn Statement>>,
	pub condition: Option<Box<Expression>>,
	pub increment: Option<Box<ExpressionStatement>>,
	pub body: Box<BlockStatement>,
}

pub struct FunctionDeclarationStatement {
	pub keyword: Option<Box<Token>>,
	pub name: Box<Token>,
	pub parameters: Vec<Token>,
	pub body: Rc<BlockStatement>,
}

pub struct IfStatement {
	pub keyword: Box<Token>,
	pub condition: Box<Expression>,
	pub then_branch: Box<BlockStatement>,
	pub else_branch: Option<(Token, Box<dyn Statement>)>,
}

pub struct PrintStatement {
	pub keyword: Box<Token>,
	pub expression: Box<Expression>,
}

pub struct ReturnStatement {
	pub keyword: Box<Token>,
	pub expression: Option<Box<Expression>>,
}

pub struct VariableDeclarationStatement {
	pub keyword: Box<Token>,
	pub name: Box<Token>,
	pub initializer: Option<Box<Expression>>,
}

pub struct WhileStatement {
	pub keyword: Box<Token>,
	pub condition: Box<Expression>,
	pub body: Box<BlockStatement>,
}

impl Statement for BlockStatement {
	fn statement_kind(&self) -> StatementKind {
		StatementKind::Block
	}

	fn as_block(&self) -> Option<&BlockStatement> {
		Some(self)
	}
}

/////////////////////////////////////////////////////////////////////////////
// Statement
/////////////////////////////////////////////////////////////////////////////

impl Statement for ClassDeclarationStatement {
	fn statement_kind(&self) -> StatementKind {
		StatementKind::ClassDeclaration
	}

	fn as_class_declaration(&self) -> Option<&ClassDeclarationStatement> {
		Some(self)
	}
}

impl Statement for ExpressionStatement {
	fn statement_kind(&self) -> StatementKind {
		StatementKind::Expression
	}

	fn as_expression(&self) -> Option<&ExpressionStatement> {
		Some(self)
	}
}

impl Statement for ForStatement {
	fn statement_kind(&self) -> StatementKind {
		StatementKind::For
	}

	fn as_for(&self) -> Option<&ForStatement> {
		Some(self)
	}
}

impl Statement for FunctionDeclarationStatement {
	fn statement_kind(&self) -> StatementKind {
		StatementKind::FunctionDeclaration
	}

	fn as_function_declaration(&self) -> Option<&FunctionDeclarationStatement> {
		Some(self)
	}
}

impl Statement for IfStatement {
	fn statement_kind(&self) -> StatementKind {
		StatementKind::If
	}

	fn as_if(&self) -> Option<&IfStatement> {
		Some(self)
	}
}

impl Statement for PrintStatement {
	fn statement_kind(&self) -> StatementKind {
		StatementKind::Print
	}

	fn as_print(&self) -> Option<&PrintStatement> {
		Some(self)
	}
}

impl Statement for ReturnStatement {
	fn statement_kind(&self) -> StatementKind {
		StatementKind::Return
	}

	fn as_return(&self) -> Option<&ReturnStatement> {
		Some(self)
	}
}

impl Statement for VariableDeclarationStatement {
	fn statement_kind(&self) -> StatementKind {
		StatementKind::VariableDeclaration
	}

	fn as_variable_declaration(&self) -> Option<&VariableDeclarationStatement> {
		Some(self)
	}
}

impl Statement for WhileStatement {
	fn statement_kind(&self) -> StatementKind {
		StatementKind::While
	}

	fn as_while(&self) -> Option<&WhileStatement> {
		Some(self)
	}
}

/////////////////////////////////////////////////////////////////////////////
// Display
/////////////////////////////////////////////////////////////////////////////

const PAD: &'static str = "";

impl Display for BlockStatement {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		let width = f.width().unwrap_or(0);

		if f.alternate() {
			write!(f, "{location} ", location = self.open_brace.locate())?;
		}
		writeln!(f, "{PAD:width$}{{")?;

		{
			let width = width + 1;
			for statement in &self.statements {
				if f.alternate() {
					writeln!(f, "{statement:#width$}")?;
				} else {
					writeln!(f, "{statement:width$}")?;
				}
			}
		}

		if f.alternate() {
			write!(f, "{location} ", location = self.close_brace.locate())?;
		}
		write!(f, "{PAD:width$}}}")
	}
}

impl Display for ClassDeclarationStatement {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		let width = f.width().unwrap_or(0);

		if f.alternate() {
			write!(f, "{location} ", location = self.keyword.locate())?;
		}

		writeln!(f, "{PAD:width$}class {name}", name = self.name.text)?;

		if f.alternate() {
			write!(f, "{location} ", location = self.open_brace.locate())?;
		}
		writeln!(f, "{PAD:width$}{{")?;

		{
			let width = width + 1;

			for method in &self.methods {
				writeln!(f, "{method:width$}")?;
			}
		}

		if f.alternate() {
			write!(f, "{location} ", location = self.close_brace.locate())?;
		}
		write!(f, "{PAD:width$}}}")
	}
}

impl Display for ExpressionStatement {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		let width = f.width().unwrap_or(0);

		if f.alternate() {
			write!(f, "{location} ", location = self.expression.locate())?;
		}
		write!(f, "{PAD:width$}(expr {expression})", expression = self.expression)
	}
}

impl Display for ForStatement {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		let width = f.width().unwrap_or(0);

		let initializer = self.initializer.as_ref().map_or(String::new(), |it| it.to_string());
		let condition = self.condition.as_ref().map_or(String::new(), |it| it.to_string());
		let increment = self.increment.as_ref().map_or(String::new(), |it| it.to_string());

		if f.alternate() {
			write!(f, "{location} ", location = self.keyword.locate())?;
		}

		write!(f, "{PAD:width$}for({initializer}; {condition}; {increment})")?;

		if f.alternate() {
			write!(f, "{body:#width$}", body = self.body)
		} else {
			write!(f, "{body:width$}", body = self.body)
		}
	}
}

impl Display for FunctionDeclarationStatement {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		let width = f.width().unwrap_or(0);

		if f.alternate() {
			write!(f, "{location} ", location = self.locate())?;
		}
		write!(f, "{PAD:width$}fun {name}(", name = self.name.text)?;

		for (index, parameter) in self.parameters.iter().enumerate() {
			if index != 0 {
				write!(f, ", ")?;
			}
			write!(f, "{parameter}", parameter = parameter.text)?;
		}

		writeln!(f, ")")?;

		if f.alternate() {
			write!(f, "{body:#width$}", body = self.body)
		} else {
			write!(f, "{body:width$}", body = self.body)
		}
	}
}

impl Display for IfStatement {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		let width = f.width().unwrap_or(0);

		if f.alternate() {
			write!(f, "{location} ", location = self.keyword.locate())?;
		}

		writeln!(f, "{PAD:width$}if {condition}", condition = self.condition)?;

		if f.alternate() {
			write!(f, "{then_branch:#width$}", then_branch = self.then_branch)?;
		} else {
			write!(f, "{then_branch:width$}", then_branch = self.then_branch)?;
		}

		if let Some((keyword, else_branch)) = &self.else_branch {
			writeln!(f)?;

			if f.alternate() {
				write!(f, "{location}", location = keyword.locate())?;
			}

			writeln!(f, "{PAD:width$}else")?;

			if f.alternate() {
				write!(f, "{else_branch:#width$}", else_branch = else_branch)?;
			} else {
				write!(f, "{else_branch:width$}", else_branch = else_branch)?;
			}
		}
		Ok(())
	}
}

impl Display for PrintStatement {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		let width = f.width().unwrap_or(0);

		if f.alternate() {
			write!(f, "{location} ", location = self.keyword.locate())?;
		}
		write!(f, "{PAD:width$}(print {expression})", expression = self.expression)
	}
}

impl Display for ReturnStatement {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		let width = f.width().unwrap_or(0);

		if f.alternate() {
			write!(f, "{location} ", location = self.keyword.locate())?;
		}

		write!(f, "{PAD:width$}")?;

		if let Some(expression) = &self.expression {
			write!(f, "(return {expression})", expression = expression)
		} else {
			write!(f, "(return)")
		}
	}
}

impl Display for VariableDeclarationStatement {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		const PAD: &str = "";
		let width = f.width().unwrap_or(0);

		if f.alternate() {
			write!(f, "{location} ", location = self.keyword.locate())?;
		}

		write!(f, "{PAD:width$}")?;

		let name = &self.name.text;
		match &self.initializer {
			Some(expression) => {
				write!(f, "(vardecl {name} = {expression})", name = name, expression = expression)
			}

			None => write!(f, "(vardecl {name})", name = name),
		}
	}
}

impl Display for WhileStatement {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		const PAD: &str = "";
		let width = f.width().unwrap_or(0);

		if f.alternate() {
			write!(f, "{location} ", location = self.keyword.locate())?;
		}

		writeln!(f, "{PAD:width$}while {condition}", condition = self.condition)?;

		if f.alternate() {
			write!(f, "{body:#width$}", body = self.body)
		} else {
			write!(f, "{body:width$}", body = self.body)
		}
	}
}

/////////////////////////////////////////////////////////////////////////////
// Locate
/////////////////////////////////////////////////////////////////////////////

impl Locate for BlockStatement {
	fn locate(&self) -> &Location {
		self.open_brace.locate()
	}
}

impl Locate for ClassDeclarationStatement {
	fn locate(&self) -> &Location {
		self.keyword.locate()
	}
}

impl Locate for ExpressionStatement {
	fn locate(&self) -> &Location {
		self.expression.locate()
	}
}

impl Locate for ForStatement {
	fn locate(&self) -> &Location {
		self.keyword.locate()
	}
}

impl Locate for FunctionDeclarationStatement {
	fn locate(&self) -> &Location {
		if let Some(ref keyword) = self.keyword {
			keyword.locate()
		} else {
			self.name.locate()
		}
	}
}

impl Locate for IfStatement {
	fn locate(&self) -> &Location {
		self.keyword.locate()
	}
}

impl Locate for PrintStatement {
	fn locate(&self) -> &Location {
		self.keyword.locate()
	}
}

impl Locate for ReturnStatement {
	fn locate(&self) -> &Location {
		self.keyword.locate()
	}
}

impl Locate for VariableDeclarationStatement {
	fn locate(&self) -> &Location {
		self.name.locate()
	}
}

impl Locate for WhileStatement {
	fn locate(&self) -> &Location {
		self.keyword.locate()
	}
}
