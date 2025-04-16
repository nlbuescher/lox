use std::fmt::{Display, Formatter};
use std::rc::Rc;

use crate::location::{Locate, Location};
use crate::parse::Expression;
use crate::tokenize::Token;

/////////////////////////////////////////////////////////////////////////////
// Struct declarations
/////////////////////////////////////////////////////////////////////////////

pub enum Statement {
	Block(BlockStatement),
	ClassDeclaration(ClassDeclarationStatement),
	Expression(ExpressionStatement),
	For(ForStatement),
	FunctionDeclaration(FunctionDeclarationStatement),
	If(IfStatement),
	Print(PrintStatement),
	Return(ReturnStatement),
	VariableDeclaration(VariableDeclarationStatement),
	While(WhileStatement),
}

pub struct BlockStatement {
	pub open_brace: Token,
	pub statements: Vec<Statement>,
	pub close_brace: Token,
}

pub struct ClassDeclarationStatement {
	pub keyword: Token,
	pub name: Token,
	pub super_class: Option<Expression>,
	pub open_brace: Token,
	pub methods: Vec<FunctionDeclarationStatement>,
	pub close_brace: Token,
}

pub struct ExpressionStatement {
	pub expression: Box<Expression>,
}

pub struct ForStatement {
	pub keyword: Token,
	pub initializer: Option<Box<Statement>>,
	pub condition: Option<Box<Expression>>,
	pub increment: Option<Box<ExpressionStatement>>,
	pub body: Box<BlockStatement>,
}

pub struct FunctionDeclarationStatement {
	pub keyword: Option<Token>,
	pub name: Token,
	pub parameters: Vec<Token>,
	pub body: Rc<BlockStatement>,
}

pub struct IfStatement {
	pub keyword: Token,
	pub condition: Box<Expression>,
	pub then_branch: Box<BlockStatement>,
	pub else_branch: Option<(Token, Box<Statement>)>,
}

pub struct PrintStatement {
	pub keyword: Token,
	pub expression: Box<Expression>,
}

pub struct ReturnStatement {
	pub keyword: Token,
	pub expression: Option<Box<Expression>>,
}

pub struct VariableDeclarationStatement {
	pub keyword: Token,
	pub name: Token,
	pub initializer: Option<Box<Expression>>,
}

pub struct WhileStatement {
	pub keyword: Token,
	pub condition: Box<Expression>,
	pub body: Box<BlockStatement>,
}

/////////////////////////////////////////////////////////////////////////////
// Display
/////////////////////////////////////////////////////////////////////////////

const PAD: &str = "";

impl Display for Statement {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		match self {
			Statement::Block(statement) => statement.fmt(f),
			Statement::ClassDeclaration(statement) => statement.fmt(f),
			Statement::Expression(statement) => statement.fmt(f),
			Statement::For(statement) => statement.fmt(f),
			Statement::FunctionDeclaration(statement) => statement.fmt(f),
			Statement::If(statement) => statement.fmt(f),
			Statement::Print(statement) => statement.fmt(f),
			Statement::Return(statement) => statement.fmt(f),
			Statement::VariableDeclaration(statement) => statement.fmt(f),
			Statement::While(statement) => statement.fmt(f),
		}
	}
}

impl Display for BlockStatement {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		let BlockStatement { statements, close_brace, .. } = self;

		let width = f.width().unwrap_or(0);

		if f.alternate() {
			write!(f, "{location} ", location = self.locate())?;
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
}

impl Display for ClassDeclarationStatement {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		let ClassDeclarationStatement {
			name: Token { text: name, .. },
			super_class,
			open_brace,
			methods,
			close_brace,
			..
		} = self;

		let width = f.width().unwrap_or(0);

		if f.alternate() {
			write!(f, "{location} ", location = self.locate())?;
		}

		write!(f, "{PAD:width$}class {name}")?;

		if let Some(Expression::Variable(Token { text: super_class, .. })) = super_class {
			write!(f, " < {super_class}")?;
		}

		writeln!(f)?;

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
}

impl Display for ExpressionStatement {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		let ExpressionStatement { expression } = self;

		let width = f.width().unwrap_or(0);

		if f.alternate() {
			write!(f, "{location} ", location = self.locate())?;
		}

		write!(f, "{PAD:width$}(expr {expression})")
	}
}
impl Display for ForStatement {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		let ForStatement { initializer, condition, increment, body, .. } = self;

		let initializer = initializer.as_ref().map_or_else(String::new, |it| it.to_string());
		let condition = condition.as_ref().map_or_else(String::new, |it| it.to_string());
		let increment = increment.as_ref().map_or_else(String::new, |it| it.to_string());

		let width = f.width().unwrap_or(0);

		if f.alternate() {
			write!(f, "{location} ", location = self.locate())?;
		}

		write!(f, "{PAD:width$}for({initializer}; {condition}; {increment})")?;

		if f.alternate() {
			write!(f, "{body:#width$}")
		} else {
			write!(f, "{body:width$}")
		}
	}
}

impl Display for FunctionDeclarationStatement {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		let FunctionDeclarationStatement { name, parameters, body, .. } = self;

		let width = f.width().unwrap_or(0);

		if f.alternate() {
			write!(f, "{location} ", location = self.locate())?;
		}

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
}

impl Display for IfStatement {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		let IfStatement { condition, then_branch, else_branch, .. } = self;

		let width = f.width().unwrap_or(0);

		if f.alternate() {
			write!(f, "{location} ", location = self.locate())?;
		}

		writeln!(f, "{PAD:width$}if {condition}")?;

		if f.alternate() {
			write!(f, "{then_branch:#width$}")?;
		} else {
			write!(f, "{then_branch:width$}")?;
		}

		if let Some((_, else_branch)) = &else_branch {
			writeln!(f)?;

			writeln!(f, "{PAD:width$}else")?;

			if f.alternate() {
				write!(f, "{else_branch:#width$}")?;
			} else {
				write!(f, "{else_branch:width$}")?;
			}
		}

		Ok(())
	}
}

impl Display for PrintStatement {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		let PrintStatement { expression, .. } = self;

		let width = f.width().unwrap_or(0);

		if f.alternate() {
			write!(f, "{location} ", location = self.locate())?;
		}

		write!(f, "{PAD:width$}(print {expression})")
	}
}

impl Display for ReturnStatement {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		let ReturnStatement { expression, .. } = self;

		let width = f.width().unwrap_or(0);

		if f.alternate() {
			write!(f, "{location} ", location = self.locate())?;
		}

		write!(f, "{PAD:width$}")?;

		if let Some(expression) = &expression {
			write!(f, "(return {expression})")
		} else {
			write!(f, "(return)")
		}
	}
}

impl Display for VariableDeclarationStatement {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		let VariableDeclarationStatement { name: Token { text: name, .. }, initializer, .. } = self;

		let width = f.width().unwrap_or(0);

		if f.alternate() {
			write!(f, "{location} ", location = self.locate())?;
		}

		write!(f, "{PAD:width$}")?;

		match &initializer {
			Some(expression) => write!(f, "(vardecl {name} = {expression})"),
			None => write!(f, "(vardecl {name})"),
		}
	}
}

impl Display for WhileStatement {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		let WhileStatement { condition, body, .. } = self;

		let width = f.width().unwrap_or(0);

		if f.alternate() {
			write!(f, "{location} ", location = self.locate())?;
		}

		writeln!(f, "{PAD:width$}while {condition}")?;

		if f.alternate() {
			write!(f, "{body:#width$}")
		} else {
			write!(f, "{body:width$}")
		}
	}
}

impl Locate for Statement {
	fn locate(&self) -> &Location {
		match self {
			Statement::Block(statement) => statement.locate(),
			Statement::ClassDeclaration(statement) => statement.locate(),
			Statement::Expression(statement) => statement.locate(),
			Statement::For(statement) => statement.locate(),
			Statement::FunctionDeclaration(statement) => statement.locate(),
			Statement::If(statement) => statement.locate(),
			Statement::Print(statement) => statement.locate(),
			Statement::Return(statement) => statement.locate(),
			Statement::VariableDeclaration(statement) => statement.locate(),
			Statement::While(statement) => statement.locate(),
		}
	}
}

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

impl Locate for IfStatement {
	fn locate(&self) -> &Location {
		self.keyword.locate()
	}
}

impl Locate for FunctionDeclarationStatement {
	fn locate(&self) -> &Location {
		if let Some(keyword) = &self.keyword {
			keyword.locate()
		} else {
			self.name.locate()
		}
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
		self.keyword.locate()
	}
}

impl Locate for WhileStatement {
	fn locate(&self) -> &Location {
		self.keyword.locate()
	}
}
