#[derive(Debug, Clone)]
pub enum ASTFunctionCallArg {
	Char(char),
	Int32(i32),
	String(String),
	Ident(String),
}

#[derive(Debug)]
pub struct ASTFunctionCall {
	pub name: String,
	pub args: Vec<ASTFunctionCallArg>,
}

impl ASTFunctionCall {
	pub fn new(name: String, args: Vec<ASTFunctionCallArg>) -> Self {
		Self { name, args }
	}
}

#[derive(Debug)]
pub struct ASTAdd {
	pub rhs: i32,
	pub lhs: i32,
}

#[derive(Debug)]
pub enum ASTAssignmentExpr {
	Int32(i32),
	Add(ASTAdd),
}

#[derive(Debug)]
pub struct ASTAssignment {
	pub type_name: String,
	pub ident: String,
	pub expr: ASTAssignmentExpr,
}

#[derive(Debug)]
pub enum ASTBlockStatement {
	Assignment(ASTAssignment),
	FunctionCall(ASTFunctionCall),
}

#[derive(Debug)]
pub struct ASTBlock {
	pub statements: Vec<ASTBlockStatement>,
}

impl ASTBlock {
	pub fn new(statements: Vec<ASTBlockStatement>) -> Self {
		Self { statements }
	}
}

#[derive(Debug)]
pub struct ASTFunction {
	pub name: String,
	pub body: ASTBlock,
}

impl ASTFunction {
	pub fn new(name: String, body: ASTBlock) -> Self {
		Self { name, body }
	}
}
