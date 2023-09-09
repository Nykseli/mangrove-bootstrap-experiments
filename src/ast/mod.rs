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
	/// is return value used somewhere
	pub return_used: bool,
}

impl ASTFunctionCall {
	pub fn new(name: String, args: Vec<ASTFunctionCallArg>, return_used: bool) -> Self {
		Self {
			name,
			args,
			return_used,
		}
	}
}

#[derive(Debug)]
pub enum ASTAssignArg {
	Int32(i32),
	Ident(String),
}

#[derive(Debug)]
pub struct ASTAdd {
	pub rhs: ASTAssignArg,
	pub lhs: ASTAssignArg,
}

#[derive(Debug)]
pub enum ASTAssignmentExpr {
	/// Single value
	Arg(ASTAssignArg),
	Add(ASTAdd),
	FunctionCall(ASTFunctionCall),
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
	Return(ASTReturn),
	IfStmt(ASTIfStmt),
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
pub struct ASTLtStmt {
	pub rhs: ASTAssignArg,
	pub lhs: ASTAssignArg,
}

#[derive(Debug)]
pub struct ASTIfStmt {
	pub conditional: ASTLtStmt,
	pub block: ASTBlock,
}

#[derive(Debug)]
pub struct ASTReturn {
	// TODO: refactor the type name
	pub expr: ASTAssignmentExpr,
}

#[derive(Debug)]
pub struct ASTFunction {
	pub name: String,
	/// Int32 arguments
	pub args: Vec<String>,
	pub body: ASTBlock,
	/// Only none and Int32 are supported
	pub returns: bool,
}

impl ASTFunction {
	pub fn new(name: String, args: Vec<String>, body: ASTBlock, returns: bool) -> Self {
		Self {
			name,
			args,
			body,
			returns,
		}
	}
}
