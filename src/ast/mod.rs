#[derive(Debug, Clone)]
pub enum ASTFunctionCallArg {
	Char(char),
	Int32(i32),
	String(String),
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
pub struct ASTBlock {
	pub statements: Vec<ASTFunctionCall>,
}

impl ASTBlock {
	pub fn new(statements: Vec<ASTFunctionCall>) -> Self {
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
