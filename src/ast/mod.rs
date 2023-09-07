#[derive(Debug)]
pub struct ASTFunctionCall {
	pub name: String,
	pub arg: String,
}

impl ASTFunctionCall {
	pub fn new(name: String, arg: String) -> Self {
		Self { name, arg }
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
