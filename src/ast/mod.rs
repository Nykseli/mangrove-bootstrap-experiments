use std::mem::discriminant;

#[derive(Debug, Clone)]
pub struct ASTInt32Type {}

#[derive(Debug, Clone, Default)]
pub struct ASTStringType {
	/// Is strign allocated on dynamically or statically?
	/// Dynamic values have to be freed at the end of scope.
	pub dynamic: bool,
	/// The address to the start of the string.
	/// Addresses are i32 in wasm so lets use it for now
	pub addr: i32,
	/// The size in bytes.
	/// Addresses are i32 in wasm so lets use it for now
	pub size: i32,
}

#[derive(Debug, Clone)]
pub enum ASTType {
	Int32(ASTInt32Type),
	String(ASTStringType),
	Custom(ASTClass),
}

impl ASTType {
	pub fn has_same_type(&self, other: &Self) -> bool {
		discriminant(self) == discriminant(other)
	}
}

#[derive(Debug, Clone)]
pub enum ASTFunctionCallArg {
	Char(char),
	Int32(i32),
	String(String),
	Ident(String),
	DottedIdent((String, String)),
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
pub struct ASTAssignIdent {
	/// Name of the identifier
	pub ident: String,
	/// Type of the identifier
	pub ident_type: ASTType,
}

#[derive(Debug)]
pub enum StaticValue {
	Int32(i32),
	String(String),
}

#[derive(Debug)]
pub struct ASTStaticAssign {
	pub value: StaticValue,
	pub value_type: ASTType,
}

#[derive(Debug)]
pub enum ASTAssignArg {
	Static(ASTStaticAssign),
	Ident(ASTAssignIdent),
}

impl ASTAssignArg {
	pub fn has_same_type(&self, other: &Self) -> bool {
		discriminant(self) == discriminant(other)
	}
}

#[derive(Debug)]
pub struct ASTAdd {
	pub rhs: ASTAssignArg,
	pub lhs: ASTAssignArg,
}

#[derive(Debug)]
pub struct ASTMinus {
	pub rhs: ASTAssignArg,
	pub lhs: ASTAssignArg,
}

#[derive(Debug)]
pub struct ASTClassInitArg {
	pub ident: String,
	pub arg: ASTAssignArg,
}

#[derive(Debug)]
pub struct ASTClassInit {
	pub name: String,
	pub args: Vec<ASTClassInitArg>,
}

#[derive(Debug)]
pub enum ASTAssignmentExpr {
	/// Single value
	Arg(ASTAssignArg),
	Add(ASTAdd),
	Minus(ASTMinus),
	FunctionCall(ASTFunctionCall),
	ClassInit(ASTClassInit),
}

#[derive(Debug)]
pub struct ASTAssignment {
	pub variable: ASTVariable,
	pub expr: ASTAssignmentExpr,
}

#[derive(Debug)]
pub enum ASTBlockStatement {
	Assignment(ASTAssignment),
	FunctionCall(ASTFunctionCall),
	Return(ASTReturn),
	IfStmt(ASTIfStmt),
}

#[derive(Debug, Clone)]
pub struct ASTVariable {
	pub ast_type: ASTType,
	pub ident: String,
}

#[derive(Debug)]
pub struct ASTBlock {
	pub variables: Vec<ASTVariable>,
	pub statements: Vec<ASTBlockStatement>,
}

impl ASTBlock {
	pub fn new(statements: Vec<ASTBlockStatement>, variables: Vec<ASTVariable>) -> Self {
		Self {
			statements,
			variables,
		}
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

#[derive(Debug, Clone)]
pub struct ASTClassMember {
	/// Name of the class identifier
	pub ident: String,
	pub type_: ASTType,
}

#[derive(Debug, Clone)]
pub struct ASTClass {
	pub name: String,
	pub members: Vec<ASTClassMember>,
}

impl ASTClass {
	pub fn member<'a>(&'a self, name: &'a str) -> Option<&'a ASTClassMember> {
		self.members.iter().find(|m| m.ident == name)
	}
}
