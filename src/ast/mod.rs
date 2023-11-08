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
pub struct ASTArrayType {
	// Needs to be boxed value
	// TODO: use Rc instead of Box for cheaper clones in the future
	pub type_: Box<ASTType>,
	/// -1 means that the initialisation will decide the lenght
	/// While magic values are bad, it's a lot cleaner to have just a size
	/// in the compiler since the AST parser will define the length anyway
	pub size: i32,
}

#[derive(Debug, Clone)]
pub struct ASTPointerType {
	// Needs to be boxed value
	// TODO: use Rc instead of Box for cheaper clones in the future
	pub type_: Box<ASTType>,
}

#[derive(Debug, Clone)]
pub enum ASTType {
	Int64,
	Int32(ASTInt32Type),
	Array(ASTArrayType),
	String(ASTStringType),
	Class(ASTClass),
	Pointer(ASTPointerType),
	Enum(ASTEnum),
}

impl ASTType {
	pub fn name(&self) -> String {
		match self {
			ASTType::Int64 => "Int64".into(),
			ASTType::Int32(_) => "Int32".into(),
			ASTType::Array(_) => "Array".into(),
			ASTType::String(_) => "String".into(),
			ASTType::Class(class) => class.name.clone(),
			ASTType::Enum(e) => e.name.clone(),
			ASTType::Pointer(ptr) => format!("{}Ptr", ptr.type_.name()),
		}
	}

	pub fn has_same_type(&self, other: &Self) -> bool {
		match (self, other) {
			// Custom types should be equal if they have the same name
			(Self::Class(c1), Self::Class(c2)) => c1.name == c2.name,
			(Self::Enum(e1), Self::Enum(e2)) => e1.name == e2.name,
			// Arrays have to have the same size and type
			(Self::Array(a1), Self::Array(a2)) => {
				a1.size == a2.size && a1.type_.has_same_type(&a2.type_)
			}
			(_, _) => discriminant(self) == discriminant(other),
		}
	}
}

#[derive(Debug, Clone)]
pub enum ASTFunctionCallArg {
	Char(char),
	Int32(i32),
	Int64(i64),
	String(String),
	Ident(String),
	DottedIdent((String, String)),
}

#[derive(Debug, Clone)]
pub struct ASTFunctionCall {
	pub name: String,
	/// Is some when function call is a method call
	pub variable: Option<ASTVariable>,
	pub args: Vec<ASTFunctionCallArg>,
	/// is return value used somewhere
	pub return_used: bool,
	pub static_: bool,
}

impl ASTFunctionCall {
	pub fn new(
		name: String,
		variable: Option<ASTVariable>,
		args: Vec<ASTFunctionCallArg>,
		return_used: bool,
		static_: bool,
	) -> Self {
		Self {
			name,
			variable,
			args,
			return_used,
			static_,
		}
	}
}

#[derive(Debug, Clone)]
pub struct ASTAssignIdent {
	/// Name of the identifier
	pub ident: String,
	/// Type of the identifier
	pub ident_type: ASTType,
}

#[derive(Debug, Clone)]
pub struct ASTAssignDottedIdent {
	/// Name of the identifier
	pub ident: (String, String),
	/// Type of the identifier
	pub ident_type: ASTType,
}

#[derive(Debug, Clone)]
pub enum StaticValue {
	Int32(i32),
	Int64(i64),
	String(String),
}

#[derive(Debug, Clone)]
pub struct ASTStaticAssign {
	pub value: StaticValue,
	pub value_type: ASTType,
}

#[derive(Debug, Clone)]
pub enum ASTAssignArg {
	Static(ASTStaticAssign),
	Ident(ASTAssignIdent),
	DottedIdent(ASTAssignDottedIdent),
	Deref(ASTAssignIdent),
}

impl ASTAssignArg {
	pub fn has_same_type(&self, other: &Self) -> bool {
		discriminant(self) == discriminant(other)
	}
}

#[derive(Debug, Clone)]
pub struct ASTAdd {
	pub rhs: ASTAssignArg,
	pub lhs: ASTAssignArg,
}

#[derive(Debug, Clone)]
pub struct ASTMinus {
	pub rhs: ASTAssignArg,
	pub lhs: ASTAssignArg,
}

#[derive(Debug, Clone)]
pub struct ASTClassInitArg {
	pub ident: String,
	pub arg: ASTAssignmentExpr,
}

#[derive(Debug, Clone)]
pub struct ASTClassInit {
	pub name: String,
	pub args: Vec<ASTClassInitArg>,
}

#[derive(Debug, Clone)]
pub struct ASTArrayInit {
	pub args: Vec<ASTAssignmentExpr>,
}

#[derive(Debug, Clone)]
pub struct ASTArrayAccess {
	pub ident: ASTIdent,
	pub arg: Box<ASTAssignmentExpr>,
}

#[derive(Debug, Clone)]
pub enum ASTAssignmentExpr {
	/// Single value
	Arg(ASTAssignArg),
	Add(ASTAdd),
	Minus(ASTMinus),
	FunctionCall(ASTFunctionCall),
	ClassInit(ASTClassInit),
	ArrayInit(ASTArrayInit),
	ASTArrayAccess(ASTArrayAccess),
}

#[derive(Debug, Clone)]
pub struct ASTAssignment {
	pub variable: ASTVariable,
	pub expr: ASTAssignmentExpr,
	/// Is value being reassigned or defined for the first time
	pub reassignment: bool,
}

#[derive(Debug, Clone)]
pub enum ASTBlockStatement {
	Assignment(ASTAssignment),
	DerefAssignment(ASTAssignment),
	FunctionCall(ASTFunctionCall),
	Return(ASTReturn),
	IfStmt(ASTIfStmt),
}

#[derive(Debug, Clone)]
pub enum ASTIdent {
	Ident(String),
	DottedIdent((String, String)),
}

impl ASTIdent {
	pub fn ident(&self) -> &str {
		match self {
			ASTIdent::Ident(ident) => ident,
			ASTIdent::DottedIdent(dotted) => &dotted.0,
		}
	}
}

impl PartialEq for ASTIdent {
	fn eq(&self, other: &Self) -> bool {
		match (self, other) {
			(Self::Ident(l0), Self::Ident(r0)) => l0 == r0,
			(Self::DottedIdent(l0), Self::DottedIdent(r0)) => l0 == r0,
			_ => false,
		}
	}
}

#[derive(Debug, Clone)]
pub struct ASTIdentErr(&'static str);

impl TryFrom<ASTIdent> for String {
	type Error = ASTIdentErr;

	fn try_from(value: ASTIdent) -> Result<Self, Self::Error> {
		match value {
			ASTIdent::Ident(value) => Ok(value),
			_ => Err(ASTIdentErr(
				"Only regular idents can be compiled to strings",
			)),
		}
	}
}

impl TryFrom<&ASTIdent> for String {
	type Error = ASTIdentErr;

	fn try_from(value: &ASTIdent) -> Result<Self, Self::Error> {
		match value {
			ASTIdent::Ident(value) => Ok(value.into()),
			_ => Err(ASTIdentErr(
				"Only regular idents can be compiled to strings",
			)),
		}
	}
}

/// &str can turned directly into ASTIdent::Ident
impl From<&str> for ASTIdent {
	fn from(value: &str) -> Self {
		Self::Ident(value.into())
	}
}

/// String can turned directly into ASTIdent::Ident
impl From<String> for ASTIdent {
	fn from(value: String) -> Self {
		Self::Ident(value)
	}
}

/// &str can be compared directly with ASTIdent::Ident
impl PartialEq<&str> for ASTIdent {
	fn eq(&self, other: &&str) -> bool {
		match self {
			ASTIdent::Ident(val) => val == other,
			ASTIdent::DottedIdent(_) => false,
		}
	}
}

/// &str can be compared directly with ASTIdent::Ident
impl PartialEq<String> for ASTIdent {
	fn eq(&self, other: &String) -> bool {
		match self {
			ASTIdent::Ident(val) => val == other,
			ASTIdent::DottedIdent(_) => false,
		}
	}
}

#[derive(Debug, Clone)]
pub struct ASTVariable {
	pub ast_type: ASTType,
	pub ident: ASTIdent,
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct ASTLtStmt {
	pub rhs: ASTAssignArg,
	pub lhs: ASTAssignArg,
}

#[derive(Debug, Clone)]
pub struct ASTIfStmt {
	pub conditional: ASTLtStmt,
	pub block: ASTBlock,
}

#[derive(Debug, Clone)]
pub struct ASTReturn {
	// TODO: refactor the type name
	pub expr: ASTAssignmentExpr,
}

#[derive(Debug, Clone)]
pub struct ASTFunction {
	pub name: String,
	/// Int32 arguments
	pub args: Vec<ASTVariable>,
	pub body: ASTBlock,
	pub static_: bool,
	// Option::None is none type
	pub returns: Option<ASTType>,
}

impl ASTFunction {
	pub fn new(
		name: String,
		args: Vec<ASTVariable>,
		body: ASTBlock,
		returns: Option<ASTType>,
	) -> Self {
		Self {
			name,
			args,
			body,
			returns,
			static_: false,
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
	pub methods: Vec<ASTFunction>,
}

impl ASTClass {
	pub fn member<'a>(&'a self, name: &'a str) -> Option<&'a ASTClassMember> {
		self.members.iter().find(|m| m.ident == name)
	}

	pub fn method<'a>(&'a self, name: &'a str) -> Option<&'a ASTFunction> {
		self.methods.iter().find(|m| m.name == name)
	}
}

#[derive(Debug, Clone)]
pub struct ASTEnumValue {
	pub name: String,
	pub value: i32,
}

#[derive(Debug, Clone)]
pub struct ASTEnum {
	pub name: String,
	pub values: Vec<ASTEnumValue>,
}

impl ASTEnum {
	pub fn value<'a>(&'a self, name: &'a str) -> Option<&'a ASTEnumValue> {
		self.values.iter().find(|e| e.name == name)
	}
}
