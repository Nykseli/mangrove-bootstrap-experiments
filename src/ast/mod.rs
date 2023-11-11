use std::{mem::discriminant, ops::Deref};

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
	Char,
	Int32(ASTInt32Type),
	Array(ASTArrayType),
	String(ASTStringType),
	Class(ASTClass),
	Pointer(ASTPointerType),
	Enum(ASTEnum),
	Template(String),
}

impl ASTType {
	pub fn name(&self) -> String {
		match self {
			ASTType::Int64 => "Int64".into(),
			ASTType::Char => "Char".into(),
			ASTType::Int32(_) => "Int32".into(),
			ASTType::Array(_) => "Array".into(),
			ASTType::String(_) => "String".into(),
			ASTType::Class(class) => class.name.clone(),
			ASTType::Enum(e) => e.name.clone(),
			ASTType::Pointer(ptr) => format!("{}Ptr", ptr.type_.name()),
			ASTType::Template(t) => t.clone(),
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

	pub fn is_templ(&self) -> bool {
		matches!(self, ASTType::Template(_))
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
	Char(char),
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

	pub fn ast_type(&self) -> ASTType {
		match self {
			ASTAssignArg::Static(stat) => stat.value_type.clone(),
			ASTAssignArg::Ident(ident) => ident.ident_type.clone(),
			ASTAssignArg::DottedIdent(ident) => ident.ident_type.clone(),
			ASTAssignArg::Deref(dref) => dref.ident_type.clone(),
		}
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

impl ASTAssignmentExpr {
	fn set_tmpl_type(&mut self, type_: ASTType) {
		match self {
			ASTAssignmentExpr::Arg(ref mut arg) => match arg {
				ASTAssignArg::Static(_) => (),
				ASTAssignArg::Ident(ref mut idnt) => {
					if idnt.ident_type.is_templ() {
						idnt.ident_type = type_
					}
				}
				ASTAssignArg::DottedIdent(ref mut idnt) => {
					if idnt.ident_type.is_templ() {
						idnt.ident_type = type_
					}
				}
				ASTAssignArg::Deref(ref mut idnt) => {
					if idnt.ident_type.is_templ() {
						idnt.ident_type = type_
					}
				}
			},
			ASTAssignmentExpr::Add(_) => todo!(),
			ASTAssignmentExpr::Minus(_) => todo!(),
			ASTAssignmentExpr::FunctionCall(_) => todo!(),
			ASTAssignmentExpr::ClassInit(_) => todo!(),
			ASTAssignmentExpr::ArrayInit(_) => todo!(),
			ASTAssignmentExpr::ASTArrayAccess(_) => todo!(),
		}
	}
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

/// Less-than
#[derive(Debug, Clone)]
pub struct ASTLtStmt {
	pub rhs: ASTAssignArg,
	pub lhs: ASTAssignArg,
}

/// Less or equal
#[derive(Debug, Clone)]
pub struct ASTLeStmt {
	pub rhs: ASTAssignArg,
	pub lhs: ASTAssignArg,
}

/// Greater than
#[derive(Debug, Clone)]
pub struct ASTGtStmt {
	pub rhs: ASTAssignArg,
	pub lhs: ASTAssignArg,
}

/// Greater or equal
#[derive(Debug, Clone)]
pub struct ASTGeStmt {
	pub rhs: ASTAssignArg,
	pub lhs: ASTAssignArg,
}

/// Equal
#[derive(Debug, Clone)]
pub struct ASTEqStmt {
	pub rhs: ASTAssignArg,
	pub lhs: ASTAssignArg,
}

#[derive(Debug, Clone)]
pub enum ASTConditional {
	Lt(ASTLtStmt),
	Le(ASTLeStmt),
	Gt(ASTGtStmt),
	Ge(ASTGeStmt),
	Eq(ASTEqStmt),
}

#[derive(Debug, Clone)]
pub struct ASTIfStmt {
	pub conditional: ASTConditional,
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
	// Name of the template
	pub template: Option<String>,
	// Assigned type of the template
	pub tmpl_type: Option<Box<ASTType>>,
}

impl ASTClass {
	pub fn member<'a>(&'a self, name: &'a str) -> Option<&'a ASTClassMember> {
		self.members.iter().find(|m| m.ident == name)
	}

	pub fn method<'a>(&'a self, name: &'a str) -> Option<&'a ASTFunction> {
		self.methods.iter().find(|m| m.name == name)
	}

	pub fn set_template_types(&mut self) {
		let type_ = if let Some(t) = &self.tmpl_type {
			t.deref().clone()
		} else {
			panic!("No template type in class {}", self.name);
		};

		for member in &mut self.members {
			if let ASTType::Template(_) = member.type_ {
				member.type_ = type_.clone()
			}
		}

		for fun in &mut self.methods {
			for arg in &mut fun.args {
				match arg.ast_type {
					ASTType::Class(ref mut class) if arg.ident.ident() == "this" => {
						class.tmpl_type = Some(Box::new(type_.clone()));
						class.set_template_types();
					}
					ASTType::Template(_) => {
						arg.ast_type = type_.clone();
					}
					_ => continue,
				}
			}

			for var in &mut fun.body.variables {
				match var.ast_type {
					ASTType::Class(ref mut class) if var.ident.ident() == "this" => {
						class.tmpl_type = Some(Box::new(type_.clone()));
						class.set_template_types();
					}
					ASTType::Template(_) => {
						var.ast_type = type_.clone();
					}
					_ => continue,
				}
			}

			for stmt in &mut fun.body.statements {
				match stmt {
					ASTBlockStatement::Assignment(ref mut assign) => {
						if assign.variable.ast_type.is_templ() {
							assign.variable.ast_type = type_.clone();
							assign.expr.set_tmpl_type(type_.clone());
						}
					}
					ASTBlockStatement::DerefAssignment(ref mut assign) => {
						if assign.variable.ast_type.is_templ() {
							assign.variable.ast_type = type_.clone();
							assign.expr.set_tmpl_type(type_.clone());
						}
					}
					ASTBlockStatement::Return(ref mut ret) => {
						ret.expr.set_tmpl_type(type_.clone());
					}
					ASTBlockStatement::FunctionCall(_) => todo!(),
					ASTBlockStatement::IfStmt(_) => todo!(),
				}
			}

			if let Some(ret) = &fun.returns {
				if matches!(ret, ASTType::Template(_)) {
					fun.returns = Some(type_.clone())
				}
			}
		}
	}
}

impl<'a> TryFrom<&'a ASTType> for &'a ASTClass {
	type Error = ();

	fn try_from(value: &'a ASTType) -> Result<Self, Self::Error> {
		match value {
			ASTType::Class(class) => Ok(class),
			_ => Err(()),
		}
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
