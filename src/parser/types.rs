#[derive(Debug, Clone)]
pub struct Position {
	pub line: usize,
	pub character: usize,
}

#[derive(Debug, Clone)]
pub struct Range {
	pub start: Position,
	pub end: Position,
}

impl Default for Range {
	fn default() -> Self {
		Self {
			start: Position {
				line: 0,
				character: 0,
			},
			end: Position {
				line: 0,
				character: 0,
			},
		}
	}
}

#[allow(dead_code)]
#[derive(Debug, Default, PartialEq, Eq, Clone, Copy)]
pub enum TokenType {
	#[default]
	Invalid,
	Eof,
	Whitespace,
	Comment,
	Newline,
	Dot,
	Ellipsis,
	Semi,
	Ident,
	LeftParen,
	RightParen,
	LeftBrace,
	RightBrace,
	LeftSquare,
	RightSquare,
	Comma,
	Colon,
	BinLit,
	OctLit,
	HexLit,
	IntLit,
	StringLit,
	CharLit,
	BoolLit,
	NullptrLit,
	Invert,
	IncOp,
	MulOp,
	AddOp,
	ShiftOp,
	BitOp,
	RelOp,
	EquOp,
	LogicOp,

	LocationSpec,
	StorageSpec,
	Type,
	AssignOp,

	FromStmt,
	ImportStmt,
	AsStmt,
	NewStmt,
	DeleteStmt,
	ReturnStmt,
	IfStmt,
	ElifStmt,
	ElseStmt,
	ForStmt,
	WhileStmt,
	DoStmt,

	NoneType,
	Arrow,
	ClassDef,
	EnumDef,
	FunctionDef,
	OperatorDef,
	Decorator,
	Visibility,
	Unsafe,

	// XXX: These only exist because of the current parser structure and aren't real tokens.
	Float32lit,
	Float64lit,
}

#[allow(dead_code)]
#[derive(Debug, Default, Clone)]
pub struct Token {
	type_: TokenType,
	value: String,
	location: Option<Range>,
	length: usize,
}

#[allow(dead_code)]
impl Token {
	pub fn new(type_: TokenType, value: String, location: Range, length: usize) -> Self {
		Self {
			type_,
			value,
			length,
			location: Some(location),
		}
	}

	pub fn from_type(type_: TokenType) -> Self {
		Self {
			type_,
			..Default::default()
		}
	}

	pub fn type_(&self) -> TokenType {
		self.type_
	}

	pub fn set_type(&mut self, type_: TokenType) {
		self.type_ = type_;
	}

	pub fn value(&self) -> &str {
		&self.value
	}

	pub fn set_value(&mut self, value: String) {
		self.value = value;
	}

	pub fn valid(&self) -> bool {
		self.type_ != TokenType::Invalid
	}
}
