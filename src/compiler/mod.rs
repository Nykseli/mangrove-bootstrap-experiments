use crate::{
	ast::{
		ASTArrayAccess, ASTArrayInit, ASTAssignArg, ASTAssignmentExpr, ASTBlock, ASTBlockStatement,
		ASTClass, ASTClassInit, ASTEnum, ASTFunction, ASTFunctionCallArg, ASTIdent, ASTLtStmt,
		ASTType, StaticValue,
	},
	parser::parse::Parser,
};

#[derive(Debug, Clone)]
enum InternalType {
	String,
	Int32,
	Int64,
	Char,
}

impl InternalType {
	fn size(&self) -> i32 {
		match self {
			// ptr + size
			InternalType::String => 8,
			// i32 is always 4 bytes
			InternalType::Int32 => 4,
			InternalType::Int64 => 8,
			// Char is 4 so it can fit UTF-8
			InternalType::Char => 4,
		}
	}

	fn is32b(&self) -> bool {
		match self {
			// string is 2x32b values
			InternalType::String => false,
			InternalType::Int32 => true,
			InternalType::Int64 => false,
			InternalType::Char => true,
		}
	}
}

impl From<&str> for InternalType {
	fn from(value: &str) -> Self {
		if value == "String" {
			InternalType::String
		} else if value == "Int32" {
			InternalType::Int32
		} else if value == "Int64" {
			InternalType::Int64
		} else if value == "Char" {
			InternalType::Char
		} else {
			panic!("{}", format!("No internal type '{value}'"))
		}
	}
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
struct CompiledArray {
	/// Total size of the structure in bytes. Element size + length
	total_size: i32,
	/// How many elements the array has
	length: i32,
	/// What type of element the Array has
	element_type: Box<CompiledType>,
}

#[derive(Debug, Clone)]
struct CompiledPointer {
	/// What type of element the Pointer has
	element_type: Box<CompiledType>,
}

#[derive(Debug, Clone)]
struct CompiledClassMember {
	ident: String,
	type_: CompiledType,
	/// How many bytes from the 0 index
	offset: i32,
}

#[derive(Debug, Clone)]
struct CompiledClass {
	/// Name of the class
	name: String,
	members: Vec<CompiledClassMember>,
	/// How much space the class/member take
	total_size: i32,
}

impl CompiledClass {
	fn member<'a>(&'a self, name: &str) -> &'a CompiledClassMember {
		self.members
			.iter()
			.find(|m| m.ident == name)
			.unwrap_or_else(|| panic!("Class {} doesn't have a member {name}", self.name))
	}
}

#[derive(Debug, Clone)]
struct CompiledEnumValue {
	name: String,
	value: i32,
}

#[derive(Debug, Clone)]
struct CompiledEnum {
	/// Name of the class
	name: String,
	values: Vec<CompiledEnumValue>,
}

impl CompiledEnum {
	fn new(enum_: &ASTEnum) -> Self {
		let values = enum_
			.values
			.iter()
			.map(|v| CompiledEnumValue {
				name: v.name.clone(),
				value: v.value,
			})
			.collect();
		Self {
			values,
			name: enum_.name.clone(),
		}
	}

	fn value<'a>(&'a self, name: &str) -> &'a CompiledEnumValue {
		self.values
			.iter()
			.find(|m| m.name == name)
			.unwrap_or_else(|| panic!("Enum {} doesn't have a value {name}", self.name))
	}
}

#[derive(Debug, Clone)]
enum CompiledType {
	Array(CompiledArray),
	Class(CompiledClass),
	Pointer(CompiledPointer),
	Enum(CompiledEnum),
	Internal(InternalType),
	Template(String),
}

impl CompiledType {
	fn size(&self) -> i32 {
		match self {
			CompiledType::Array(arr) => arr.total_size,
			CompiledType::Class(class) => class.total_size,
			CompiledType::Internal(intr) => intr.size(),
			// enums are i32 so 4 bytes
			CompiledType::Enum(_) => 4,
			CompiledType::Pointer(_) => 4,
			CompiledType::Template(_) => unreachable!("Templates don't have a size"),
		}
	}

	fn is32b(&self) -> bool {
		match &self {
			// Array and class is pointer and address are i32
			CompiledType::Array(_) | CompiledType::Class(_) => true,
			// enum is i32 so i32b
			CompiledType::Enum(_) => true,
			CompiledType::Pointer(_) => true,
			CompiledType::Internal(intr) => intr.is32b(),
			CompiledType::Template(_) => unreachable!("Templates don't have a size"),
		}
	}

	fn copy_size(&self) -> Option<i32> {
		match self {
			CompiledType::Array(a) => Some(a.total_size),
			CompiledType::Class(c) => Some(c.total_size),
			CompiledType::Internal(_) => None,
			// enums are integers so no need to copy anything
			CompiledType::Enum(_) => None,
			// Pointers are just integers so no need to copy anything
			CompiledType::Pointer(_) => None,
			CompiledType::Template(_) => unreachable!("Templates cannot be copied"),
		}
	}
}

#[derive(Debug, Clone)]
struct CompiledVariable {
	type_: CompiledType,
	ident: String,
}

impl CompiledVariable {
	fn local_type(&self) -> &'static str {
		match self.type_.is32b() {
			true => "i32",
			false => "i64",
		}
	}
}

#[derive(Debug, Clone)]
struct StaticString {
	start: i32,
	value: String,
}

impl StaticString {
	/// Create the wasm i64 representation of static string
	fn value(&self) -> u64 {
		// Set pointer and pointer size (string length)
		// Using a 64 bit value here lets us set the string pointer and length
		// in a single instruction.
		let start: u64 = self.start as u64;
		let size: u64 = self.value.len() as u64;
		// wasm little endian byte order so the length is least significant
		size << 32 | start
	}
}

#[derive(Debug, Clone)]
enum StaticData {
	StaticString(StaticString),
}

impl StaticData {
	fn start(&self) -> i32 {
		match self {
			StaticData::StaticString(s) => s.start,
		}
	}

	fn size(&self) -> i32 {
		match self {
			StaticData::StaticString(s) => s.value.len() as i32,
		}
	}

	fn value(&self) -> String {
		match self {
			StaticData::StaticString(s) => format!("\"{}\"", s.value.replace('\n', "\\n")),
		}
	}
}

#[derive(Debug, Clone)]
struct CompileCtx {
	/// Variables in the current context
	vars: Vec<CompiledVariable>,
	/// All the compiled classes
	classes: Vec<CompiledClass>,
	/// All the compiled enums
	enums: Vec<CompiledEnum>,
	/// Statically allocated memory
	static_data: Vec<StaticData>,
}

impl CompileCtx {
	fn find_compiled_class<'a>(
		&'a self,
		name: &str,
		tmpl: Option<&str>,
	) -> Option<&'a CompiledClass> {
		let name = if let Some(tmpl) = tmpl {
			format!("{name}{tmpl}")
		} else {
			name.into()
		};

		self.classes.iter().find(|c| c.name == name)
	}

	fn find_variable<'a>(&'a self, name: &str) -> &'a CompiledVariable {
		self.vars
			.iter()
			.find(|v| v.ident == name)
			.unwrap_or_else(|| panic!("Variable '{name}' is not defined"))
	}

	fn check_enum<'a>(&'a self, name: &str) -> Option<&'a CompiledEnum> {
		self.enums.iter().find(|e| e.name == name)
	}

	fn find_compiled_type(&self, name: &str) -> CompiledType {
		// TODO: Arrays
		let class = self.classes.iter().find(|c| c.name == name);
		if let Some(cls) = class {
			return CompiledType::Class(cls.clone());
		}

		let enum_ = self.check_enum(name);
		if let Some(enum_) = enum_ {
			return CompiledType::Enum(enum_.clone());
		}

		let intr: InternalType = name.into();
		CompiledType::Internal(intr)
	}

	fn var_stack_offset(&self, var: &CompiledVariable) -> i32 {
		let mut offset = 0;
		for ctx_var in &self.vars {
			if ctx_var.ident == var.ident {
				return offset;
			}
			offset += ctx_var.type_.size()
		}
		panic!("Compiled variable {var:#?} is not in stack")
	}

	fn ast_type_into_compiled(&mut self, ast_type: &ASTType) -> CompiledType {
		match ast_type {
			ASTType::Char => CompiledType::Internal(InternalType::Char),
			ASTType::Int64 => CompiledType::Internal(InternalType::Int64),
			ASTType::Int32(_) => CompiledType::Internal(InternalType::Int32),
			ASTType::String(_) => CompiledType::Internal(InternalType::String),
			ASTType::Class(class) => {
				let tmpl = class.tmpl_type.as_ref().map(|tmpl| tmpl.name());

				if self
					.find_compiled_class(&class.name, tmpl.as_deref())
					.is_none() && class.tmpl_type.is_some()
					&& class.template.is_some()
				{
					self.add_ast_class(class)
				}

				let class = self
					.find_compiled_class(&class.name, tmpl.as_deref())
					.unwrap()
					.clone();
				CompiledType::Class(class)
			}
			ASTType::Array(array) => {
				let type_ = self.ast_type_into_compiled(&array.type_);
				// We have to know the size of the array when compiling the type
				assert!(array.size > -1);
				CompiledType::Array(CompiledArray {
					total_size: type_.size() * array.size,
					length: array.size,
					element_type: Box::new(type_),
				})
			}
			ASTType::Pointer(ptr) => {
				let type_ = self.ast_type_into_compiled(&ptr.type_);
				CompiledType::Pointer(CompiledPointer {
					element_type: Box::new(type_),
				})
			}
			ASTType::Enum(enum_) => CompiledType::Enum(CompiledEnum::new(enum_)),
			ASTType::Template(t) => CompiledType::Template(t.into()),
		}
	}

	fn add_ast_class(&mut self, class: &ASTClass) {
		let mut offset: i32 = 0;
		let mut members: Vec<CompiledClassMember> = Vec::new();
		for member in &class.members {
			let type_ = if let ASTType::Template(_) = &member.type_ {
				class.tmpl_type.as_ref().unwrap()
			} else {
				&member.type_
			};

			let type_ = self.ast_type_into_compiled(type_);
			let size = type_.size();
			members.push(CompiledClassMember {
				ident: member.ident.clone(),
				type_,
				offset,
			});
			offset += size;
		}

		let name = if let Some(type_) = &class.tmpl_type {
			format!("{}{}", class.name, type_.name())
		} else {
			class.name.clone()
		};

		self.classes.push(CompiledClass {
			name,
			members,
			total_size: offset,
		})
	}

	fn add_static_string(&mut self, string: &str) -> StaticString {
		let start = if let Some(stat) = self.static_data.last() {
			// Next start should be right after the previous memory allocation
			stat.size() + stat.start()
		} else {
			// first 1024 * 1024 bytes are reserved for internal functions and data
			1024 * 1024
		};

		let new_string = StaticData::StaticString(StaticString {
			start,
			value: string.into(),
		});
		self.static_data.push(new_string);

		StaticString {
			start,
			value: string.into(),
		}
	}

	fn load_dotted_ident(&self, ident: (&str, &str), offset: i32) -> InitExpression {
		let dotted = ident.1;
		let ident = ident.0;
		if let Some(enum_) = self.check_enum(ident) {
			let value = enum_.value(dotted);
			return InitExpression {
				offset,
				is32b: true,
				expr: format!("(i32.const {})", value.value),
			};
		}

		let var = self.find_variable(ident);
		match &var.type_ {
			CompiledType::Array(_arr) => todo!("Implement Array members"),
			CompiledType::Internal(inter) => match inter {
				InternalType::String => {
					if dotted != "length" {
						panic!("String type doesn't have a member '{dotted}'")
					}

					InitExpression::new(
						offset,
						format!("(i32.wrap_i64 (i64.shr_u (local.get ${ident}) (i64.const 32)))"),
					)
				}
				InternalType::Int32 => todo!("Implement Int32 members"),
				InternalType::Int64 => todo!("Implement Int64 members"),
				InternalType::Char => todo!("Implement Char members"),
			},
			CompiledType::Class(class) => {
				let member = class.member(dotted);
				// dotted ident offset are always the member offset
				let offset = /* offset +  */member.offset;
				if let CompiledType::Class(_) = &member.type_ {
					// Just load the offset to the member
					InitExpression::new(
						offset,
						format!("(i32.add (get_local ${ident}) (i32.const {offset}))"),
					)
				} else if member.type_.is32b() {
					InitExpression::new(
						offset,
						format!("(i32.load (i32.add (get_local ${ident}) (i32.const {offset})))"),
					)
				} else {
					InitExpression::new_64b(
						offset,
						format!("(i64.load (i32.add (get_local ${ident}) (i32.const {offset})))"),
					)
				}
			}
			CompiledType::Enum(_) => todo!("Implement enum values"),
			CompiledType::Pointer(_) => todo!(),
			CompiledType::Template(_) => todo!(),
		}
	}
}

#[derive(Debug)]
struct InitExpression {
	/// Offset to the memory area where the expression value should be saved.
	/// This can be ignored when, for exaxmple, Int32 variable is assigned
	offset: i32,
	/// The wasm expression in string format
	expr: String,
	/// i32.store or i64.store
	is32b: bool,
}

impl InitExpression {
	fn new(offset: i32, expr: String) -> Self {
		Self {
			offset,
			expr,
			is32b: true,
		}
	}

	fn new_64b(offset: i32, expr: String) -> Self {
		Self {
			offset,
			expr,
			is32b: false,
		}
	}

	fn store(&self) -> &'static str {
		match self.is32b {
			true => "i32.store",
			false => "i64.store",
		}
	}
}

trait ASTCompile<T> {
	fn compile(&self, ctx: &mut CompileCtx, ctype: &T, offset: i32) -> Vec<InitExpression>;
}

impl ASTAssignArg {
	fn compile(&self, ctx: &mut CompileCtx, _ctype: &CompiledType, offset: i32) -> InitExpression {
		// TODO: Make sure that the ast types are actually correct.
		//       In theory, AST sould catch the type errors but better to be safe than sorry
		match self {
			ASTAssignArg::Static(stat) => match &stat.value {
				StaticValue::Int32(val) => {
					InitExpression::new(offset, format!("(i32.const {val})"))
				}
				StaticValue::Int64(val) => {
					InitExpression::new_64b(offset, format!("(i64.const {val})"))
				}
				StaticValue::String(string) => {
					let sstring = ctx.add_static_string(string);
					let value = sstring.value();
					InitExpression::new_64b(offset, format!("(i64.const {})", value))
				}
				StaticValue::Char(char) => {
					InitExpression::new(offset, format!("(i32.const {})", *char as u64))
				}
			},

			ASTAssignArg::Ident(ident) => {
				InitExpression::new(offset, format!("(get_local ${})", ident.ident))
			}
			ASTAssignArg::DottedIdent(ident) => {
				ctx.load_dotted_ident((&ident.ident.0, &ident.ident.1), offset)
			}
			ASTAssignArg::Deref(deref) => {
				// TODO: handle classes and arrays
				let type_ = ctx.ast_type_into_compiled(&deref.ident_type);
				if type_.is32b() {
					InitExpression::new(offset, format!("(i32.load (get_local ${}))", deref.ident))
				} else {
					InitExpression::new(offset, format!("(i64.load (get_local ${}))", deref.ident))
				}
			}
		}
	}
}

impl ASTLtStmt {
	fn compile(&self, ctx: &mut CompileCtx) -> String {
		format!(
			"(i32.lt_s {} {})",
			self.lhs
				.compile(ctx, &CompiledType::Internal(InternalType::Int32), 0)
				.expr,
			self.rhs
				.compile(ctx, &CompiledType::Internal(InternalType::Int32), 0)
				.expr
		)
	}
}

impl ASTFunctionCallArg {
	fn compile(&self, ctx: &mut CompileCtx) -> String {
		match &self {
			ASTFunctionCallArg::Char(v) => format!("(i32.const {})", *v as i32),
			ASTFunctionCallArg::Int32(v) => format!("(i32.const {v})"),
			ASTFunctionCallArg::Int64(v) => format!("(i64.const {v})"),
			ASTFunctionCallArg::String(s) => {
				let sstring = ctx.add_static_string(s);
				let value = sstring.value();
				format!("(i64.const {value})")
			}
			ASTFunctionCallArg::Ident(i) => format!("(get_local ${i})"),
			ASTFunctionCallArg::DottedIdent(ident) => {
				ctx.load_dotted_ident((&ident.0, &ident.1), 0).expr
			}
		}
	}
}

impl ASTCompile<CompiledArray> for ASTArrayInit {
	fn compile(
		&self,
		ctx: &mut CompileCtx,
		array: &CompiledArray,
		offset: i32,
	) -> Vec<InitExpression> {
		let mut elements: Vec<InitExpression> = Vec::new();

		for (idx, arg) in self.args.iter().enumerate() {
			let offset = offset + (array.element_type.size() * idx as i32);
			let exprs = arg.compile(ctx, &array.element_type, offset);
			elements.extend(exprs.into_iter());
		}
		elements
	}
}

impl ASTArrayAccess {
	fn compile(&self, ctx: &mut CompileCtx, array: &CompiledArray, offset: i32) -> InitExpression {
		let is32b = array.element_type.is32b();
		// TODO: dotted ident access
		let ident: String = (&self.ident).try_into().unwrap();
		let size = array.element_type.size();
		let idx = self.arg.compile(ctx, &array.element_type, offset);
		assert!(idx.len() == 1, "Array idx expression len needs to be 1");
		let idx = &idx[0].expr;
		let expr = format!("(i32.add (get_local ${ident}) (i32.mul (i32.const {size}) {idx}))\n");
		let expr = match array.element_type.as_ref() {
			// We want to load the the address of the class or array ptr
			CompiledType::Array(_) | CompiledType::Class(_) => expr,
			// We want to load the direct value from the address
			CompiledType::Internal(val) => match val.is32b() {
				true => format!("(i32.load {expr})\n"),
				false => format!("(i64.load {expr})\n"),
			},
			CompiledType::Enum(_) => format!("(i32.load {expr})\n"),
			CompiledType::Pointer(_) => todo!("Implement pointer array access"),
			CompiledType::Template(_) => todo!("Implement template array access"),
		};

		InitExpression {
			offset,
			expr,
			is32b,
		}
	}
}

impl ASTCompile<CompiledClass> for ASTClassInit {
	fn compile(
		&self,
		ctx: &mut CompileCtx,
		class: &CompiledClass,
		offset: i32,
	) -> Vec<InitExpression> {
		let mut assigns: Vec<InitExpression> = Vec::new();
		for arg in &self.args {
			let member = class.member(&arg.ident);
			let member_type = &member.type_;
			let offset = offset + member.offset;
			let exprs = arg.arg.compile(ctx, member_type, offset);
			assigns.extend(exprs.into_iter());
		}
		assigns
	}
}

impl ASTCompile<CompiledType> for ASTAssignmentExpr {
	fn compile(
		&self,
		ctx: &mut CompileCtx,
		type_: &CompiledType,
		offset: i32,
	) -> Vec<InitExpression> {
		match self {
			ASTAssignmentExpr::Arg(arg) => vec![arg.compile(ctx, type_, offset)],
			ASTAssignmentExpr::Add(add) => {
				// Assuming lhs has same type as rhs
				let fn_type = match &add.lhs {
					ASTAssignArg::Static(val) => &val.value_type,
					ASTAssignArg::Ident(val) => &val.ident_type,
					ASTAssignArg::DottedIdent(val) => &val.ident_type,
					ASTAssignArg::Deref(_) => todo!(),
				};

				let function = match fn_type {
					ASTType::Int64 => "i64.add",
					ASTType::Int32(_) => "i32.add",
					ASTType::Pointer(_) => "i32.add",
					ASTType::String(_) => "call $__string_concat2",
					ASTType::Class(_) => unreachable!("Cannot add two custom types"),
					ASTType::Array(_) => unreachable!("Cannot add two array types"),
					ASTType::Enum(_) => unreachable!("Cannot add two enum types"),
					ASTType::Template(_) => unreachable!("Cannot add two templates"),
					ASTType::Char => todo!(),
				};

				let ctype = ctx.ast_type_into_compiled(fn_type);
				let lhs = add.lhs.compile(ctx, &ctype, offset).expr;
				let rhs = add.rhs.compile(ctx, &ctype, offset).expr;
				let is32b = ctype.is32b();
				let expr = format!("({function} {lhs} {rhs})");
				vec![InitExpression {
					offset,
					expr,
					is32b,
				}]
			}
			ASTAssignmentExpr::Minus(minus) => {
				// Assuming lhs has same type as rhs
				let fn_type = match &minus.lhs {
					ASTAssignArg::Static(val) => &val.value_type,
					ASTAssignArg::Ident(val) => &val.ident_type,
					ASTAssignArg::DottedIdent(val) => &val.ident_type,
					ASTAssignArg::Deref(_) => todo!(),
				};
				let ctype = ctx.ast_type_into_compiled(fn_type);
				let lhs = minus.lhs.compile(ctx, &ctype, offset).expr;
				let rhs = minus.rhs.compile(ctx, &ctype, offset).expr;
				let is32b = ctype.is32b();
				let expr = format!("(i32.sub {lhs} {rhs})");
				vec![InitExpression {
					offset,
					expr,
					is32b,
				}]
			}
			ASTAssignmentExpr::FunctionCall(func) => {
				if func.name == "__sizeof" {
					if func.args.len() != 1 {
						panic!("__sizeof takes exactly 1 argument");
					}

					let arg_name = if let ASTFunctionCallArg::Ident(arg) = &func.args[0] {
						arg
					} else {
						panic!("__sizeof only accepts identifiers as an argument");
					};

					let type_ = ctx.find_compiled_type(arg_name);
					let expr = format!("(i32.const {})\n", type_.size());
					return vec![InitExpression {
						expr,
						offset,
						is32b: false,
					}];
				}

				match &func.variable {
					Some(var) if matches!(var.ast_type, ASTType::String(_)) => {
						if func.name == "get" {
							assert!(func.args.len() == 1, "String.get has exactly one argument");
							let arg = func.args[0].compile(ctx);
							// get the variable pointer
							let expr = format!(
								"(i32.load8_u (i32.add (i32.wrap_i64 (get_local ${})) {arg}))",
								var.ident.ident()
							);
							return vec![InitExpression {
								expr,
								offset,
								is32b: true,
							}];
						}
					}
					_ => (),
				}

				let (arg, fn_name) = if let Some(var) = &func.variable {
					let fn_name = match &var.ast_type {
						ASTType::Class(c) if c.tmpl_type.is_some() => {
							format!(
								"__{}{}_class_{}",
								var.ast_type.name(),
								c.tmpl_type.as_deref().unwrap().name(),
								func.name,
							)
						}
						_ => format!("__{}_class_{}", var.ast_type.name(), func.name),
					};
					(format!("(get_local ${})", var.ident.ident()), fn_name)
				} else {
					(String::new(), func.name.clone())
				};

				let mut arg_str = arg;
				for arg in &func.args {
					arg_str.push_str(&arg.compile(ctx));
				}

				let expr = format!("(call ${} {arg_str})\n", fn_name);
				let is32b = if let Some(type_) = &func.variable {
					ctx.ast_type_into_compiled(&type_.ast_type).is32b()
				} else {
					// fn call is not assigned to variable so the bitness doesn't matter
					true
				};

				vec![InitExpression {
					expr,
					offset,
					is32b,
				}]
			}
			ASTAssignmentExpr::ClassInit(class) => {
				let class_type = if let CompiledType::Class(class_type) = type_ {
					class_type
				} else {
					panic!("Expected class type for class init");
				};
				class.compile(ctx, class_type, offset)
			}
			ASTAssignmentExpr::ArrayInit(array) => {
				let array_type = if let CompiledType::Array(array_type) = type_ {
					array_type
				} else {
					panic!("Expected class type for class init");
				};
				array.compile(ctx, array_type, offset)
			}
			ASTAssignmentExpr::ASTArrayAccess(access) => {
				// TODO: dotted ident support
				let var_type = &ctx.find_variable(access.ident.ident()).type_.clone();
				let var_arr = if let CompiledType::Array(arr) = var_type {
					arr
				} else {
					panic!("Expected array type for ASTArrayAccess")
				};

				vec![access.compile(ctx, var_arr, offset)]
			}
		}
	}
}

fn compile_variable_assignment(
	ctx: &mut CompileCtx,
	target: &CompiledVariable,
	expr: &ASTAssignmentExpr,
	is_deref: bool,
	is_target_dotted: bool,
) -> String {
	match &target.type_ {
		CompiledType::Array(array) => {
			let expr = if let ASTAssignmentExpr::ArrayInit(expr) = expr {
				expr
			} else if let ASTAssignmentExpr::ASTArrayAccess(access) = expr {
				// TODO: dotted ident support
				let var_type = &ctx.find_variable(access.ident.ident()).type_.clone();
				let var_arr = if let CompiledType::Array(arr) = var_type {
					arr
				} else {
					panic!("Expected array type for ASTArrayAccess")
				};

				let expr = access.compile(ctx, var_arr, 0).expr;
				return format!("(set_local ${} {expr})", target.ident);
			} else if let ASTAssignmentExpr::Arg(arg) = expr {
				return arg.compile(ctx, &target.type_, 0).expr;
			} else if let ASTAssignmentExpr::FunctionCall(func) = expr {
				let mut arg_str = String::new();
				for arg in &func.args {
					arg_str.push_str(&arg.compile(ctx));
				}

				if target.type_.copy_size().is_some() {
					let offset = ctx.var_stack_offset(target);
					arg_str.push_str(&format!(
						"(i32.add (get_local $__stack_ptr) (i32.const {offset}))"
					));
				}

				let expr = format!("(call ${} {arg_str})\n", func.name);
				return format!("(set_local ${} {expr})", target.ident);
			} else {
				panic!("Expected Array init expression");
			};

			let stack_offset = ctx.var_stack_offset(target);
			let mut assignment = String::new();

			let inits = expr.compile(ctx, array, 0);
			for init in inits {
				assignment.push_str(&format!(
					"({} (i32.add (get_local $__stack_ptr) (i32.const {})) {})\n",
					init.store(),
					init.offset + stack_offset,
					init.expr
				))
			}

			assignment.push_str(&format!(
				"(set_local ${} (i32.add (get_local $__stack_ptr) (i32.const {stack_offset})))\n",
				target.ident
			));
			assignment
		}
		CompiledType::Class(class) => {
			let expr = if let ASTAssignmentExpr::ClassInit(expr) = expr {
				expr
			} else if let ASTAssignmentExpr::ASTArrayAccess(access) = expr {
				// TODO: dotted ident support
				let var_type = &ctx.find_variable(access.ident.ident()).type_.clone();
				let var_arr = if let CompiledType::Array(arr) = var_type {
					arr
				} else {
					panic!("Expected array type for ASTArrayAccess")
				};

				let expr = access.compile(ctx, var_arr, 0).expr;
				return format!("(set_local ${} {expr})", target.ident);
			} else if let ASTAssignmentExpr::Arg(arg) = expr {
				if !is_target_dotted {
					let expr = arg.compile(ctx, &target.type_, 0).expr;
					return format!("(set_local ${} {expr})\n", target.ident);
				}
				return arg.compile(ctx, &target.type_, 0).expr;
			} else if let ASTAssignmentExpr::FunctionCall(func) = expr {
				let mut arg_str = String::new();
				for arg in &func.args {
					arg_str.push_str(&arg.compile(ctx));
				}

				if target.type_.copy_size().is_some() {
					let offset = ctx.var_stack_offset(target);
					arg_str.push_str(&format!(
						"(i32.add (get_local $__stack_ptr) (i32.const {offset}))"
					));
				}

				let expr = format!("(call ${} {arg_str})\n", func.name);
				return format!("(set_local ${} {expr})", target.ident);
			} else {
				panic!("Expected Class init expression {expr:#?}");
			};

			let stack_offset = ctx.var_stack_offset(target);
			let mut assignment = String::new();

			let inits = expr.compile(ctx, class, 0);
			for init in inits {
				assignment.push_str(&format!(
					"({} (i32.add (get_local $__stack_ptr) (i32.const {})) {})\n",
					init.store(),
					init.offset + stack_offset,
					init.expr
				))
			}

			assignment.push_str(&format!(
				"(set_local ${} (i32.add (get_local $__stack_ptr) (i32.const {stack_offset})))\n",
				target.ident
			));
			assignment
		}
		CompiledType::Internal(_internal) => {
			let expr = expr.compile(ctx, &target.type_, 0);
			assert!(expr.len() == 1, "Internal assign expr len needs to be 1");
			let expr = &expr[0].expr;
			format!("(set_local ${} {expr})\n", target.ident)
		}
		CompiledType::Enum(_) => {
			let expr = expr.compile(ctx, &target.type_, 0);
			assert!(expr.len() == 1, "Enum assign expr len needs to be 1");
			let expr = &expr[0].expr;
			format!("(set_local ${} {expr})\n", target.ident)
		}
		CompiledType::Pointer(ptr) => {
			match expr {
				ASTAssignmentExpr::FunctionCall(func) => {
					if func.name != "__allocate_bytes" {
						panic!("can only init ptr with __allocate_bytes")
					}
				}
				ASTAssignmentExpr::Arg(_) => (),
				ASTAssignmentExpr::Add(_) => (),
				ASTAssignmentExpr::Minus(_) => (),
				_ => {
					panic!("Can only initialise a pointer with function call or nullptr {expr:#?}")
				}
			}
			let expr = expr.compile(ctx, &target.type_, 0);
			assert!(expr.len() == 1, "ptr assign expr len needs to be 1");
			let expr = &expr[0].expr;
			if is_deref {
				let size = if ptr.element_type.is32b() {
					"i32"
				} else {
					"i64"
				};

				format!("({size}.store (get_local ${}) {expr})\n", target.ident)
			} else {
				format!("(set_local ${} {expr})\n", target.ident)
			}
		}
		CompiledType::Template(_) => todo!(),
	}
}

fn compile_format_print_call(ctx: &mut CompileCtx, args: &Vec<ASTFunctionCallArg>) -> String {
	let mut function = String::new();

	if args.is_empty() {
		panic!("__print_format requires at least 1 argument");
	}

	let format_arg = if let ASTFunctionCallArg::String(s) = &args[0] {
		s
	} else {
		panic!("__print_format's first argument needs to be a static string");
	};

	let mut format = &format_arg[..];
	let mut arg_idx: usize = 1;
	let mut str_buf = String::new();
	while !format.is_empty() {
		if format.starts_with("{}") {
			let sstring = ctx.add_static_string(&str_buf);
			let value = sstring.value();
			// Print the current string
			str_buf = String::new();
			function.push_str(&format!("(call $__print_str (i64.const {value}))\n"));

			// Print the argument
			let arg = &args[arg_idx];
			match arg {
				ASTFunctionCallArg::Char(c) => {
					function.push_str(&format!("(call $__print_char (i32.const {}))\n", *c as i32))
				}
				ASTFunctionCallArg::Int32(val) => {
					function.push_str(&format!("(call $__print_int (i32.const {}))\n", val))
				}
				ASTFunctionCallArg::Int64(val) => {
					function.push_str(&format!("(call $__print_int (i64.const {}))\n", val))
				}
				ASTFunctionCallArg::String(s) => {
					let sstring = ctx.add_static_string(s);
					let value = sstring.value();
					function.push_str(&format!("(call $__print_str (i64.const {value}))\n"));
				}
				ASTFunctionCallArg::Ident(ident) => {
					function.push_str(&format!("(call $__print_int (get_local ${ident}))\n"))
				}
				ASTFunctionCallArg::DottedIdent(_) => todo!(),
			}

			format = &format[2..];
			arg_idx += 1;
		} else {
			str_buf.push_str(&format[0..1]);
			format = &format[1..];
		}
	}

	if !str_buf.is_empty() {
		let sstring = ctx.add_static_string(&str_buf);
		let value = sstring.value();
		function.push_str(&format!("(call $__print_str (i64.const {value}))\n"));
	}

	function
}

// Compile block, variables are not block scoped
fn compile_block(
	ctx: &mut CompileCtx,
	function: &ASTFunction,
	block: &ASTBlock,
	body: &mut String,
	variables: &mut String,
	fn_stack_size: i32,
) {
	for statment in &block.statements {
		match statment {
			ASTBlockStatement::DerefAssignment(assign) | ASTBlockStatement::Assignment(assign) => {
				let is_deref = matches!(statment, ASTBlockStatement::DerefAssignment(_));

				if let ASTIdent::DottedIdent((ident, dotted)) = &assign.variable.ident {
					let var = ctx.find_variable(ident).clone();
					let class = if let CompiledType::Class(class) = &var.type_ {
						class
					} else {
						panic!("Can only set class members")
					};

					let member = class.member(dotted);
					let offset = member.offset;
					let store = if member.type_.is32b() {
						format!("(i32.store (i32.add (get_local ${ident}) (i32.const {offset}))")
					} else {
						format!("(i64.store (i32.add (get_local ${ident}) (i32.const {offset}))")
					};
					let expr = compile_variable_assignment(ctx, &var, &assign.expr, is_deref, true);
					body.push_str(&format!("{store} {expr})"));
					continue;
				}

				let ident: String = (&assign.variable.ident)
					.try_into()
					.unwrap_or_else(|_| panic!("{:#?}", assign.variable));
				// TODO: remove the clone here
				let var = ctx.find_variable(&ident).clone();
				body.push_str(&compile_variable_assignment(
					ctx,
					&var,
					&assign.expr,
					is_deref,
					false,
				));
				if !assign.reassignment {
					variables.push_str(&format!("(local ${} {})\n", var.ident, var.local_type()));
				}
			}
			ASTBlockStatement::FunctionCall(call) => {
				// TODO: special functions
				if call.name == "__print_format" {
					let special = compile_format_print_call(ctx, &call.args);
					body.push_str(&special);
					continue;
				}

				let args: Vec<String> = call.args.iter().map(|a| a.compile(ctx)).collect();
				let args = args.join(" ");
				let name = if let Some(var) = &call.variable {
					let local = if call.static_ {
						"".into()
					} else {
						// TODO: dotted ident support
						let var_name: String = (&var.ident).try_into().unwrap();
						format!("(get_local ${var_name})")
					};

					match &var.ast_type {
						ASTType::Class(c) if c.tmpl_type.is_some() => {
							format!(
								"__{}{}_class_{} {local}",
								var.ast_type.name(),
								c.tmpl_type.as_deref().unwrap().name(),
								call.name,
							)
						}
						_ => format!("__{}_class_{} {local}", var.ast_type.name(), call.name),
					}
				} else {
					call.name.clone()
				};
				body.push_str(&format!("(call ${name} {args})\n"));
			}
			ASTBlockStatement::Return(stmt) => {
				// TODO: support none return
				let ret = function
					.returns
					.as_ref()
					.unwrap_or_else(|| panic!("Cannot return none"));
				let type_ = ctx.ast_type_into_compiled(ret);
				let ret = stmt.expr.compile(ctx, &type_, 0);
				assert!(ret.len() == 1, "Return expression len needs to be 1");
				let ret = &ret[0].expr;

				// TODO: mem_n_copy to the $__return_ptr, if needed
				body.push_str(&format!(
					"(call $__release_stack_bytes (i32.const {fn_stack_size}))\n"
				));

				if let Some(size) = type_.copy_size() {
					// FIXME: heap allocated string not copied, only the pointer
					body.push_str(&format!("(call $__mem_n_copy (get_local $__return_ptr) {ret} (i32.const {size}) \n)"));
					body.push_str("(return (get_local $__return_ptr)\n)")
				} else {
					body.push_str(&format!("(return {ret}\n)"))
				}
			}
			ASTBlockStatement::IfStmt(stmt) => {
				let mut inner_stmts = String::new();
				compile_block(
					ctx,
					function,
					&stmt.block,
					&mut inner_stmts,
					variables,
					fn_stack_size,
				);

				body.push_str(&format!(
					"(if {} (then {}))\n",
					stmt.conditional.compile(ctx),
					inner_stmts
				))
			}
		}
	}
}

fn compile_function(
	ctx: &mut CompileCtx,
	instructions: &mut String,
	fn_name: &str,
	function: &ASTFunction,
) {
	// pointer and variables are local to functions
	ctx.vars = function
		.body
		.variables
		.iter()
		.map(|v| CompiledVariable {
			type_: ctx.ast_type_into_compiled(&v.ast_type),
			ident: (&v.ident).try_into().unwrap(),
		})
		.collect();

	let mut variables = String::new();
	let mut body = String::new();

	instructions.push_str(&format!("(func ${}", fn_name));
	let fn_stack_size = ctx.vars.iter().fold(0, |acc, v| acc + v.type_.size());
	variables.push_str("(local $__stack_ptr i32)\n");
	body.push_str(&format!(
		"(set_local $__stack_ptr (call $__reserve_stack_bytes (i32.const {fn_stack_size})))\n"
	));

	if !function.args.is_empty() {
		instructions.push_str("(param");
		for (idx, arg) in function.args.iter().enumerate() {
			let arg_type = ctx.ast_type_into_compiled(&arg.ast_type);
			let ints = if arg_type.is32b() { "i32" } else { "i64" };
			instructions.push_str(&format!(" {ints}"));
			let arg_ident: String = (&arg.ident).try_into().unwrap();
			body.push_str(&format!("(set_local ${} (local.get {}))\n", arg_ident, idx));
			variables.push_str(&format!("(local ${} {ints})\n", arg_ident));
			ctx.vars.push(CompiledVariable {
				type_: arg_type,
				ident: arg_ident,
			})
		}

		if let Some(ret) = &function.returns {
			if ctx.ast_type_into_compiled(ret).copy_size().is_some() {
				body.push_str(&format!(
					"(set_local $__return_ptr (local.get {}))\n",
					function.args.len()
				));
				variables.push_str("(local $__return_ptr i32)\n");
				instructions.push_str(" i32")
			}
		}

		instructions.push(')');
	} else if let Some(ret) = &function.returns {
		if ctx.ast_type_into_compiled(ret).copy_size().is_some() {
			instructions.push_str("(param i32)");
			body.push_str(&format!(
				"(set_local $__return_ptr (local.get {}))\n",
				function.args.len()
			));
			variables.push_str("(local $__return_ptr i32)\n");
		}
	}

	// TODO: function returns type
	if let Some(ret) = &function.returns {
		if ctx.ast_type_into_compiled(ret).is32b() {
			instructions.push_str("(result i32)\n");
		} else {
			instructions.push_str("(result i64)\n");
		}
	} else {
		instructions.push('\n');
	}

	compile_block(
		ctx,
		&function,
		&function.body,
		&mut body,
		&mut variables,
		fn_stack_size,
	);

	// if function returns, stack is released just before return
	// otherwise it needs to be released at the end of the function
	if function.returns.is_none() {
		body.push_str(&format!(
			"(call $__release_stack_bytes (i32.const {fn_stack_size}))\n"
		));
	}

	instructions.push_str(&variables);
	instructions.push_str(&body);
	instructions.push(')');
	instructions.push('\n');
}

#[derive(Debug)]
pub struct Compiler {
	ast: Parser,
}

impl Compiler {
	pub fn new(ast: Parser) -> Self {
		Self { ast }
	}

	pub fn compile(&mut self) -> String {
		let mut instructions = String::new();
		let mut ctx = CompileCtx {
			vars: Vec::new(),
			enums: Vec::new(),
			classes: Vec::new(),
			static_data: Vec::new(),
		};

		for enum_ in &self.ast.enum_types {
			ctx.enums.push(CompiledEnum::new(enum_));
		}

		for class in &self.ast.class_types {
			if class.template.is_some() && class.tmpl_type.is_none() {
				continue;
			}
			ctx.add_ast_class(class);
			for method in &class.methods {
				let fn_name = if let Some(type_) = &class.tmpl_type {
					format!("__{}{}_class_{}", class.name, type_.name(), method.name)
				} else {
					format!("__{}_class_{}", class.name, method.name)
				};

				compile_function(&mut ctx, &mut instructions, &fn_name, method);
			}
		}

		for function in &self.ast.nodes {
			compile_function(&mut ctx, &mut instructions, &function.name, function)
		}

		let data: Vec<String> = ctx
			.static_data
			.iter()
			.map(|sd| format!("(data (i32.const {}) {})", sd.start(), sd.value()))
			.collect();
		let data = data.join("\n");

		let data_end = if let Some(end) = ctx.static_data.iter().last() {
			end.start() + end.size()
		} else {
			// Internal functions reserve 1024 * 1024 bytes so the "heap" has to start there
			1024 * 1024
		};

		format!(
			";; Autogenerated module, do not modify

		(module
			;; Internal functions
			;; TODO: only import required ones
			(import \"internals\" \"__print_char\" (func $__print_char (param i32)))
			(import \"internals\" \"__print_int\" (func $__print_int (param i32)))
			(import \"internals\" \"__print_int64\" (func $__print_int64 (param i64)))
			(import \"internals\" \"__init_memory\" (func $__init_memory (param i32)))
			(import \"internals\" \"__free_bytes\" (func $__free_bytes (param i32)))
			(import \"internals\" \"__allocate_bytes\" (func $__allocate_bytes (param i32) (result i32)))
			(import \"internals\" \"__reserve_stack_bytes\" (func $__reserve_stack_bytes (param i32) (result i32)))
			(import \"internals\" \"__release_stack_bytes\" (func $__release_stack_bytes (param i32)))
			(import \"internals\" \"__mem_n_copy\" (func $__mem_n_copy (param i32) (param i32) (param i32)))
			(import \"internals\" \"__string_concat\" (func $__string_concat (param i32) (param i32) (result i32)))
			(import \"internals\" \"__string_concat2\" (func $__string_concat2 (param i64) (param i64) (result i64)))
			(import \"internals\" \"__print_str\" (func $__print_str (param i64)))
			(import \"internals\" \"__print_str_ptr\" (func $__print_str_ptr (param i32)))
			(import \"internals\" \"__exit\" (func $__exit (param i32)))
			;; shared internal memory
			(import \"internals\" \"memory\" (memory 0))

			;; Generated memory
			{data}

			;; generated functions
			{instructions}

			;; We always have to export main
			(export \"main\" (func $main))

			(func $init
				(call $__init_memory (i32.const {data_end}))
			)
			(export \"init\" (func $init))
		)
			")
	}
}
