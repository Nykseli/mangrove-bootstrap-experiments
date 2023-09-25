use crate::{
	ast::{
		ASTArrayAccess, ASTArrayInit, ASTAssignArg, ASTAssignmentExpr, ASTBlockStatement, ASTClass,
		ASTClassInit, ASTFunction, ASTFunctionCallArg, ASTIdent, ASTLtStmt, ASTType, StaticValue,
	},
	parser::parse::Parser,
};

#[derive(Debug, Clone)]
enum InternalType {
	String,
	Int32,
}

impl InternalType {
	fn size(&self) -> i32 {
		match self {
			// ptr + size
			InternalType::String => 8,
			// i32 is always 4 bytes
			InternalType::Int32 => 4,
		}
	}

	fn is32b(&self) -> bool {
		match self {
			// string is 2x32b values
			InternalType::String => false,
			InternalType::Int32 => true,
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
enum CompiledType {
	Array(CompiledArray),
	Class(CompiledClass),
	Internal(InternalType),
}

impl CompiledType {
	fn size(&self) -> i32 {
		match self {
			CompiledType::Array(arr) => arr.total_size,
			CompiledType::Class(class) => class.total_size,
			CompiledType::Internal(intr) => intr.size(),
		}
	}

	fn is32b(&self) -> bool {
		match &self {
			// Array and class is pointer and address are i32
			CompiledType::Array(_) | CompiledType::Class(_) => true,
			CompiledType::Internal(intr) => intr.is32b(),
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
	/// Statically allocated memory
	static_data: Vec<StaticData>,
}

impl CompileCtx {
	fn find_compiled_class<'a>(&'a self, name: &str) -> &'a CompiledClass {
		self.classes
			.iter()
			.find(|c| c.name == name)
			.unwrap_or_else(|| panic!("Class '{name}' is not defined"))
	}

	fn find_variable<'a>(&'a self, name: &str) -> &'a CompiledVariable {
		self.vars
			.iter()
			.find(|v| v.ident == name)
			.unwrap_or_else(|| panic!("Variable '{name}' is not defined"))
	}

	fn ast_type_into_compiled(&self, ast_type: &ASTType) -> CompiledType {
		match ast_type {
			ASTType::Int32(_) => CompiledType::Internal(InternalType::Int32),
			ASTType::String(_) => CompiledType::Internal(InternalType::String),
			ASTType::Custom(class) => {
				let class = self.find_compiled_class(&class.name).clone();
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
		}
	}

	fn add_ast_class(&mut self, class: &ASTClass) {
		let mut offset: i32 = 0;
		let mut members: Vec<CompiledClassMember> = Vec::new();
		for member in &class.members {
			let type_ = self.ast_type_into_compiled(&member.type_);
			let size = type_.size();
			members.push(CompiledClassMember {
				ident: member.ident.clone(),
				type_,
				offset,
			});
			offset += size;
		}

		self.classes.push(CompiledClass {
			name: class.name.clone(),
			members,
			total_size: offset,
		})
	}

	fn add_static_string(&mut self, string: &str) -> StaticString {
		let start = if let Some(stat) = self.static_data.last() {
			// Next start should be right after the previous memory allocation
			stat.size() + stat.start()
		} else {
			// first 4096 bytes are reserved for internal functions and data
			4096
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
			},
			CompiledType::Class(class) => {
				let member = class.member(dotted);
				let offset = offset + member.offset;
				if member.type_.is32b() {
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
				StaticValue::String(string) => {
					let sstring = ctx.add_static_string(string);
					let value = sstring.value();
					InitExpression::new_64b(offset, format!("(i64.const {})", value))
				}
			},

			ASTAssignArg::Ident(ident) => {
				InitExpression::new(offset, format!("(get_local ${})", ident.ident))
			}
			ASTAssignArg::DottedIdent(ident) => {
				ctx.load_dotted_ident((&ident.ident.0, &ident.ident.1), 0)
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
				};

				let function = match fn_type {
					ASTType::Int32(_) => "i32.add",
					ASTType::String(_) => "call $__string_concat2",
					ASTType::Custom(_) => unreachable!("Cannot add two custom types"),
					ASTType::Array(_) => unreachable!("Cannot add two array types"),
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
				let mut arg_str = String::new();
				for arg in &func.args {
					arg_str.push_str(&arg.compile(ctx));
				}
				let expr = format!("(call ${} {arg_str})\n", func.name);
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
			} else if let ASTAssignmentExpr::FunctionCall(_arg) = expr {
				let expr = expr.compile(ctx, &target.type_, 0);
				assert!(
					expr.len() == 1,
					"Array init funciton call can only be one expression"
				);
				let expr = &expr[0].expr;
				return format!("(set_local ${} {expr})", target.ident);
			} else {
				panic!("Expected Array init expression");
			};

			let mut assignment = format!(
				"(set_local ${} (call $__reserve_bytes (i32.const {})))\n",
				target.ident,
				target.type_.size()
			);

			let inits = expr.compile(ctx, array, 0);
			for init in inits {
				assignment.push_str(&format!(
					"({} (i32.add (get_local ${}) (i32.const {})) {})\n",
					init.store(),
					target.ident,
					init.offset,
					init.expr
				))
			}
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
				return arg.compile(ctx, &target.type_, 0).expr;
			} else if let ASTAssignmentExpr::FunctionCall(_arg) = expr {
				let expr = expr.compile(ctx, &target.type_, 0);
				assert!(
					expr.len() == 1,
					"Class init funciton call can only be one expression"
				);
				let expr = &expr[0].expr;
				return format!("(set_local ${} {expr})", target.ident);
			} else {
				panic!("Expected Class init expression {expr:#?}");
			};

			let mut assignment = format!(
				"(set_local ${} (call $__reserve_bytes (i32.const {})))\n",
				target.ident,
				target.type_.size()
			);

			let inits = expr.compile(ctx, class, 0);
			for init in inits {
				assignment.push_str(&format!(
					"({} (i32.add (get_local ${}) (i32.const {})) {})\n",
					init.store(),
					target.ident,
					init.offset,
					init.expr
				))
			}
			assignment
		}
		CompiledType::Internal(_internal) => {
			let expr = expr.compile(ctx, &target.type_, 0);
			assert!(expr.len() == 1, "Internal assign expr len needs to be 1");
			let expr = &expr[0].expr;
			format!("(set_local ${} {expr})\n", target.ident)
		}
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

fn compile_function(
	ctx: &mut CompileCtx,
	instructions: &mut String,
	fn_name: &str,
	function: &ASTFunction,
) {
	// pointer and variables are local to functions
	ctx.vars = Vec::new();

	let mut variables = String::new();
	let mut body = String::new();

	instructions.push_str(&format!("(func ${}", fn_name));
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
		instructions.push(')');
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

	for statment in &function.body.statements {
		match statment {
			ASTBlockStatement::Assignment(assign) => {
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
					let expr = compile_variable_assignment(ctx, &var, &assign.expr);
					body.push_str(&format!("{store} {expr})"));
					continue;
				}

				let type_ = ctx.ast_type_into_compiled(&assign.variable.ast_type);
				let var = CompiledVariable {
					type_: type_.clone(),
					ident: (&assign.variable.ident)
						.try_into()
						.unwrap_or_else(|_| panic!("{:#?}", assign.variable)),
				};
				body.push_str(&compile_variable_assignment(ctx, &var, &assign.expr));
				if !assign.reassignment {
					variables.push_str(&format!("(local ${} {})\n", var.ident, var.local_type()));
					ctx.vars.push(var);
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

					format!("__{}_class_{} {local}", var.ast_type.name(), call.name,)
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

				body.push_str(&format!("(return {ret}\n)"))
			}
			ASTBlockStatement::IfStmt(stmt) => {
				let mut inner_stmts = String::new();
				for inner_block in &stmt.block.statements {
					match inner_block {
						ASTBlockStatement::Return(stmt) => {
							let ret = stmt.expr.compile(
								ctx,
								&CompiledType::Internal(InternalType::Int32),
								0,
							);
							assert!(ret.len() == 1, "Return expression len needs to be 1");
							let ret = &ret[0].expr;
							inner_stmts.push_str(&format!("(return {ret}\n)"))
						}
						_ => panic!("Only return in if blocks"),
					}
				}

				body.push_str(&format!(
					"(if {} (then {}))\n",
					stmt.conditional.compile(ctx),
					inner_stmts
				))
			}
		}
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
			classes: Vec::new(),
			static_data: Vec::new(),
		};

		for class in &self.ast.custom_types {
			ctx.add_ast_class(class);
			for method in &class.methods {
				let fn_name = format!("__{}_class_{}", class.name, method.name);
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
			// Internal functions reserve 4096 bytes so the "heap" has to start there
			4096
		};

		format!(
			";; Autogenerated module, do not modify

		(module
			;; Internal functions
			;; TODO: only import required ones
			(import \"internals\" \"__print_char\" (func $__print_char (param i32)))
			(import \"internals\" \"__print_int\" (func $__print_int (param i32)))
			(import \"internals\" \"__init_memory\" (func $__init_memory (param i32)))
			(import \"internals\" \"__reserve_bytes\" (func $__reserve_bytes (param i32) (result i32)))
			(import \"internals\" \"__string_concat\" (func $__string_concat (param i32) (param i32) (result i32)))
			(import \"internals\" \"__string_concat2\" (func $__string_concat2 (param i64) (param i64) (result i64)))
			(import \"internals\" \"__print_str\" (func $__print_str (param i64)))
			(import \"internals\" \"__print_str_ptr\" (func $__print_str_ptr (param i32)))
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
