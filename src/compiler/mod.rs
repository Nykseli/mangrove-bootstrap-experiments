use crate::{
	ast::{
		ASTAssignArg, ASTAssignmentExpr, ASTBlockStatement, ASTClass, ASTFunction, ASTFunctionCall,
		ASTFunctionCallArg, ASTLtStmt, ASTType, StaticValue,
	},
	parser::parse::Parser,
};

struct CompileCtx {
	vars: Vec<Variable>,
	ptrs: Vec<Pointer>,
	types: Vec<CompiledType>,
	global_data: Vec<GlobalData>,
}

impl CompileCtx {
	fn dottet_ident_type<'a>(&'a self, dotted: (&str, &str)) -> &'a TypeInfo {
		// If the values are not compiled, it's probably a probem with AST
		let var = &self
			.vars
			.iter()
			.find(|v| v.ident == dotted.0)
			.unwrap()
			.type_;
		let dotted = var.members.iter().find(|m| m.ident == dotted.1).unwrap();
		&dotted.type_
	}
}

impl ASTAssignArg {
	/// [ident] is Some when the assignment is directly set to a variable
	/// ```grove
	/// String foo = "bar"
	/// //     ^ option will be "foo"
	/// ```
	/// It's None when the value is anonymous
	/// ```grove
	/// String foo = "bar" + "zoo"
	/// //           ^ "bar" and "zoo" need to be set to anonymous pointers
	/// ```
	fn compile(&self, ctx: &mut CompileCtx, ident: Option<&str>) -> String {
		match self {
			ASTAssignArg::Ident(s) => format!("(get_local ${})", s.ident),
			ASTAssignArg::Static(s) => match &s.value {
				StaticValue::Int32(val) => format!("(i32.const {val})"),
				StaticValue::String(val) => {
					let data_len = val.len();
					let ptr_idx = ctx.ptrs.len();
					let global = Compiler::add_data(ctx, ASTFunctionCallArg::String(val.into()));
					let global_addr = global.start;
					let target = if let Some(ident) = ident {
						ident.into()
					} else {
						format!("__pointer{ptr_idx}")
					};
					// call reserve bytes
					// save stackpointer to offset 0
					// save length of the string to offset 4
					let setup = format!(
						"
						(set_local ${target} (call $__reserve_bytes (i32.const 8)))
						(i32.store (get_local ${target}) (i32.const {global_addr}))
						(i32.store (i32.add (get_local ${target}) (i32.const 4)) (i32.const {data_len}))
					"
					);

					ctx.ptrs.push(Pointer {
						anonymous: ident.is_none(),
						setup,
					});
					format!("(get_local ${target})")
				}
			},
			ASTAssignArg::DottedIdent(dotted_ident) => {
				let (ident, dotted) = &dotted_ident.ident;
				let offset = ctx.dottet_ident_type((ident, dotted)).offset;
				// Dotted value is ident + offset to it
				format!("(i32.load (i32.add (get_local ${ident}) (i32.const {offset})))")
			}
		}
	}
}

impl ASTAssignmentExpr {
	///
	fn compile(&self, ctx: &mut CompileCtx, ident: Option<&str>) -> (bool, String) {
		match self {
			ASTAssignmentExpr::Arg(arg) => (true, arg.compile(ctx, ident)),
			ASTAssignmentExpr::Add(val) => {
				// Assuming lhs has same type as rhs
				let fn_type = match &val.lhs {
					ASTAssignArg::Static(val) => &val.value_type,
					ASTAssignArg::Ident(val) => &val.ident_type,
					ASTAssignArg::DottedIdent(val) => &val.ident_type,
				};

				let function = match fn_type {
					ASTType::Int32(_) => "i32.add",
					ASTType::String(_) => "call $__string_concat",
					ASTType::Custom(_) => unreachable!("Cannot add two custom types"),
				};

				(
					true,
					format!(
						"({function} {} {})",
						val.lhs.compile(ctx, None),
						val.rhs.compile(ctx, None)
					),
				)
			}
			ASTAssignmentExpr::Minus(val) => (
				true,
				format!(
					"(i32.sub {} {})",
					val.lhs.compile(ctx, None),
					val.rhs.compile(ctx, None)
				),
			),
			ASTAssignmentExpr::FunctionCall(func) => {
				// There's no args at this point
				let mut arg_str = String::new();
				for arg in &func.args {
					let arg_fmt = match &arg {
						ASTFunctionCallArg::Char(v) => format!("(i32.const {})", *v as i32),
						ASTFunctionCallArg::Int32(v) => format!("(i32.const {v})"),
						ASTFunctionCallArg::DottedIdent(s) => todo!(),
						ASTFunctionCallArg::String(s) => {
							/* format!("(i32.const {}) (i32.const {})", self.start, s.len()) */
							"".into()
						}
						ASTFunctionCallArg::Ident(i) => format!("(get_local ${i})"),
					};
					arg_str.push_str(&arg_fmt);
				}

				(true, format!("(call ${} {})\n", func.name, arg_str))
			}
			ASTAssignmentExpr::ClassInit(class) => {
				/// We need to collect the arguments first so we can
				/// set up pointers for static strings etc
				let mut assign_args = Vec::new();
				for arg in &class.args {
					// We cannot directly assign static values to structures
					// so always create anonymous pointer for it if needed
					assign_args.push(arg.arg.compile(ctx, None))
				}

				// TODO: support creating an anonymous pointer for return statements
				let target = ident.expect("ClassInit requires a variable target");
				// If type is not compiled, it should be a problem with AST
				let type_ = ctx.types.iter().find(|t| t.name == class.name).unwrap();
				// TODO: we shouldn't need to clone here
				let type_ = type_.clone();

				let data_size = type_.total_size();
				let ptr_idx = ctx.ptrs.len();
				let mut setup = format!(
					"\n(set_local ${target} (call $__reserve_bytes (i32.const {data_size})))\n"
				);

				for (idx, arg) in class.args.iter().enumerate() {
					// If type_info is not compiled, it should be a problem with AST
					let type_info = type_.members.iter().find(|m| m.ident == arg.ident).unwrap();
					let offset = type_info.type_.offset;
					// TODO: check if the return type should set_local
					//       this should be Arg or Add.
					let assign_arg = &assign_args[idx].1;
					setup.push_str(&format!(
						"(i32.store (i32.add (get_local ${target}) (i32.const {offset})) {assign_arg})\n",
					));
				}

				(false, setup)
			}
		}
	}
}

impl ASTLtStmt {
	fn compile(&self, ctx: &mut CompileCtx) -> String {
		format!(
			"(i32.lt_s {} {})",
			self.lhs.compile(ctx, None),
			self.rhs.compile(ctx, None)
		)
	}
}

#[derive(Debug, Clone)]
pub struct Pointer {
	anonymous: bool,
	/// Allocation instructions
	setup: String,
}

#[derive(Debug, Clone)]
pub struct Variable {
	ident: String,
	type_: CompiledType,
}

#[derive(Debug, Clone)]
pub struct GlobalData {
	start: i32,
	data: ASTFunctionCallArg,
}

impl GlobalData {
	fn data_size(&self, ctx: &CompileCtx) -> usize {
		match &self.data {
			ASTFunctionCallArg::Char(_) => 4,
			ASTFunctionCallArg::Int32(_) => 4,
			ASTFunctionCallArg::Ident(_) => 4,
			ASTFunctionCallArg::String(s) => s.len(),
			ASTFunctionCallArg::DottedIdent((ident, dotted)) => {
				ctx.dottet_ident_type((&ident, &dotted)).size as usize
			}
		}
	}

	fn is_global(&self) -> bool {
		match &self.data {
			ASTFunctionCallArg::String(s) => true,
			_ => false,
		}
	}

	fn function_arg(&self, ctx: &CompileCtx) -> String {
		match &self.data {
			ASTFunctionCallArg::Char(v) => format!("(i32.const {})", *v as i32),
			ASTFunctionCallArg::Int32(v) => format!("(i32.const {v})"),
			ASTFunctionCallArg::String(s) => {
				format!("(i32.const {}) (i32.const {})", self.start, s.len())
			}
			ASTFunctionCallArg::Ident(i) => format!("(get_local ${i})"),
			ASTFunctionCallArg::DottedIdent((ident, dotted)) => {
				let offset = ctx.dottet_ident_type((&ident, &dotted)).offset;
				// Dotted value is ident + offset to it
				format!("(i32.load (i32.add (get_local ${ident}) (i32.const {offset})))")
			}
		}
	}

	fn data_value(&self) -> String {
		match &self.data {
			ASTFunctionCallArg::Char(v) => format!("{}", *v as i32),
			ASTFunctionCallArg::Int32(v) => format!("{v}"),
			ASTFunctionCallArg::String(s) => {
				format!("\"{}\"", s.replace("\n", "\\n"))
			}
			ASTFunctionCallArg::Ident(i) => format!("(${i})"),
			ASTFunctionCallArg::DottedIdent(_) => todo!(),
		}
	}
}

#[derive(Debug, Clone)]
pub struct TypeInfo {
	/// How big member is
	size: i32,
	/// The offset from start of the member's memory area
	offset: i32,
}

#[derive(Debug, Clone)]
pub struct CompiledMember {
	ident: String,
	type_: TypeInfo,
}

#[derive(Debug, Clone)]
pub struct CompiledType {
	name: String,
	members: Vec<CompiledMember>,
}

impl CompiledType {
	fn from_classes(classes: &Vec<ASTClass>) -> Vec<Self> {
		// Start with internal types
		let mut types: Vec<CompiledType> = vec![CompiledType {
			name: "String".into(),
			members: vec![{
				CompiledMember {
					ident: "length".into(),
					type_: TypeInfo { size: 4, offset: 4 },
				}
			}],
		}];

		for class in classes {
			let mut offset: i32 = 0;
			let mut members = Vec::new();
			for member in &class.members {
				let ident = member.ident.clone();
				let size: i32 = match member.type_ {
					ASTType::Int32(_) => 4,
					// ptr and size
					ASTType::String(_) => 8,
					// ptr and size
					ASTType::Custom(_) => 8,
				};

				members.push(CompiledMember {
					ident,
					type_: TypeInfo { size, offset },
				});
				offset += size;
			}
			types.push(CompiledType {
				members,
				name: class.name.clone(),
			})
		}

		types
	}

	fn total_size(&self) -> i32 {
		self.members.iter().fold(0, |acc, x| acc + x.type_.size)
	}
}

#[derive(Debug)]
pub struct Compiler {
	ast: Parser,
}

impl Compiler {
	pub fn new(ast: Parser) -> Self {
		Self { ast }
	}

	/// Add data and return the created item
	fn add_data(ctx: &mut CompileCtx, data: ASTFunctionCallArg) -> GlobalData {
		// first 4096 bytes are reserved for internal functions
		let start = if ctx.global_data.len() == 0 {
			4096
		} else {
			let last = ctx.global_data.last().unwrap();
			last.start + last.data_size(ctx) as i32
		};

		let new_data = GlobalData { start, data };

		ctx.global_data.push(new_data.clone());
		new_data
	}

	fn compile_args(ctx: &mut CompileCtx, args: &Vec<ASTFunctionCallArg>) -> String {
		let mut arg_str = String::new();
		for arg in args {
			let data = Self::add_data(ctx, arg.clone());
			arg_str.push_str(&data.function_arg(&ctx))
		}

		arg_str
	}

	fn is_special_function(function: &ASTFunctionCall) -> bool {
		function.name == "__print_format"
	}

	fn compile_special_function(ctx: &mut CompileCtx, args: &Vec<ASTFunctionCallArg>) -> String {
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
				// Print the current string
				let data = Self::add_data(ctx, ASTFunctionCallArg::String(str_buf));
				str_buf = String::new();
				function.push_str(&format!("(call $__print_str {})\n", data.function_arg(ctx)));

				// Print the argument
				let arg = &args[arg_idx];
				match arg {
					ASTFunctionCallArg::Char(c) => function
						.push_str(&format!("(call $__print_char (i32.const {}))\n", *c as i32)),
					ASTFunctionCallArg::Int32(val) => {
						function.push_str(&format!("(call $__print_int (i32.const {}))\n", val))
					}
					ASTFunctionCallArg::String(_) => {
						let data = Self::add_data(ctx, arg.clone());
						function
							.push_str(&format!("(call $__print_str {})\n", data.function_arg(ctx)))
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
			let data = Self::add_data(ctx, ASTFunctionCallArg::String(str_buf));
			function.push_str(&format!("(call $__print_str {})\n", data.function_arg(ctx)));
		}

		function
	}

	fn add_ctx_variable(ctx: &mut CompileCtx, ident: &str, type_: &ASTType) {
		match &type_ {
			ASTType::Int32(_) => (),
			ASTType::String(_) => {
				let type_ = ctx.types.iter().find(|t| t.name == "String").unwrap();
				ctx.vars.push(Variable {
					ident: ident.into(),
					type_: type_.clone(),
				})
			}
			ASTType::Custom(class) => {
				let type_ = ctx.types.iter().find(|t| t.name == class.name).unwrap();
				ctx.vars.push(Variable {
					ident: ident.into(),
					type_: type_.clone(),
				})
			}
		}
	}

	pub fn compile(&mut self) -> String {
		let mut instructions = String::new();
		let types = CompiledType::from_classes(&self.ast.custom_types);
		let mut ctx = CompileCtx {
			types,
			global_data: Vec::new(),
			ptrs: Vec::new(),
			vars: Vec::new(),
		};

		for function in &self.ast.nodes {
			// pointer and variables are local to functions
			ctx.ptrs = Vec::new();
			ctx.vars = Vec::new();

			let mut variables = String::new();
			let mut body = String::new();

			instructions.push_str(&format!("(func ${}", function.name));
			if function.args.len() > 0 {
				instructions.push_str("(param");
				for _ in &function.args {
					instructions.push_str(" i32");
				}
				instructions.push(')');
			}

			if function.returns {
				instructions.push_str("(result i32)\n");
			} else {
				instructions.push('\n');
			}

			for (idx, var) in function.args.iter().enumerate() {
				body.push_str(&format!("(set_local ${} (local.get {}))\n", var.ident, idx));
				variables.push_str(&format!("(local ${} i32)\n", var.ident));
				Self::add_ctx_variable(&mut ctx, &var.ident, &var.ast_type);
			}

			for block in &function.body.statements {
				match block {
					ASTBlockStatement::Assignment(assign) => {
						if !assign.reassignment {
							variables
								.push_str(&format!("(local ${} i32)\n", assign.variable.ident));
						}
						let (set_local, compiled) =
							assign.expr.compile(&mut ctx, Some(&assign.variable.ident));
						if set_local {
							body.push_str(&format!(
								"(set_local ${} {})\n",
								assign.variable.ident, compiled
							));
						} else {
							body.push_str(&compiled);
						}

						Self::add_ctx_variable(
							&mut ctx,
							&assign.variable.ident,
							&assign.variable.ast_type,
						);
					}
					ASTBlockStatement::FunctionCall(call) => {
						if Self::is_special_function(call) {
							let special = Self::compile_special_function(&mut ctx, &call.args);
							body.push_str(&special);
						} else {
							let args = Self::compile_args(&mut ctx, &call.args);
							body.push_str(&format!("(call ${} {})\n", call.name, args));
						}
					}
					ASTBlockStatement::Return(stmt) => body.push_str(&format!(
						"(return {})\n",
						stmt.expr.compile(&mut ctx, None).1
					)),
					ASTBlockStatement::IfStmt(stmt) => {
						let mut inner_stmts = String::new();
						for inner_block in &stmt.block.statements {
							match inner_block {
								ASTBlockStatement::Return(inner_stmt) => {
									inner_stmts.push_str(&format!(
										"(return {})\n",
										inner_stmt.expr.compile(&mut ctx, None).1
									))
								}
								_ => panic!("Only return in if blocks"),
							}
						}

						body.push_str(&format!(
							"(if {} (then {}))\n",
							stmt.conditional.compile(&mut ctx),
							inner_stmts
						))
					}
				}
			}

			instructions.push_str(&variables);
			for (idx, ptr) in ctx.ptrs.iter().enumerate() {
				if ptr.anonymous {
					instructions.push_str(&format!("(local $__pointer{} i32)\n", idx));
				}
			}
			for pointer in &ctx.ptrs {
				instructions.push_str(&pointer.setup);
			}
			instructions.push_str(&body);
			instructions.push(')');
		}

		let data_end = if let Some(end) = ctx.global_data.iter().last() {
			end.start + end.data_size(&ctx) as i32
		} else {
			// Internal functions reserve 4096 bytes so the "heap" has to start there
			4096
		};
		let data: Vec<String> = ctx
			.global_data
			.iter()
			.filter(|gd| gd.is_global())
			.map(|gd| format!("(data (i32.const {}) {})", gd.start, gd.data_value()))
			.collect();
		let data = data.join("\n");

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
			(import \"internals\" \"__print_str\" (func $__print_str (param i32) (param i32)))
			(import \"internals\" \"__print_str_ptr\" (func $__print_str_ptr (param i32)))
			;; shared internal memory
			(import \"internals\" \"memory\" (memory 0))

			;; Generated memory
			{}

			;; generated functions
			{}

			;; We always have to export main
			(export \"main\" (func $main))

			(func $init
				(call $__init_memory (i32.const {data_end}))
			)
			(export \"init\" (func $init))
		)
			",
			data, instructions
		)
	}
}
