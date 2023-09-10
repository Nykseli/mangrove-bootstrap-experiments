use crate::ast::{
	ASTAssignArg, ASTAssignmentExpr, ASTBlockStatement, ASTFunction, ASTFunctionCall,
	ASTFunctionCallArg, ASTLtStmt, StaticValue,
};

impl ASTAssignArg {
	fn compile(&self, global_data: &mut Vec<GlobalData>, vars: &mut Vec<Pointer>) -> String {
		match self {
			ASTAssignArg::Ident(s) => format!("(get_local ${})", s.ident),
			ASTAssignArg::Static(s) => match &s.value {
				StaticValue::Int32(val) => format!("(i32.const {val})"),
				StaticValue::String(val) => {
					let data_len = val.len();
					let ptr_idx = vars.len();
					let global =
						Compiler::add_data(global_data, ASTFunctionCallArg::String(val.into()));
					let global_addr = global.start;
					// call reserve bytes
					// save stackpointer to offset 0
					// save length of the string to offset 4
					let setup = format!(
						"
						(set_local $__pointer{ptr_idx} (call $__reserve_bytes (i32.const 8)))
						(i32.store (get_local $__pointer{ptr_idx}) (i32.const {global_addr}))
						(i32.store (i32.add (get_local $__pointer{ptr_idx}) (i32.const 4)) (i32.const {data_len}))
					"
					);
					vars.push(Pointer { size: 0, setup });
					format!("(get_local $__pointer{ptr_idx})")
				}
			},
		}
	}
}

impl ASTAssignmentExpr {
	fn compile(&self, global_data: &mut Vec<GlobalData>, vars: &mut Vec<Pointer>) -> String {
		match self {
			ASTAssignmentExpr::Arg(arg) => arg.compile(global_data, vars),
			ASTAssignmentExpr::Add(val) => {
				format!(
					"(i32.add {} {})",
					val.lhs.compile(global_data, vars),
					val.rhs.compile(global_data, vars)
				)
			}
			ASTAssignmentExpr::Minus(val) => {
				format!(
					"(i32.sub {} {})",
					val.lhs.compile(global_data, vars),
					val.rhs.compile(global_data, vars)
				)
			}
			ASTAssignmentExpr::FunctionCall(func) => {
				// There's no args at this point
				let mut arg_str = String::new();
				for arg in &func.args {
					let arg_fmt = match &arg {
						ASTFunctionCallArg::Char(v) => format!("(i32.const {})", *v as i32),
						ASTFunctionCallArg::Int32(v) => format!("(i32.const {v})"),
						ASTFunctionCallArg::String(s) => {
							/* format!("(i32.const {}) (i32.const {})", self.start, s.len()) */
							"".into()
						}
						ASTFunctionCallArg::Ident(i) => format!("(get_local ${i})"),
					};
					arg_str.push_str(&arg_fmt);
				}

				format!("(call ${} {})\n", func.name, arg_str)
			}
		}
	}
}

impl ASTLtStmt {
	fn compile(&self, global_data: &mut Vec<GlobalData>, vars: &mut Vec<Pointer>) -> String {
		format!(
			"(i32.lt_s {} {})",
			self.lhs.compile(global_data, vars),
			self.rhs.compile(global_data, vars)
		)
	}
}

#[derive(Debug, Clone)]
pub struct Pointer {
	size: i32,
	/// Allocation instructions
	setup: String,
}

#[derive(Debug, Clone)]
pub struct GlobalData {
	start: i32,
	data: ASTFunctionCallArg,
}

impl GlobalData {
	fn data_size(&self) -> usize {
		match &self.data {
			ASTFunctionCallArg::Char(_) => 4,
			ASTFunctionCallArg::Int32(_) => 4,
			ASTFunctionCallArg::Ident(_) => 4,
			ASTFunctionCallArg::String(s) => s.len(),
		}
	}

	fn is_global(&self) -> bool {
		match &self.data {
			ASTFunctionCallArg::String(s) => true,
			_ => false,
		}
	}

	fn function_arg(&self) -> String {
		match &self.data {
			ASTFunctionCallArg::Char(v) => format!("(i32.const {})", *v as i32),
			ASTFunctionCallArg::Int32(v) => format!("(i32.const {v})"),
			ASTFunctionCallArg::String(s) => {
				format!("(i32.const {}) (i32.const {})", self.start, s.len())
			}
			ASTFunctionCallArg::Ident(i) => format!("(get_local ${i})"),
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
		}
	}
}

#[derive(Debug)]
pub struct Compiler {
	ast: Vec<ASTFunction>,
	global_data: Vec<GlobalData>,
}

impl Compiler {
	pub fn new(ast: Vec<ASTFunction>) -> Self {
		Self {
			ast,
			global_data: Vec::new(),
		}
	}

	/// Add data and return the created item
	fn add_data(global_data: &mut Vec<GlobalData>, data: ASTFunctionCallArg) -> GlobalData {
		// first 4096 bytes are reserved for internal functions
		let start = if global_data.len() == 0 {
			4096
		} else {
			let last = global_data.last().unwrap();
			last.start + last.data_size() as i32
		};

		// TODO: properly replace literals
		// let data: String = data.replace("\n", "\\n");
		let new_data = GlobalData { start, data };

		global_data.push(new_data.clone());
		new_data
	}

	fn compile_args(global_data: &mut Vec<GlobalData>, args: &Vec<ASTFunctionCallArg>) -> String {
		let mut arg_str = String::new();
		for arg in args {
			let data = Self::add_data(global_data, arg.clone());
			arg_str.push_str(&data.function_arg())
		}

		arg_str
	}

	fn is_special_function(function: &ASTFunctionCall) -> bool {
		function.name == "__print_format"
	}

	fn compile_special_function(
		global_data: &mut Vec<GlobalData>,
		args: &Vec<ASTFunctionCallArg>,
	) -> String {
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
				let data = Self::add_data(global_data, ASTFunctionCallArg::String(str_buf));
				str_buf = String::new();
				function.push_str(&format!("(call $__print_str {})\n", data.function_arg()));

				// Print the argument
				let arg = &args[arg_idx];
				match arg {
					ASTFunctionCallArg::Char(c) => function
						.push_str(&format!("(call $__print_char (i32.const {}))\n", *c as i32)),
					ASTFunctionCallArg::Int32(val) => {
						function.push_str(&format!("(call $__print_int (i32.const {}))\n", val))
					}
					ASTFunctionCallArg::String(_) => {
						let data = Self::add_data(global_data, arg.clone());
						function.push_str(&format!("(call $__print_str {})\n", data.function_arg()))
					}
					ASTFunctionCallArg::Ident(ident) => {
						function.push_str(&format!("(call $__print_int (get_local ${ident}))\n"))
					}
				}

				format = &format[2..];
				arg_idx += 1;
			} else {
				str_buf.push_str(&format[0..1]);
				format = &format[1..];
			}
		}

		if !str_buf.is_empty() {
			let data = Self::add_data(global_data, ASTFunctionCallArg::String(str_buf));
			function.push_str(&format!("(call $__print_str {})\n", data.function_arg()));
		}

		function
	}

	pub fn compile(&mut self) -> String {
		let mut instructions = String::new();
		let mut global_data: Vec<GlobalData> = Vec::new();

		for function in &self.ast {
			let mut pointers: Vec<Pointer> = Vec::new();
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

			if function.args.len() > 0 {
				for (idx, name) in function.args.iter().enumerate() {
					body.push_str(&format!("(set_local ${} (local.get {}))\n", name, idx));
					variables.push_str(&format!("(local ${} i32)\n", name));
				}
			}

			for block in &function.body.statements {
				match block {
					ASTBlockStatement::Assignment(assign) => {
						variables.push_str(&format!("(local ${} i32)\n", assign.variable.ident));
						body.push_str(&format!(
							"(set_local ${} {})\n",
							assign.variable.ident,
							assign.expr.compile(&mut global_data, &mut pointers)
						));
					}
					ASTBlockStatement::FunctionCall(call) => {
						if Self::is_special_function(call) {
							let special =
								Self::compile_special_function(&mut global_data, &call.args);
							body.push_str(&special);
						} else {
							let args = Self::compile_args(&mut global_data, &call.args);
							body.push_str(&format!("(call ${} {})\n", call.name, args));
						}
					}
					ASTBlockStatement::Return(stmt) => body.push_str(&format!(
						"(return {})\n",
						stmt.expr.compile(&mut global_data, &mut pointers)
					)),
					ASTBlockStatement::IfStmt(stmt) => {
						let mut inner_stmts = String::new();
						for inner_block in &stmt.block.statements {
							match inner_block {
								ASTBlockStatement::Return(inner_stmt) => {
									inner_stmts.push_str(&format!(
										"(return {})\n",
										inner_stmt.expr.compile(&mut global_data, &mut pointers)
									))
								}
								_ => panic!("Only return in if blocks"),
							}
						}

						body.push_str(&format!(
							"(if {} (then {}))\n",
							stmt.conditional.compile(&mut global_data, &mut pointers),
							inner_stmts
						))
					}
				}
			}

			instructions.push_str(&variables);
			for (idx, _) in pointers.iter().enumerate() {
				instructions.push_str(&format!("(local $__pointer{} i32)\n", idx));
			}
			for pointer in &pointers {
				instructions.push_str(&pointer.setup);
			}
			instructions.push_str(&body);
			instructions.push(')');
		}

		let data_end = if let Some(end) = global_data.iter().last() {
			end.start + end.data_size() as i32
		} else {
			// Internal functions reserve 4096 bytes so the "heap" has to start there
			4096
		};
		let data: Vec<String> = global_data
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
