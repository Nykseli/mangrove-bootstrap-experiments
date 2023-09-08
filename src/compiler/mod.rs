use crate::ast::{ASTBlockStatement, ASTFunction, ASTFunctionCall, ASTFunctionCallArg};

impl ASTFunctionCallArg {}

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
			instructions.push_str(&format!("(func ${}\n", function.name));
			let mut variables = String::new();
			let mut body = String::new();
			for block in &function.body.statements {
				match block {
					ASTBlockStatement::Assignment(assign) => {
						variables.push_str(&format!("(local ${} i32)\n", assign.ident));
						body.push_str(&format!(
							"(set_local ${} (i32.const {}))\n",
							assign.ident, assign.value
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
				}
			}

			instructions.push_str(&variables);
			instructions.push_str(&body);
			instructions.push(')');
		}

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
			(import \"internals\" \"__print_str\" (func $__print_str (param i32) (param i32)))
			;; shared internal memory
			(import \"internals\" \"memory\" (memory 0))

			;; Generated memory
			{}

			;; generated functions
			{}

			;; We always have to export main
			(export \"main\" (func $main))
		)
			",
			data, instructions
		)
	}
}
