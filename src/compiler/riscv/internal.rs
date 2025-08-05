use crate::{
	ast::{ASTFunctionCall, ASTFunctionCallArg},
	compiler::riscv::{instr::Instruction, register::Register, CompileCtx, CompiledType},
};

const INTERNAL_FUNCTIONS: &'static [(
	&'static str,
	fn(&mut CompileCtx, &ASTFunctionCall) -> Result<Vec<Instruction>, String>,
)] = &[
	("__print_int", compile_print_int),
	("__print_int64", compile_print_int64),
	("__print_str", compile_print_str),
	("__print_char", compile_print_char),
	("__print_format", compile_print_format),
];

fn compile_print_format(
	ctx: &mut CompileCtx,
	function_call: &ASTFunctionCall,
) -> Result<Vec<Instruction>, String> {
	if function_call.args.is_empty() {
		return Err("__print_format requires at least 1 argument".into());
	}

	let format_arg = if let ASTFunctionCallArg::String(s) = &function_call.args[0] {
		s
	} else {
		return Err("__print_format's first argument needs to be a static string".into());
	};

	let mut fn_instrs = Vec::new();
	let mut format = &format_arg[..];
	let mut arg_idx: usize = 1;
	let mut str_buf = String::new();

	while !format.is_empty() {
		if format.starts_with("{}") {
			// print the current string
			let data_label = ctx.add_static_string(&str_buf);
			let instr1 = Instruction::La {
				dest: Register::A0,
				label: data_label,
			};
			fn_instrs.push(instr1);
			let instr2 = Instruction::Addi {
				dst: Register::A1,
				src: Register::Zero,
				imm: str_buf.len() as i32,
			};
			fn_instrs.push(instr2);
			fn_instrs.push(Instruction::Call("__print_str".into()));
			str_buf = String::new();

			// print the arg
			let arg = &function_call.args[arg_idx];
			match arg {
				ASTFunctionCallArg::Int32(int) => {
					let instr1 = Instruction::Addi {
						dst: Register::A0,
						src: Register::Zero,
						imm: *int,
					};
					fn_instrs.push(instr1);
					fn_instrs.push(Instruction::Call("__print_int".into()));
				}
				ASTFunctionCallArg::Char(ch) => {
					let instr1 = Instruction::Addi {
						dst: Register::A0,
						src: Register::Zero,
						imm: *ch as i32,
					};
					fn_instrs.push(instr1);
					fn_instrs.push(Instruction::Call("__print_char".into()));
				}
				ASTFunctionCallArg::String(string) => {
					let data_label = ctx.add_static_string(string);
					let instr1 = Instruction::La {
						dest: Register::A0,
						label: data_label,
					};
					fn_instrs.push(instr1);
					let instr2 = Instruction::Addi {
						dst: Register::A1,
						src: Register::Zero,
						imm: string.len() as i32,
					};
					fn_instrs.push(instr2);
					fn_instrs.push(Instruction::Call("__print_str".into()));
				}
				_ => {
					return Err(format!(
						"Internal function '{}' doesn't accept argument {:#?}",
						function_call.name, arg
					))
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
		let data_label = ctx.add_static_string(&str_buf);
		let instr1 = Instruction::La {
			dest: Register::A0,
			label: data_label,
		};
		fn_instrs.push(instr1);
		let instr2 = Instruction::Addi {
			dst: Register::A1,
			src: Register::Zero,
			imm: str_buf.len() as i32,
		};
		fn_instrs.push(instr2);
		fn_instrs.push(Instruction::Call("__print_str".into()));
	}

	Ok(fn_instrs)
}

fn compile_print_int(
	ctx: &mut CompileCtx,
	function_call: &ASTFunctionCall,
) -> Result<Vec<Instruction>, String> {
	if function_call.args.len() != 1 {
		return Err(format!(
			"Internal function '{}' expects exactly one argument.",
			function_call.name
		));
	}

	match &function_call.args[0] {
		ASTFunctionCallArg::Int32(int) => {
			let mut fn_instrs = Vec::new();
			let instr1 = Instruction::Addi {
				dst: Register::A0,
				src: Register::Zero,
				imm: *int,
			};
			fn_instrs.push(instr1);
			fn_instrs.push(Instruction::Call(function_call.name.clone()));
			Ok(fn_instrs)
		}
		ASTFunctionCallArg::Ident(idnt) => {
			let var = ctx.compiled_variable(&idnt)?;
			if !matches!(var.type_, CompiledType::Int32) {
				return Err(format!("Ident '{idnt}' is not a type of Int32"));
			}

			let mut fn_instrs = Vec::new();
			let instr1 = Instruction::Lw {
				dest: Register::A0,
				base: Register::Sp,
				offset: var.offset as i32,
			};
			fn_instrs.push(instr1);
			fn_instrs.push(Instruction::Call(function_call.name.clone()));
			Ok(fn_instrs)
		}
		_ => {
			return Err(format!(
				"Internal function '{}' only accepts static int32 as an argument.",
				function_call.name
			))
		}
	}
}

fn compile_print_int64(
	ctx: &mut CompileCtx,
	function_call: &ASTFunctionCall,
) -> Result<Vec<Instruction>, String> {
	if function_call.args.len() != 1 {
		return Err(format!(
			"Internal function '{}' expects exactly one argument.",
			function_call.name
		));
	}

	match &function_call.args[0] {
		ASTFunctionCallArg::Int64(int) => {
			let mut fn_instrs = Vec::new();
			let instr1 = Instruction::Addi {
				dst: Register::A0,
				src: Register::Zero,
				imm: (*int) as i32,
			};
			fn_instrs.push(instr1);
			fn_instrs.push(Instruction::Call(function_call.name.clone()));
			Ok(fn_instrs)
		}
		ASTFunctionCallArg::Ident(idnt) => {
			let var = ctx.compiled_variable(&idnt)?;
			if !matches!(var.type_, CompiledType::Int64) {
				return Err(format!("Ident '{idnt}' is not a type of Int64"));
			}

			let mut fn_instrs = Vec::new();
			let instr1 = Instruction::Ld {
				dest: Register::A0,
				base: Register::Sp,
				offset: var.offset as i32,
			};
			fn_instrs.push(instr1);
			fn_instrs.push(Instruction::Call(function_call.name.clone()));
			Ok(fn_instrs)
		}
		_ => {
			return Err(format!(
				"Internal function '{}' only accepts static Int64 as an argument.",
				function_call.name
			))
		}
	}
}

fn compile_print_str(
	ctx: &mut CompileCtx,
	function_call: &ASTFunctionCall,
) -> Result<Vec<Instruction>, String> {
	if function_call.args.len() != 1 {
		return Err(format!(
			"Internal function '{}' expects exactly one argument.",
			function_call.name
		));
	}

	match &function_call.args[0] {
		ASTFunctionCallArg::String(string) => {
			let mut fn_instrs = Vec::new();
			let data_label = ctx.add_static_string(string);
			let instr1 = Instruction::La {
				dest: Register::A0,
				label: data_label,
			};
			fn_instrs.push(instr1);
			let instr2 = Instruction::Addi {
				dst: Register::A1,
				src: Register::Zero,
				imm: string.len() as i32,
			};
			fn_instrs.push(instr2);
			fn_instrs.push(Instruction::Call(function_call.name.clone()));
			Ok(fn_instrs)
		}
		_ => {
			return Err(format!(
				"Internal function '{}' only accepts static strings as an argument.",
				function_call.name
			))
		}
	}
}

fn compile_print_char(
	ctx: &mut CompileCtx,
	function_call: &ASTFunctionCall,
) -> Result<Vec<Instruction>, String> {
	if function_call.args.len() != 1 {
		return Err(format!(
			"Internal function '{}' expects exactly one argument.",
			function_call.name
		));
	}

	match &function_call.args[0] {
		ASTFunctionCallArg::Char(ch) => {
			let mut fn_instrs = Vec::new();
			let instr1 = Instruction::Addi {
				dst: Register::A0,
				src: Register::Zero,
				imm: *ch as i32,
			};
			fn_instrs.push(instr1);
			fn_instrs.push(Instruction::Call(function_call.name.clone()));
			Ok(fn_instrs)
		}
		ASTFunctionCallArg::Ident(idnt) => {
			let var = ctx.compiled_variable(&idnt)?;
			if !matches!(var.type_, CompiledType::Char) {
				return Err(format!("Ident '{idnt}' is not a type of Char"));
			}

			let mut fn_instrs = Vec::new();
			let instr1 = Instruction::Lb {
				dest: Register::A0,
				base: Register::Sp,
				offset: var.offset as i32,
			};
			fn_instrs.push(instr1);
			fn_instrs.push(Instruction::Call(function_call.name.clone()));
			Ok(fn_instrs)
		}
		_ => {
			return Err(format!(
				"Internal function '{}' only accepts static char as an argument.",
				function_call.name
			))
		}
	}
}

pub fn compile_internal_function_call(
	ctx: &mut CompileCtx,
	function_call: &ASTFunctionCall,
) -> Result<Vec<Instruction>, String> {
	let func = INTERNAL_FUNCTIONS
		.iter()
		.find(|(name, _)| name == &function_call.name)
		.ok_or(format!(
			"Internal function '{}' not found",
			function_call.name
		))?;

	(func.1)(ctx, function_call)
}
