use crate::{
	ast::{ASTFunctionCall, ASTFunctionCallArg},
	compiler::riscv::{instr::Instruction, register::Register, CompileCtx},
};

const INTERNAL_FUNCTIONS: &'static [(
	&'static str,
	fn(&mut CompileCtx, &ASTFunctionCall) -> Result<Vec<Instruction>, String>,
)] = &[
	("__print_str", compile_print_str),
	("__print_char", compile_print_char),
];

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
	_ctx: &mut CompileCtx,
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
				dst: Register::A1,
				src: Register::Zero,
				imm: *ch as i32,
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
