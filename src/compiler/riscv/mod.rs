use std::fmt::Display;

use crate::{
	ast::{ASTBlockStatement, ASTFunction, ASTFunctionCallArg},
	compiler::{
		common::Compiler,
		riscv::{instr::Instruction, register::Register},
	},
	parser::parse::Parser,
};

mod instr;
mod register;

#[derive(Debug)]
struct CompiledFn {
	name: String,
	stack_size: u32,
	instructions: Vec<Instruction>,
}

impl Display for CompiledFn {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		writeln!(f, ".global {}", self.name)?;
		writeln!(f, "{}:", self.name)?;
		for instr in &self.instructions {
			writeln!(f, "\t{instr}")?;
		}
		Ok(())
	}
}

#[derive(Debug, Clone)]
enum StaticData {
	StaticString { label: String, data: String },
}

#[derive(Debug, Clone)]
struct CompileCtx {
	/// Statically allocated memory
	static_data: Vec<StaticData>,
}

impl CompileCtx {
	fn add_static_string(&mut self, string: &str) -> String {
		let label = format!(".D{}", self.static_data.len());
		let new_string = StaticData::StaticString {
			label: label.clone(),
			data: string.into(),
		};

		self.static_data.push(new_string);
		label
	}
}

fn compile_function(ctx: &mut CompileCtx, function: &ASTFunction) -> CompiledFn {
	let mut compiledfn = CompiledFn {
		// TODO: dynamicall calculate stack size
		stack_size: 32,
		name: function.name.clone(),
		instructions: Vec::new(),
	};

	let mut body_isntr: Vec<Instruction> = Vec::new();
	for statement in &function.body.statements {
		match statement {
			// TODO: handle internal function special cases
			ASTBlockStatement::FunctionCall(astfunction_call) => {
				for val in &astfunction_call.args {
					match val {
						ASTFunctionCallArg::String(string) => {
							let data_label = ctx.add_static_string(string);
							let instr1 = Instruction::La {
								dest: Register::A0,
								label: data_label,
							};
							body_isntr.push(instr1);
							let instr2 = Instruction::Addi {
								dst: Register::A1,
								src: Register::Zero,
								imm: string.len() as i32,
							};
							body_isntr.push(instr2);
						}
						_ => todo!("TODO: function argument\n{:#?}", val),
					};
				}
				let call = Instruction::Call(astfunction_call.name.clone());
				body_isntr.push(call);
			}
			_ => todo!("TODO: function statement\n{:#?}", statement),
		}
	}

	// allocate space from stack
	compiledfn.instructions.push(Instruction::Addi {
		dst: Register::Sp,
		src: Register::Sp,
		imm: -(compiledfn.stack_size as i32),
	});
	// save return address to stack
	// TODO: what about frame pointer?
	compiledfn.instructions.push(Instruction::Sd {
		src: Register::Ra,
		base: Register::Sp,
		offset: 24,
	});

	compiledfn.instructions.extend(body_isntr);

	// load return addreess from stack
	compiledfn.instructions.push(Instruction::Ld {
		dest: Register::Ra,
		base: Register::Sp,
		offset: 24,
	});
	// restore stack pointer
	compiledfn.instructions.push(Instruction::Addi {
		dst: Register::Sp,
		src: Register::Sp,
		imm: compiledfn.stack_size as i32,
	});
	compiledfn.instructions.push(Instruction::Ret);

	compiledfn
}

#[derive(Debug)]
pub struct RiscVCompiler {
	ast: Parser,
}

impl Compiler for RiscVCompiler {
	fn new(ast: Parser) -> Self {
		Self { ast }
	}

	fn compile(&mut self) -> String {
		let mut instructions = Vec::new();

		let mut ctx = CompileCtx {
			static_data: Vec::new(),
		};

		for function in &self.ast.nodes {
			instructions.push(compile_function(&mut ctx, function));
		}

		let output: Vec<String> = instructions
			.iter()
			.map(|instr| format!("{instr}"))
			.collect();
		let mut output = output.join("\n");

		let data: Vec<String> = ctx
			.static_data
			.iter()
			.map(|value| match value {
				StaticData::StaticString { label, data } => {
					format!("{label}: .ascii \"{}\"", data.replace('\n', "\\n"))
				}
			})
			.collect();
		let data = data.join("\n");

		output.push('\n');
		output.push_str(&data);
		output
	}
}
