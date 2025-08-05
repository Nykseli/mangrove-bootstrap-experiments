use std::fmt::Display;

use crate::{
	ast::{
		ASTAssignArg, ASTAssignment, ASTAssignmentExpr, ASTBlockStatement, ASTFunction,
		ASTFunctionCallArg, ASTType, ASTVariable, StaticValue,
	},
	compiler::{
		common::Compiler,
		riscv::{instr::Instruction, internal::compile_internal_function_call, register::Register},
	},
	parser::parse::Parser,
};

mod instr;
mod internal;
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
enum CompiledType {
	/// Internal character type
	Char,
	/// Signed 32 bit integer
	Int32,
	/// Signed 64 bit integer
	Int64,
}

impl CompiledType {
	/// Size in bytes
	fn size(&self) -> u32 {
		match self {
			CompiledType::Char => 1,
			CompiledType::Int32 => 4,
			CompiledType::Int64 => 8,
		}
	}
}

impl TryFrom<&ASTType> for CompiledType {
	type Error = String;

	fn try_from(value: &ASTType) -> Result<Self, Self::Error> {
		match value {
			ASTType::Char => Ok(Self::Char),
			ASTType::Int64 => Ok(Self::Int64),
			ASTType::Int32(_) => Ok(Self::Int32),
			_ => Err(format!(
				"Riscv doesn't support compiling ASTType {:?}",
				value
			)),
		}
	}
}

#[derive(Debug, Clone)]
struct CompiledVariable {
	ident: String,
	type_: CompiledType,
	/// offset to stack
	offset: u32,
}

#[derive(Debug, Clone)]
struct CompileCtx {
	/// Statically allocated memory
	static_data: Vec<StaticData>,
	/// variables in the current block
	block_variables: Vec<CompiledVariable>,
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

	fn set_block_variables(&mut self, variables: &[ASTVariable]) -> Result<u32, String> {
		let mut vars = Vec::new();
		for var in variables {
			let type_: CompiledType = (&var.ast_type).try_into()?;
			let ident = var.ident.ident().into();
			vars.push(CompiledVariable {
				ident,
				type_,
				//
				offset: 0,
			});
		}

		let stack_size: u32 = vars.iter().fold(0, |acc, var| acc + var.type_.size());
		// stacksize needs to be dividable by 32 to keep the ABI consistent
		// TODO: link docs for that
		let stack_size = if stack_size < 32 {
			32
		} else if stack_size % 32 == 0 {
			stack_size
		} else {
			stack_size + (32 - (stack_size % 32))
		};

		// 8 is reserved since we always need to save the return address
		let mut stack_start = stack_size - 8;
		for var in &mut vars {
			stack_start -= var.type_.size();
			var.offset = stack_start;
		}

		self.block_variables = vars;
		Ok(stack_size)
	}

	fn compiled_variable<'a>(&'a mut self, var: &str) -> Result<&'a CompiledVariable, String> {
		self.block_variables
			.iter()
			.find(|bvar| bvar.ident.as_str() == var)
			.ok_or(format!("Variable not {:?} found in current scope", var))
	}
}

fn compile_ast_assignment(
	ctx: &mut CompileCtx,
	assignment: &ASTAssignment,
) -> Result<Vec<Instruction>, String> {
	let var = ctx.compiled_variable(assignment.variable.ident.ident())?;
	let mut isntrs = Vec::new();

	match &assignment.expr {
		ASTAssignmentExpr::Arg(astassign_arg) => match astassign_arg {
			ASTAssignArg::Static(aststatic_assign) => match aststatic_assign.value {
				StaticValue::Char(c) => {
					isntrs.push(Instruction::Addi {
						dst: Register::T5,
						src: Register::Zero,
						imm: c as i32,
					});
					isntrs.push(Instruction::Sb {
						src: Register::T5,
						base: Register::Sp,
						offset: var.offset as i32,
					});
				}
				StaticValue::Int32(val) => {
					isntrs.push(Instruction::Li {
						dest: Register::T5,
						value: val as u64,
					});
					isntrs.push(Instruction::Sw {
						src: Register::T5,
						base: Register::Sp,
						offset: var.offset as i32,
					});
				}
				StaticValue::Int64(val) => {
					isntrs.push(Instruction::Li {
						dest: Register::T5,
						value: val as u64,
					});
					isntrs.push(Instruction::Sd {
						src: Register::T5,
						base: Register::Sp,
						offset: var.offset as i32,
					});
				}
				_ => {
					return Err(format!(
						"Following StaticValue cannot be compiled\n{:#?}",
						assignment.expr
					))
				}
			},
			_ => {
				return Err(format!(
					"Following ASTAssignArg cannot be compiled\n{:#?}",
					assignment.expr
				))
			}
		},
		_ => {
			return Err(format!(
				"Following ASTAssignmentExpr cannot be compiled\n{:#?}",
				assignment.expr
			))
		}
	}

	Ok(isntrs)
}

fn compile_function(ctx: &mut CompileCtx, function: &ASTFunction) -> CompiledFn {
	let stack_size = ctx.set_block_variables(&function.body.variables).unwrap();
	let mut compiledfn = CompiledFn {
		stack_size,
		name: function.name.clone(),
		instructions: Vec::new(),
	};

	let mut body_isntr: Vec<Instruction> = Vec::new();
	for statement in &function.body.statements {
		match statement {
			// TODO: handle internal function special cases
			ASTBlockStatement::FunctionCall(astfunction_call) => {
				if astfunction_call.name.starts_with("__") {
					let isntrs = compile_internal_function_call(ctx, astfunction_call).unwrap();
					body_isntr.extend(isntrs);
					continue;
				}

				body_isntr.push(Instruction::Call(astfunction_call.name.clone()));
			}
			ASTBlockStatement::Assignment(assignment) => {
				let instrs = compile_ast_assignment(ctx, assignment).unwrap();
				body_isntr.extend(instrs);
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
		offset: (compiledfn.stack_size as i32) - 8,
	});

	compiledfn.instructions.extend(body_isntr);

	// load return addreess from stack
	compiledfn.instructions.push(Instruction::Ld {
		dest: Register::Ra,
		base: Register::Sp,
		offset: (compiledfn.stack_size as i32) - 8,
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
			block_variables: Vec::new(),
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
