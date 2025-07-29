use crate::{
	ast::{ASTBlockStatement, ASTFunction, ASTFunctionCallArg},
	compiler::common::Compiler,
	parser::parse::Parser,
};

#[derive(Debug, Clone)]
enum StaticData {
	StaticString(String),
}

#[derive(Debug, Clone)]
struct CompileCtx {
	/// Statically allocated memory
	static_data: Vec<StaticData>,
}

impl CompileCtx {
	fn add_static_string(&mut self, string: &str) {
		let new_string = StaticData::StaticString(string.into());
		self.static_data.push(new_string);
	}
}

fn compile_function(ctx: &mut CompileCtx, instructions: &mut String, function: &ASTFunction) {
	instructions.push_str(&format!(".global {}\n", function.name));
	instructions.push_str(&format!("{}:\n", function.name));
	// allocate space from stack
	instructions.push_str("\taddi sp, sp, -32\n");
	// save return address to stack
	// TODO: what about frame pointer?
	instructions.push_str("\tsd ra, 24(sp)\n");
	for statement in &function.body.statements {
		match statement {
			// TODO: handle internal function special cases
			ASTBlockStatement::FunctionCall(astfunction_call) => {
				let mut arg_idx = 0;
				for val in &astfunction_call.args {
					match val {
						ASTFunctionCallArg::String(string) => {
							let static_data = format!(".D{}", ctx.static_data.len());
							ctx.add_static_string(string);
							instructions.push_str(&format!("\tla a{arg_idx}, {static_data}\n"));
							arg_idx += 1;
							instructions
								.push_str(&format!("\taddi a{arg_idx}, x0, {}\n", string.len()));
							arg_idx += 1;
						}
						_ => todo!("TODO: function argument\n{:#?}", val),
					};
				}
				instructions.push_str(&format!("\tcall {}\n", astfunction_call.name));
			}
			_ => todo!("TODO: function statement\n{:#?}", statement),
		}
	}

	// load return addreess from stack
	instructions.push_str("\tld ra, 24(sp)\n");
	// restore stack pointer
	instructions.push_str("\taddi sp, sp, 32\n");
	instructions.push_str("\tret\n");
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
		let mut instructions = String::new();

		let mut ctx = CompileCtx {
			static_data: Vec::new(),
		};

		for function in &self.ast.nodes {
			compile_function(&mut ctx, &mut instructions, function)
		}

		let data: Vec<String> = ctx
			.static_data
			.iter()
			.enumerate()
			.map(|(idx, value)| match value {
				StaticData::StaticString(string) => {
					format!(".D{idx}: .ascii \"{}\"", string.replace('\n', "\\n"))
				}
			})
			.collect();

		let data = data.join("\n");

		instructions.push('\n');
		instructions.push_str(&data);
		instructions
	}
}
