use crate::{compiler::common::Compiler, parser::parse::Parser};

#[derive(Debug)]
pub struct RiscVCompiler {
	ast: Parser,
}

impl Compiler for RiscVCompiler {
	fn new(ast: Parser) -> Self {
		Self { ast }
	}

	fn compile(&mut self) -> String {
		String::from("hello from riscv")
	}
}
