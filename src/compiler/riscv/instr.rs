use std::fmt::Display;

use crate::compiler::riscv::register::Register;

/// RISC-V 64 bit instruction set
///
/// Speficication version 20250508
///
/// https://github.com/riscv/riscv-isa-manual/tree/20250508
/// and
/// https://lf-riscv.atlassian.net/wiki/spaces/HOME/pages/16154769/RISC-V+Technical+Specifications
#[derive(Debug)]
pub enum Instruction {
	/// 2.4.1. Integer Register-Immediate Instructions
	/// adds the sign-extended 12-bit immediate to register
	/// ex: addi sp, sp, -32
	Addi {
		dst: Register,
		src: Register,
		/// immidiate value
		imm: i32,
	},
	/// 2.5.1. Unconditional Jumps
	/// Call + label that assembler will compile to jal(r) instruction
	/// ex: call __print_str
	Call(String),
	/// TODO: find documentation from spec
	/// Pseudo instruction for loading a symbol
	La { dest: Register, label: String },
	/// 4.3. Load and Store Instructions and 34.5.1. lb
	/// The Lb instruction loads a 8-bit value from memory into register rd for RV64I
	/// 8 bit version of lw instruction
	Lb {
		dest: Register,
		base: Register,
		offset: i32,
	},
	/// 4.3. Load and Store Instructions and 34.5.1. ld
	/// The LD instruction loads a 64-bit value from memory into register rd for RV64I
	/// 64 bit version of lw instruction
	Ld {
		dest: Register,
		base: Register,
		offset: i32,
	},
	/// TODO: find documentation from spec
	/// Pseudo instruction for loading immideate integer
	Li { dest: Register, value: u64 },
	/// 4.3. Load and Store Instructions and 34.5.1. lw
	/// The LD instruction loads a 32-bit value from memory into register rd for RV32I
	Lw {
		dest: Register,
		base: Register,
		offset: i32,
	},
	/// TODO: find documentation from spec
	/// pseudo instruction for returning from function
	Ret,
	/// 4.3. Load and Store Instructions and 34.5.2. sb
	/// The Sb, instructions store 8-bit value from the low bits of register src to memory.
	/// SD, SW, and SH are 64, 32, and 16 bit versions of this
	Sb {
		src: Register,
		base: Register,
		offset: i32,
	},
	/// 4.3. Load and Store Instructions and 34.5.2. sd
	/// The SD, instructions store 64-bit value from the low bits of register src to memory.
	/// SW, SH, and SB are 32, 16, and 8 bit versions of this
	Sd {
		src: Register,
		base: Register,
		offset: i32,
	},
	/// 4.3. Load and Store Instructions and 34.5.2. sw
	/// The SW, instructions store 32-bit value from the low bits of register src to memory.
	/// SD, SH, and SB are 64, 16, and 8 bit versions of this
	Sw {
		src: Register,
		base: Register,
		offset: i32,
	},
}

impl Display for Instruction {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Instruction::Addi { dst, src, imm } => write!(f, "addi {dst}, {src}, {imm}")?,
			Instruction::Call(label) => write!(f, "call {label}")?,
			Instruction::La { dest, label } => write!(f, "la {dest}, {label}")?,
			Instruction::Lb { dest, base, offset } => write!(f, "lb {dest}, {offset}({base})")?,
			Instruction::Ld { dest, base, offset } => write!(f, "ld {dest}, {offset}({base})")?,
			Instruction::Li { dest, value } => write!(f, "li {dest}, {value}")?,
			Instruction::Lw { dest, base, offset } => write!(f, "lw {dest}, {offset}({base})")?,
			Instruction::Ret => write!(f, "ret")?,
			Instruction::Sb { src, base, offset } => write!(f, "sb {src}, {offset}({base})")?,
			Instruction::Sd { src, base, offset } => write!(f, "sd {src}, {offset}({base})")?,
			Instruction::Sw { src, base, offset } => write!(f, "s {src}, {offset}({base})")?,
		}
		Ok(())
	}
}
