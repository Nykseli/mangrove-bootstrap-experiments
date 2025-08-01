use std::fmt::Display;

/// RISC-V registers
///
/// The RV32I base integer ISA includes 32 registers, named x0 to x31.
/// https://asm-docs.microagi.org/risc-v/riscv-asm.html
#[derive(Debug)]
#[allow(unused)]
pub enum Register {
	/// x0, hardwired to 0, ignores writes
	Zero,
	/// x1, return address for jumps
	Ra,
	/// x2, stack pointer
	Sp,
	/// x3, global pointer
	Gp,
	/// x4, thread pointer
	Tp,
	/// x5, temporary register 0
	T0,
	/// x6, temporary register 1
	T1,
	/// x7, temporary register 2
	T2,
	/// x8, saved register 0 or frame pointer, alias to `S0`
	Fp,
	/// x8, saved register 0 or frame pointer, aloas to `Fp`
	S0,
	/// x9, saved register 1
	S1,
	/// x10, return value or function argument 0
	A0,
	/// x11, return value or function argument 1
	A1,
	/// x12, function argument 2
	A2,
	/// x13, function argument 3
	A3,
	/// x14, function argument 4
	A4,
	/// x15, function argument 5
	A5,
	/// x16, function argument 6
	A6,
	/// x17, function argument 7
	A7,
	/// x18, saved register 2
	S2,
	/// x19, saved register 3
	S3,
	/// x20, saved register 4
	S4,
	/// x21, saved register 5
	S5,
	/// x22, saved register 6
	S6,
	/// x23, saved register 7
	S7,
	/// x24, saved register 8
	S8,
	/// x25, saved register 9
	S9,
	/// x26, saved register 10
	S10,
	/// x27, saved register 11
	S11,
	/// x28, temporary register 3
	T3,
	/// x29, temporary register 4
	T4,
	/// x30, temporary register 5
	T5,
	/// x31, temporary register 6
	T6,
	/// pc, program counter
	Pc,
}

impl Display for Register {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Register::Zero => write!(f, "zero")?,
			Register::Ra => write!(f, "ra")?,
			Register::Sp => write!(f, "sp")?,
			Register::Gp => write!(f, "gp")?,
			Register::Tp => write!(f, "tp")?,
			Register::T0 => write!(f, "t0")?,
			Register::T1 => write!(f, "t1")?,
			Register::T2 => write!(f, "t2")?,
			Register::Fp => write!(f, "fp")?,
			Register::S0 => write!(f, "s0")?,
			Register::S1 => write!(f, "s1")?,
			Register::A0 => write!(f, "a0")?,
			Register::A1 => write!(f, "a1")?,
			Register::A2 => write!(f, "a2")?,
			Register::A3 => write!(f, "a3")?,
			Register::A4 => write!(f, "a4")?,
			Register::A5 => write!(f, "a5")?,
			Register::A6 => write!(f, "a6")?,
			Register::A7 => write!(f, "a7")?,
			Register::S2 => write!(f, "s2")?,
			Register::S3 => write!(f, "s3")?,
			Register::S4 => write!(f, "s4")?,
			Register::S5 => write!(f, "s5")?,
			Register::S6 => write!(f, "s6")?,
			Register::S7 => write!(f, "s7")?,
			Register::S8 => write!(f, "s8")?,
			Register::S9 => write!(f, "s9")?,
			Register::S10 => write!(f, "s10")?,
			Register::S11 => write!(f, "s11")?,
			Register::T3 => write!(f, "t3")?,
			Register::T4 => write!(f, "t4")?,
			Register::T5 => write!(f, "t5")?,
			Register::T6 => write!(f, "t6")?,
			Register::Pc => write!(f, "pc")?,
		};

		Ok(())
	}
}
