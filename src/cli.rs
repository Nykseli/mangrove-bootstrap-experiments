use clap::{Parser, ValueEnum};

#[derive(Debug, Clone, Copy, Default, ValueEnum)]
pub enum TargetArch {
	/// Web assembly
	#[default]
	Wasm,
	/// 64 bit RISC-V (experimental)
	Riscv,
}

#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
pub struct Args {
	#[arg(short = 'O', long, help = "Optimise code AST before compiling")]
	optimise: bool,
	#[arg(short = 't', long, help = "Target architecture (default: wasm)")]
	target: Option<TargetArch>,
	file: String,
}

impl Args {
	pub fn optimise(&self) -> bool {
		self.optimise
	}

	pub fn file(&self) -> &str {
		&self.file
	}

	pub fn target(&self) -> TargetArch {
		self.target.unwrap_or_default()
	}
}
