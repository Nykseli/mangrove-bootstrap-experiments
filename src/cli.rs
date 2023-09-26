use clap::Parser;

#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
pub struct Args {
	#[arg(short = 'O', long, help = "Optimise code AST before compiling")]
	optimise: bool,
	file: String,
}

impl Args {
	pub fn optimise(&self) -> bool {
		self.optimise
	}

	pub fn file(&self) -> &str {
		&self.file
	}
}
