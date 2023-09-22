use super::{
	recogniser::{CharRecogniser, StringRecogniser},
	types::{Position, Range, Token, TokenType},
};

/// Err means EOF
#[allow(clippy::result_unit_err)]
type TokenResult<T> = Result<T, ()>;

#[derive(Debug)]
#[allow(dead_code)]
/// Helper struct for reading tokens
struct ReadStart {
	read_pos: usize,
	position: Position,
}

#[derive(Debug)]
pub struct Tokeniser {
	file: Vec<char>,
	current_char: char,
	read_pos: usize,
	position: Position,
	eof: bool,
}

#[allow(dead_code)]
impl Tokeniser {
	pub fn new(file: String) -> Self {
		let file: Vec<char> = file.chars().collect();
		Self {
			current_char: file[0],
			file,
			read_pos: 0,
			position: Position {
				line: 0,
				character: 0,
			},
			eof: false,
		}
	}

	fn gen_token(&self, type_: TokenType) -> TokenResult<Token> {
		Ok(Token::from_type(type_))
	}

	fn simple_token(&mut self, type_: TokenType) -> TokenResult<Token> {
		let _ = self.next_char();
		Ok(Token::from_type(type_))
	}

	fn simple_token_value(&mut self, type_: TokenType) -> TokenResult<Token> {
		// TODO: merge this with simple_token so every token has a char or string
		let start = self.read_start();
		let _ = self.next_char();
		self.from_read_start(start, type_)
	}

	#[allow(clippy::wrong_self_convention)]
	fn from_read_start(&self, start: ReadStart, type_: TokenType) -> TokenResult<Token> {
		let value: String = self.file[start.read_pos..self.read_pos].iter().collect();
		let val_len = value.len();
		Ok(Token::new(type_, value, Range::default(), val_len))
	}

	fn read_start(&self) -> ReadStart {
		ReadStart {
			position: self.position.clone(),
			read_pos: self.read_pos,
		}
	}

	#[allow(clippy::result_unit_err)]
	pub fn next_token(&mut self) -> TokenResult<Token> {
		if self.eof {
			self.gen_token(TokenType::Eof)
		} else {
			self.read_token()
		}
	}

	#[allow(clippy::result_unit_err)]
	pub fn peek_token(&mut self, count: usize) -> TokenResult<Token> {
		let current_char = self.current_char;
		let read_pos = self.read_pos;
		let position = self.position.clone();
		let eof = self.eof;

		for _ in 0..count - 1 {
			if self.eof {
				return self.gen_token(TokenType::Eof);
			} else {
				self.read_token().unwrap();
			}
		}

		let copy = if self.eof {
			self.gen_token(TokenType::Eof)
		} else {
			self.read_token()
		};

		self.current_char = current_char;
		self.read_pos = read_pos;
		self.position = position;
		self.eof = eof;

		copy
	}

	fn peek_char(&self) -> TokenResult<char> {
		if self.read_pos + 1 >= self.file.len() {
			return Err(());
		}

		Ok(self.file[self.read_pos + 1])
	}

	fn next_char(&mut self) -> TokenResult<char> {
		if self.read_pos + 1 >= self.file.len() {
			self.eof = true;
			return Err(());
		}

		self.read_pos += 1;
		self.position.line += 1;
		self.current_char = self.file[self.read_pos];
		Ok(self.current_char)
	}

	// TODO: we need cleaner solution than magic bool
	fn next_escaped_char(&mut self) -> TokenResult<(bool, char)> {
		let _c = self.next_char()?;
		if self.current_char != '\\' {
			return Ok((false, self.current_char));
		}

		let c = self.next_char()?;
		let c = match c {
			'\\' => '\\',
			'b' => '\x08',
			'r' => '\x0D',
			'n' => '\x0A',
			't' => '\x09',
			'v' => '\x0B',
			'f' => '\x0C',
			'a' => '\x07',
			'u' | 'U' => {
				let token = self.read_hex_token()?;
				// since this reads until the last valid char, we need to go back one
				self.read_pos -= 1;
				self.position.line -= 1;
				self.current_char = self.file[self.read_pos];
				// TODO: return errors instead of panic
				char::from_u32(u32::from_str_radix(token.value(), 16).unwrap()).unwrap()
			}
			_ => c,
		};

		Ok((true, c))
	}

	fn read_token(&mut self) -> TokenResult<Token> {
		match self.current_char {
			' ' | '\t' => self.simple_token(TokenType::Whitespace),
			'#' => self.read_line_comment(),
			'\r' | '\n' => self.simple_token(TokenType::Newline),
			'.' => self.read_ellipsis_token(),
			';' => self.simple_token(TokenType::Semi),
			'{' => self.simple_token(TokenType::LeftBrace),
			'}' => self.simple_token(TokenType::RightBrace),
			'(' => self.simple_token(TokenType::LeftParen),
			')' => self.simple_token(TokenType::RightParen),
			'[' => self.simple_token(TokenType::LeftSquare),
			']' => self.simple_token(TokenType::RightSquare),
			',' => self.simple_token(TokenType::Comma),
			':' => self.simple_token(TokenType::Colon),
			'"' => self.read_string_token(),
			'\'' => self.read_char_token(),
			'~' => self.simple_token_value(TokenType::Invert),
			'/' => self.read_div_token(),
			'*' | '%' => self.read_mul_token(),
			'+' | '-' => self.read_add_token(),
			'&' | '|' => self.read_boolean_token(),
			'^' => self.read_bitwise_token(),
			'<' | '>' => self.read_relation_token(),
			'!' | '=' => self.read_equality_token(),
			_ => self.read_extended_token(),
		}
	}

	fn read_line_comment(&mut self) -> TokenResult<Token> {
		self.next_char()?;
		let start = self.read_start();
		while self.current_char != '\n' {
			self.next_char()?;
		}

		self.from_read_start(start, TokenType::Comment)
	}

	fn read_part_comment(&mut self) -> TokenResult<Token> {
		self.next_char()?;
		let mut com = vec![];
		loop {
			if self.current_char == '*' && self.peek_char()? == '/' {
				// skip the ending '*/'
				self.next_char()?;
				self.next_char()?;
				break;
			}
			com.push(self.current_char);
			self.next_char()?;
		}

		let value: String = com.iter().collect();
		let val_len = value.len();
		Ok(Token::new(
			TokenType::Comment,
			value,
			Range::default(),
			val_len,
		))
	}

	fn read_ellipsis_token(&mut self) -> TokenResult<Token> {
		let _ = self.read_start();
		// skip the first .
		let current = self.next_char()?;
		let type_ = if current == '.' && self.peek_char()? == '.' {
			// Skip the next two ellipses
			self.next_char()?;
			self.next_char()?;
			TokenType::Ellipsis
		} else {
			TokenType::Dot
		};

		self.gen_token(type_)
	}

	fn read_string_token(&mut self) -> TokenResult<Token> {
		let c = self.peek_char()?;
		// Empty string
		if c.is_double_quote() {
			// skip the both '"'s
			self.next_char()?;
			self.next_char()?;
			return Ok(Token::new(
				TokenType::StringLit,
				"".into(),
				Range::default(),
				0,
			));
		}

		let mut lit = vec![];
		let mut invalid_sequence = false;
		loop {
			let c = self.next_escaped_char()?;
			if !c.0 && c.1.is_double_quote() {
				break;
			}

			if c.0 && c.1.is_single_quote() {
				invalid_sequence = true;
			}

			lit.push(c.1);
		}

		// skip the '"'
		self.next_char()?;
		let value: String = lit.iter().collect();
		let val_len = value.len();
		if !invalid_sequence {
			Ok(Token::new(
				TokenType::StringLit,
				value,
				Range::default(),
				val_len,
			))
		} else {
			self.gen_token(TokenType::Invalid)
		}
	}

	fn read_char_token(&mut self) -> TokenResult<Token> {
		let c = self.peek_char()?;
		// Empty string
		if c.is_single_quote() {
			// skip the both '\''s
			self.next_char()?;
			self.next_char()?;
			return Ok(Token::new(
				TokenType::Invalid,
				"".into(),
				Range::default(),
				0,
			));
		}

		let mut lit = vec![];
		let mut invalid_sequence = false;
		loop {
			let c = self.next_escaped_char()?;
			if !c.0 && c.1.is_single_quote() {
				break;
			}

			if c.0 && (c.1.is_double_quote() || c.1 == '/') {
				invalid_sequence = true;
			}

			lit.push(c.1);
		}

		// skip the '\''
		self.next_char()?;
		let value: String = lit.iter().collect();
		let val_len = value.len();
		if !invalid_sequence {
			Ok(Token::new(
				TokenType::CharLit,
				value,
				Range::default(),
				val_len,
			))
		} else {
			self.gen_token(TokenType::Invalid)
		}
	}

	fn read_div_token(&mut self) -> TokenResult<Token> {
		let start = self.read_start();

		match self.next_char()? {
			'*' => self.read_part_comment(),
			'/' => self.read_line_comment(),
			'=' => {
				// consume the '='
				self.next_char()?;
				self.from_read_start(start, TokenType::AssignOp)
			}
			_ => self.from_read_start(start, TokenType::MulOp),
		}
	}

	fn read_mul_token(&mut self) -> TokenResult<Token> {
		let start = self.read_start();
		let next = self.next_char()?;

		if next.is_equals() {
			// consume the '='
			self.next_char()?;
			self.from_read_start(start, TokenType::AssignOp)
		} else {
			self.from_read_start(start, TokenType::MulOp)
		}
	}

	fn read_add_token(&mut self) -> TokenResult<Token> {
		let current = self.current_char;
		let start = self.read_start();
		let next = self.next_char()?;

		if next.is_equals() {
			// consume the '='
			self.next_char()?;
			self.from_read_start(start, TokenType::AssignOp)
		} else if current == '-' && next == '>' {
			// consume the '>'
			self.next_char()?;
			self.gen_token(TokenType::Arrow)
		} else if current == next {
			// This is the way the original parser does it
			let token = self.from_read_start(start, TokenType::IncOp);
			self.next_char()?;
			token
		} else {
			self.from_read_start(start, TokenType::AddOp)
		}
	}

	fn read_boolean_token(&mut self) -> TokenResult<Token> {
		let current = self.current_char;
		let start = self.read_start();
		let next = self.next_char()?;

		if next.is_equals() {
			// consume the '='
			self.next_char()?;
			self.from_read_start(start, TokenType::AssignOp)
		} else if current == next {
			// This is the way the original parser does it
			let token = self.from_read_start(start, TokenType::LogicOp);
			self.next_char()?;
			token
		} else {
			self.from_read_start(start, TokenType::BitOp)
		}
	}

	fn read_bitwise_token(&mut self) -> TokenResult<Token> {
		let start = self.read_start();
		let next = self.next_char()?;

		if next.is_equals() {
			// consume the '='
			self.next_char()?;
			self.from_read_start(start, TokenType::AssignOp)
		} else {
			self.from_read_start(start, TokenType::BitOp)
		}
	}

	fn read_relation_token(&mut self) -> TokenResult<Token> {
		let current = self.current_char;
		let start = self.read_start();
		let next = self.next_char()?;

		if next.is_equals() {
			// consume the '='
			self.next_char()?;
			self.from_read_start(start, TokenType::RelOp)
		} else if current == next {
			if self.peek_char()?.is_equals() {
				self.next_char()?;
				self.next_char()?;
				self.from_read_start(start, TokenType::AssignOp)
			} else {
				self.next_char()?;
				self.from_read_start(start, TokenType::ShiftOp)
			}
		} else {
			self.from_read_start(start, TokenType::RelOp)
		}
	}

	fn read_equality_token(&mut self) -> TokenResult<Token> {
		let current = self.current_char;
		let start = self.read_start();
		let next = self.next_char()?;

		if next.is_equals() {
			// consume the '='
			self.next_char()?;
			self.from_read_start(start, TokenType::EquOp)
		} else if current.is_equals() {
			self.from_read_start(start, TokenType::AssignOp)
		} else {
			self.from_read_start(start, TokenType::Invert)
		}
	}

	fn read_bin_token(&mut self) -> TokenResult<Token> {
		// skip the 'b'
		self.next_char()?;
		let start = self.read_start();

		while self.current_char.is_bin() {
			self.next_char()?;
		}

		if start.read_pos == self.read_pos {
			return self.gen_token(TokenType::Invalid);
		}

		self.from_read_start(start, TokenType::BinLit)
	}

	fn read_oct_token(&mut self) -> TokenResult<Token> {
		// skip the 'c'
		self.next_char()?;
		let start = self.read_start();

		while self.current_char.is_oct() {
			self.next_char()?;
		}

		if start.read_pos == self.read_pos {
			return self.gen_token(TokenType::Invalid);
		}

		self.from_read_start(start, TokenType::OctLit)
	}

	fn read_hex_token(&mut self) -> TokenResult<Token> {
		// skip the 'x'
		self.next_char()?;
		let start = self.read_start();

		while self.current_char.is_hex() {
			self.next_char()?;
		}

		if start.read_pos == self.read_pos {
			return self.gen_token(TokenType::Invalid);
		}

		self.from_read_start(start, TokenType::HexLit)
	}

	fn read_int_token(&mut self) -> TokenResult<Token> {
		let start = self.read_start();
		if self.current_char == '0' {
			let c = self.next_char()?;
			if c.is_begin_bin() {
				return self.read_bin_token();
			} else if c.is_begin_oct() {
				return self.read_oct_token();
			} else if c.is_begin_hex() {
				return self.read_hex_token();
			}
		}

		while self.current_char.is_ascii_digit() {
			self.next_char()?;
		}

		self.from_read_start(start, TokenType::IntLit)
	}

	fn read_alpha_num_token(&mut self) -> TokenResult<Token> {
		let start = self.read_start();
		while self.current_char.is_alphanumeric() || self.current_char.is_underscore() {
			self.next_char()?;
		}
		let mut token = self.from_read_start(start, TokenType::Ident)?;
		let value = token.value();
		if value.is_true() || value.is_false() {
			token.set_type(TokenType::BoolLit);
		} else if value.is_null() {
			token.set_type(TokenType::NullptrLit);
		} else if value.is_and() {
			token.set_type(TokenType::LogicOp);
			token.set_value('&'.into());
		} else if value.is_or() {
			token.set_type(TokenType::LogicOp);
			token.set_value('|'.into());
		} else if value.is_not() {
			token.set_type(TokenType::Invert);
			token.set_value('!'.into());
		} else if value.is_location_spec() {
			token.set_type(TokenType::LocationSpec);
		} else if value.is_storage_spec() {
			token.set_type(TokenType::StorageSpec);
		} else if value.is_new() {
			token.set_type(TokenType::NewStmt);
		} else if value.is_delete() {
			token.set_type(TokenType::DeleteStmt);
		} else if value.is_from() {
			token.set_type(TokenType::FromStmt);
		} else if value.is_import() {
			token.set_type(TokenType::ImportStmt);
		} else if value.is_as() {
			token.set_type(TokenType::AsStmt);
		} else if value.is_return() {
			token.set_type(TokenType::ReturnStmt);
		} else if value.is_if_stmt() {
			token.set_type(TokenType::IfStmt);
		} else if value.is_elif_stmt() {
			token.set_type(TokenType::ElifStmt);
		} else if value.is_else_stmt() {
			token.set_type(TokenType::ElseStmt);
		} else if value.is_for_stmt() {
			token.set_type(TokenType::ForStmt);
		} else if value.is_while_stmt() {
			token.set_type(TokenType::WhileStmt);
		} else if value.is_do_stmt() {
			token.set_type(TokenType::DoStmt);
		} else if value.is_none() {
			token.set_type(TokenType::NoneType);
		} else if value.is_class() {
			token.set_type(TokenType::ClassDef);
		} else if value.is_enum() {
			token.set_type(TokenType::EnumDef);
		} else if value.is_function_def() {
			token.set_type(TokenType::FunctionDef);
		} else if value.is_operator_def() {
			token.set_type(TokenType::OperatorDef);
		} else if value.is_visibility() {
			token.set_type(TokenType::Visibility);
		} else if value.is_unsafe() {
			token.set_type(TokenType::Unsafe);
		}

		Ok(token)
	}

	fn read_extended_token(&mut self) -> TokenResult<Token> {
		if self.current_char.is_alphabetic() || self.current_char.is_underscore() {
			return self.read_alpha_num_token();
		} else if self.current_char.is_ascii_digit() {
			return self.read_int_token();
		}

		self.gen_token(TokenType::Invalid)
	}
}
