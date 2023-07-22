use std::{fs, path::Path};

use mangrove_rs::parser::{tokeniser::Tokeniser, types::TokenType};

fn tokeniser_for(file: &str) -> Tokeniser {
	let path = Path::new("tests/cases/tokenisation/");
	let path = path.join(file);
	Tokeniser::new(fs::read_to_string(path).unwrap())
}

fn read_value(tokeniser: &mut Tokeniser, expected_type: TokenType, expected_value: &str) {
	let token = tokeniser.next_token().unwrap();
	assert!(token.valid());
	assert_eq!(token.type_(), expected_type);
	assert_eq!(token.value(), expected_value);
}

fn read_empty_value(tokeniser: &mut Tokeniser, expected_type: TokenType) {
	let token = tokeniser.next_token().unwrap();
	assert!(token.valid());
	assert_eq!(token.type_(), expected_type);
	assert!(token.value().is_empty());
}

fn read_newline(tokeniser: &mut Tokeniser) {
	let token = tokeniser.next_token().unwrap();
	if !token.valid() {
		println!("{token:?}");
	}
	assert!(token.valid());
	assert_eq!(token.type_(), TokenType::Newline);
	assert!(token.value().is_empty());
}

fn read_eof(tokeniser: &mut Tokeniser) {
	let token = tokeniser.next_token().unwrap();
	assert!(token.valid());
	assert_eq!(token.type_(), TokenType::Eof);
	assert!(token.value().is_empty());
}

fn read_invalid(tokeniser: &mut Tokeniser) {
	let token = tokeniser.next_token().unwrap();
	assert!(!token.valid());
	assert_eq!(token.type_(), TokenType::Invalid);
	assert!(token.value().is_empty());
}

fn read_whitespace(tokeniser: &mut Tokeniser) {
	let token = tokeniser.next_token().unwrap();
	assert!(token.valid());
	assert_eq!(token.type_(), TokenType::Whitespace);
	// TODO:
	// assert!(!token.value().is_empty());
}

fn read_assignment(
	tokeniser: &mut Tokeniser,
	ident_value: &str,
	assignt_op_value: &str,
	literal_value: &str,
) {
	read_value(tokeniser, TokenType::Ident, ident_value);
	read_whitespace(tokeniser);
	read_value(tokeniser, TokenType::AssignOp, assignt_op_value);
	read_whitespace(tokeniser);
	read_value(tokeniser, TokenType::IntLit, literal_value);
	read_newline(tokeniser);
}

#[test]
fn test_integer_literals() {
	let mut tokeniser = tokeniser_for("integralLiterals.case");

	// It is assumed after each test value that a single Linux-style new line follows
	// Consume the first token from the input and start testing tokenisation
	println!("Checking tokenisation of '0'");
	read_value(&mut tokeniser, TokenType::IntLit, "0");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '07'");
	read_value(&mut tokeniser, TokenType::IntLit, "07");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '08'");
	read_value(&mut tokeniser, TokenType::IntLit, "08");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '0b1001'");
	read_value(&mut tokeniser, TokenType::BinLit, "1001");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '0b'");
	read_invalid(&mut tokeniser);
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '0c11'");
	read_value(&mut tokeniser, TokenType::OctLit, "11");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '0c'");
	read_invalid(&mut tokeniser);
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '0x95'");
	read_value(&mut tokeniser, TokenType::HexLit, "95");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '0x'");
	read_invalid(&mut tokeniser);
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '100'");
	read_value(&mut tokeniser, TokenType::IntLit, "100");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '6'");
	read_value(&mut tokeniser, TokenType::IntLit, "6");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '0a'");
	read_value(&mut tokeniser, TokenType::IntLit, "0");
	read_value(&mut tokeniser, TokenType::Ident, "a");
	read_newline(&mut tokeniser);
	// Finally, consume one last token and make sure it's the EOF token
	read_eof(&mut tokeniser);
}

#[test]
fn test_string_literals() {
	let mut tokeniser = tokeniser_for("stringLiterals.case");
	// It is assumed after each test value that a single Linux-style new line follows
	// Consume the first token from the input and start testing tokenisation
	println!("Checking tokenisation of \"\"");
	read_value(&mut tokeniser, TokenType::StringLit, "");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of \"The quick brown fox jumped over the lazy dog\"");
	read_value(
		&mut tokeniser,
		TokenType::StringLit,
		"The quick brown fox jumped over the lazy dog",
	);
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of \"\\ufffd\"");
	read_value(&mut tokeniser, TokenType::StringLit, "\u{fffd}");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of \"⛏\"");
	read_value(&mut tokeniser, TokenType::StringLit, "⛏");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of \"🥭💖\"");
	read_value(&mut tokeniser, TokenType::StringLit, "🥭💖");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of \"🎉 unicode literals 🎊\"");
	read_value(
		&mut tokeniser,
		TokenType::StringLit,
		"🎉 unicode literals 🎊",
	);
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of ''");
	read_invalid(&mut tokeniser);
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '🍑'");
	read_value(&mut tokeniser, TokenType::CharLit, "🍑");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '⛏'");
	read_value(&mut tokeniser, TokenType::CharLit, "⛏");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of \"'\"");
	read_value(&mut tokeniser, TokenType::StringLit, "'");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of \"\\\"\"");
	read_value(&mut tokeniser, TokenType::StringLit, "\"");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '\\''");
	read_value(&mut tokeniser, TokenType::CharLit, "'");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '\"'");
	read_value(&mut tokeniser, TokenType::CharLit, "\"");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '\\\\'");
	read_value(&mut tokeniser, TokenType::CharLit, "\\");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '\\r'");
	read_value(&mut tokeniser, TokenType::CharLit, "\r");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '\\n'");
	read_value(&mut tokeniser, TokenType::CharLit, "\n");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '\\b'");
	read_value(&mut tokeniser, TokenType::CharLit, "\u{08}");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '\\v'");
	read_value(&mut tokeniser, TokenType::CharLit, "\u{0B}");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '\\t'");
	read_value(&mut tokeniser, TokenType::CharLit, "\t");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '\\a'");
	read_value(&mut tokeniser, TokenType::CharLit, "\u{07}");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '\\f'");
	read_value(&mut tokeniser, TokenType::CharLit, "\u{0C}");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of \"\\'\"");
	read_invalid(&mut tokeniser);
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '\\\"'");
	read_invalid(&mut tokeniser);
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '\\/'");
	read_invalid(&mut tokeniser);
	read_newline(&mut tokeniser);
	// Finally, consume one last token and make sure it's the EOF token
	read_eof(&mut tokeniser);
}

#[test]
fn test_keywords() {
	let mut tokeniser = tokeniser_for("keywords.case");
	// It is assumed after each test value that a single Linux-style new line follows
	// Consume the first token from the input and start testing tokenisation
	println!("Checking tokenisation of 'true'");
	read_value(&mut tokeniser, TokenType::BoolLit, "true");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of 'false'");
	read_value(&mut tokeniser, TokenType::BoolLit, "false");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of 'nullptr'");
	read_value(&mut tokeniser, TokenType::NullptrLit, "nullptr");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of 'and'");
	read_value(&mut tokeniser, TokenType::LogicOp, "&");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of 'or'");
	read_value(&mut tokeniser, TokenType::LogicOp, "|");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of 'not'");
	read_value(&mut tokeniser, TokenType::Invert, "!");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of 'eeprom'");
	read_value(&mut tokeniser, TokenType::LocationSpec, "eeprom");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of 'flash'");
	read_value(&mut tokeniser, TokenType::LocationSpec, "flash");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of 'rom'");
	read_value(&mut tokeniser, TokenType::LocationSpec, "rom");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of 'const'");
	read_value(&mut tokeniser, TokenType::StorageSpec, "const");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of 'static'");
	read_value(&mut tokeniser, TokenType::StorageSpec, "static");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of 'volatile'");
	read_value(&mut tokeniser, TokenType::StorageSpec, "volatile");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of 'new'");
	read_value(&mut tokeniser, TokenType::NewStmt, "new");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of 'delete'");
	read_value(&mut tokeniser, TokenType::DeleteStmt, "delete");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of 'from'");
	read_value(&mut tokeniser, TokenType::FromStmt, "from");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of 'import'");
	read_value(&mut tokeniser, TokenType::ImportStmt, "import");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of 'as'");
	read_value(&mut tokeniser, TokenType::AsStmt, "as");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of 'return'");
	read_value(&mut tokeniser, TokenType::ReturnStmt, "return");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of 'if'");
	read_value(&mut tokeniser, TokenType::IfStmt, "if");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of 'elif'");
	read_value(&mut tokeniser, TokenType::ElifStmt, "elif");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of 'else'");
	read_value(&mut tokeniser, TokenType::ElseStmt, "else");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of 'for'");
	read_value(&mut tokeniser, TokenType::ForStmt, "for");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of 'while'");
	read_value(&mut tokeniser, TokenType::WhileStmt, "while");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of 'do'");
	read_value(&mut tokeniser, TokenType::DoStmt, "do");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of 'none'");
	read_value(&mut tokeniser, TokenType::NoneType, "none");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of 'class'");
	read_value(&mut tokeniser, TokenType::ClassDef, "class");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of 'enum'");
	read_value(&mut tokeniser, TokenType::EnumDef, "enum");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of 'function'");
	read_value(&mut tokeniser, TokenType::FunctionDef, "function");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of 'operator'");
	read_value(&mut tokeniser, TokenType::OperatorDef, "operator");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of 'public'");
	read_value(&mut tokeniser, TokenType::Visibility, "public");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of 'protected'");
	read_value(&mut tokeniser, TokenType::Visibility, "protected");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of 'private'");
	read_value(&mut tokeniser, TokenType::Visibility, "private");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of 'unsafe'");
	read_value(&mut tokeniser, TokenType::Unsafe, "unsafe");
	read_newline(&mut tokeniser);
	// Finally, consume one last token and make sure it's the EOF token
	read_eof(&mut tokeniser);
}

#[test]
fn test_assignments() {
	let mut tokeniser = tokeniser_for("assignments.case");
	// It is assumed after each test value that a single Linux-style new line follows
	// Consume the first token from the input and start testing tokenisation
	println!("Checking tokenisation of 'a = 1'");
	read_assignment(&mut tokeniser, "a", "=", "1");
	println!("Checking tokenisation of 'b += 2'");
	read_assignment(&mut tokeniser, "b", "+=", "2");
	println!("Checking tokenisation of 'c -= 3'");
	read_assignment(&mut tokeniser, "c", "-=", "3");
	println!("Checking tokenisation of 'd *= 4'");
	read_assignment(&mut tokeniser, "d", "*=", "4");
	println!("Checking tokenisation of 'e /= 5'");
	read_assignment(&mut tokeniser, "e", "/=", "5");
	println!("Checking tokenisation of 'f %= 6'");
	read_assignment(&mut tokeniser, "f", "%=", "6");
	println!("Checking tokenisation of 'g &= 7'");
	read_assignment(&mut tokeniser, "g", "&=", "7");
	println!("Checking tokenisation of 'h |= 8'");
	read_assignment(&mut tokeniser, "h", "|=", "8");
	println!("Checking tokenisation of 'i ^= 9'");
	read_assignment(&mut tokeniser, "i", "^=", "9");
	println!("Checking tokenisation of 'j >>= 10'");
	read_assignment(&mut tokeniser, "j", "<<=", "10");
	println!("Checking tokenisation of 'k <<= 11'");
	read_assignment(&mut tokeniser, "k", ">>=", "11");
	// Finally, consume one last token and make sure it's the EOF token
	read_eof(&mut tokeniser);
}

#[test]
fn test_punctuation() {
	let mut tokeniser = tokeniser_for("punctuation.case");
	// It is assumed after each test value that a single Linux-style new line follows
	// Consume the first token from the input and start testing tokenisation
	println!("Checking tokenisation of '.'");
	read_empty_value(&mut tokeniser, TokenType::Dot);
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '..'");
	read_empty_value(&mut tokeniser, TokenType::Dot);
	read_empty_value(&mut tokeniser, TokenType::Dot);
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '...'");
	read_empty_value(&mut tokeniser, TokenType::Ellipsis);
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '....'");
	read_empty_value(&mut tokeniser, TokenType::Ellipsis);
	read_empty_value(&mut tokeniser, TokenType::Dot);
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '# Line comment'");
	read_value(&mut tokeniser, TokenType::Comment, " Line comment");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '// Other line comment'");
	read_value(&mut tokeniser, TokenType::Comment, " Other line comment");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '/**/'");
	read_empty_value(&mut tokeniser, TokenType::Comment);
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '/* Partial * line comment */'");
	read_value(
		&mut tokeniser,
		TokenType::Comment,
		" Partial * line comment ",
	);
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '[]'");
	read_empty_value(&mut tokeniser, TokenType::LeftSquare);
	read_empty_value(&mut tokeniser, TokenType::RightSquare);
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '{{}}'");
	read_empty_value(&mut tokeniser, TokenType::LeftBrace);
	read_empty_value(&mut tokeniser, TokenType::RightBrace);
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '()'");
	read_empty_value(&mut tokeniser, TokenType::LeftParen);
	read_empty_value(&mut tokeniser, TokenType::RightParen);
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of ','");
	read_empty_value(&mut tokeniser, TokenType::Comma);
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of ':'");
	read_empty_value(&mut tokeniser, TokenType::Colon);
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of ';'");
	read_empty_value(&mut tokeniser, TokenType::Semi);
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '<'");
	read_value(&mut tokeniser, TokenType::RelOp, "<");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '<='");
	read_value(&mut tokeniser, TokenType::RelOp, "<=");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '>'");
	read_value(&mut tokeniser, TokenType::RelOp, ">");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '>='");
	read_value(&mut tokeniser, TokenType::RelOp, ">=");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '!='");
	read_value(&mut tokeniser, TokenType::EquOp, "!=");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '=='");
	read_value(&mut tokeniser, TokenType::EquOp, "==");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '+'");
	read_value(&mut tokeniser, TokenType::AddOp, "+");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '-'");
	read_value(&mut tokeniser, TokenType::AddOp, "-");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '*'");
	read_value(&mut tokeniser, TokenType::MulOp, "*");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '/'");
	read_value(&mut tokeniser, TokenType::MulOp, "/");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '%'");
	read_value(&mut tokeniser, TokenType::MulOp, "%");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '~'");
	read_value(&mut tokeniser, TokenType::Invert, "~");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '!'");
	read_value(&mut tokeniser, TokenType::Invert, "!");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '&'");
	read_value(&mut tokeniser, TokenType::BitOp, "&");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '|'");
	read_value(&mut tokeniser, TokenType::BitOp, "|");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '^'");
	read_value(&mut tokeniser, TokenType::BitOp, "^");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '&&'");
	read_value(&mut tokeniser, TokenType::LogicOp, "&");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '||'");
	read_value(&mut tokeniser, TokenType::LogicOp, "|");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '>>'");
	read_value(&mut tokeniser, TokenType::ShiftOp, ">>");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '<<'");
	read_value(&mut tokeniser, TokenType::ShiftOp, "<<");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '++'");
	read_value(&mut tokeniser, TokenType::IncOp, "+");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '--'");
	read_value(&mut tokeniser, TokenType::IncOp, "-");
	read_newline(&mut tokeniser);
	println!("Checking tokenisation of '->'");
	read_empty_value(&mut tokeniser, TokenType::Arrow);
	read_newline(&mut tokeniser);
	// Finally, consume one last token and make sure it's the EOF token
	read_eof(&mut tokeniser);
}
