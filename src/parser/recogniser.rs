pub trait CharRecogniser {
	fn is_new_line(&self) -> bool;
	fn is_underscore(&self) -> bool;
	fn is_begin_bin(&self) -> bool;
	fn is_begin_oct(&self) -> bool;
	fn is_begin_hex(&self) -> bool;
	fn is_bin(&self) -> bool;
	fn is_oct(&self) -> bool;
	fn is_hex(&self) -> bool;
	fn is_dot(&self) -> bool;
	fn is_single_quote(&self) -> bool;
	fn is_double_quote(&self) -> bool;
	fn is_equals(&self) -> bool;
}

impl CharRecogniser for char {
	fn is_double_quote(&self) -> bool {
		*self == '"'
	}

	fn is_new_line(&self) -> bool {
		*self == '\n' || *self == '\r'
	}
	fn is_underscore(&self) -> bool {
		*self == '_'
	}
	fn is_begin_bin(&self) -> bool {
		*self == 'B' || *self == 'b'
	}
	fn is_begin_oct(&self) -> bool {
		*self == 'C' || *self == 'c'
	}
	fn is_begin_hex(&self) -> bool {
		*self == 'X' || *self == 'x'
	}
	fn is_bin(&self) -> bool {
		*self == '0' || *self == '1'
	}
	fn is_oct(&self) -> bool {
		('0'..='7').contains(self)
	}
	fn is_hex(&self) -> bool {
		self.is_ascii_digit() || ('A'..='F').contains(self) || ('a'..='f').contains(self)
	}
	fn is_dot(&self) -> bool {
		*self == '.'
	}
	fn is_single_quote(&self) -> bool {
		*self == '\''
	}
	fn is_equals(&self) -> bool {
		*self == '='
	}
}

pub trait StringRecogniser {
	fn is_true(&self) -> bool;
	fn is_false(&self) -> bool;
	fn is_null(&self) -> bool;
	fn is_and(&self) -> bool;
	fn is_or(&self) -> bool;
	fn is_not(&self) -> bool;
	fn is_begin_tmpl(&self) -> bool;
	fn is_end_tmpl(&self) -> bool;
	fn is_equality(&self) -> bool;
	fn is_relation(&self) -> bool;
	fn is_new(&self) -> bool;
	fn is_delete(&self) -> bool;
	fn is_from(&self) -> bool;
	fn is_import(&self) -> bool;
	fn is_as(&self) -> bool;
	fn is_return(&self) -> bool;
	fn is_if_stmt(&self) -> bool;
	fn is_elif_stmt(&self) -> bool;
	fn is_else_stmt(&self) -> bool;
	fn is_for_stmt(&self) -> bool;
	fn is_while_stmt(&self) -> bool;
	fn is_do_stmt(&self) -> bool;
	fn is_class(&self) -> bool;
	fn is_enum(&self) -> bool;
	fn is_function_def(&self) -> bool;
	fn is_operator_def(&self) -> bool;
	fn is_const(&self) -> bool;
	fn is_static(&self) -> bool;
	fn is_volatile(&self) -> bool;
	fn is_location_spec(&self) -> bool;
	fn is_storage_spec(&self) -> bool;
	fn is_none(&self) -> bool;
	fn is_arrow(&self) -> bool;
	fn is_visibility(&self) -> bool;
	fn is_unsafe(&self) -> bool;
}

impl StringRecogniser for str {
	fn is_true(&self) -> bool {
		self == "true"
	}
	fn is_false(&self) -> bool {
		self == "false"
	}
	fn is_null(&self) -> bool {
		self == "nullptr"
	}
	fn is_and(&self) -> bool {
		self == "and"
	}
	fn is_or(&self) -> bool {
		self == "or"
	}
	fn is_not(&self) -> bool {
		self == "not"
	}
	fn is_begin_tmpl(&self) -> bool {
		self == "<"
	}
	fn is_end_tmpl(&self) -> bool {
		self == ">"
	}
	fn is_equality(&self) -> bool {
		self == "<" || self == "<=" || self == ">" || self == ">="
	}
	fn is_relation(&self) -> bool {
		self.is_equality() || self == "!=" || self == "=="
	}
	fn is_new(&self) -> bool {
		self == "new"
	}
	fn is_delete(&self) -> bool {
		self == "delete"
	}
	fn is_from(&self) -> bool {
		self == "from"
	}
	fn is_import(&self) -> bool {
		self == "import"
	}
	fn is_as(&self) -> bool {
		self == "as"
	}
	fn is_return(&self) -> bool {
		self == "return"
	}
	fn is_if_stmt(&self) -> bool {
		self == "if"
	}
	fn is_elif_stmt(&self) -> bool {
		self == "elif"
	}
	fn is_else_stmt(&self) -> bool {
		self == "else"
	}
	fn is_for_stmt(&self) -> bool {
		self == "for"
	}
	fn is_while_stmt(&self) -> bool {
		self == "while"
	}
	fn is_do_stmt(&self) -> bool {
		self == "do"
	}
	fn is_class(&self) -> bool {
		self == "class"
	}
	fn is_enum(&self) -> bool {
		self == "enum"
	}
	fn is_function_def(&self) -> bool {
		self == "function"
	}
	fn is_operator_def(&self) -> bool {
		self == "operator"
	}
	fn is_const(&self) -> bool {
		self == "const"
	}
	fn is_static(&self) -> bool {
		self == "static"
	}
	fn is_volatile(&self) -> bool {
		self == "volatile"
	}
	fn is_location_spec(&self) -> bool {
		self == "eeprom" || self == "flash" || self == "rom"
	}
	fn is_storage_spec(&self) -> bool {
		self.is_const() || self.is_volatile() || self.is_static()
	}
	fn is_none(&self) -> bool {
		self == "none"
	}
	fn is_arrow(&self) -> bool {
		self == "->"
	}
	fn is_visibility(&self) -> bool {
		self == "public" || self == "private" || self == "protected"
	}
	fn is_unsafe(&self) -> bool {
		self == "unsafe"
	}
}
