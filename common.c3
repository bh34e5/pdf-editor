module pdf::common;

<*
 @require values::@is_lvalue(#s)
*>
macro @assert_starts_with(#s, $target, bool $proceed = true)
{
	var s = #s;
	$if $proceed:
		defer #s = s;
	$endif

	$switch (@typeid($target))
		$case String.typeid:
			if (!s.starts_with($target))
			{
				abort();
			}
			s = s[$target.len..];
		$case char.typeid:
			if (s[0] != $target)
			{
				abort();
			}
			s = s[1..];
		$default:
			$error "Can only assert String or char types";
	$endswitch
}

<*
 @require values::@is_lvalue(#s)
*>
macro @skip_whitespace(#s)
{
	String s = #s;

	usz ind = 0;
	while (ind < s.len && s[ind].is_pdf_space()) ++ind;

	#s = s[ind..];
}

macro bool char.is_pdf_space(self)
{
	switch (self)
	{
		case 0x00: // nul
		case 0x09: // tab
		case 0x0A: // line feed
		case 0x0C: // form feed
		case 0x0D: // carriage return
		case 0x20: // space
			return true;
		default:
			return false;
	}
}

macro bool char.is_pdf_delimeter(self)
{
	switch (self)
	{
		case '(':
		case ')':
		case '<':
		case '>':
		case '[':
		case ']':
		case '{':
		case '}':
		case '/':
		case '%':
			return true;
		default:
			return false;
	}
}

macro bool char.is_name_char(self) => !(self.is_pdf_space() || self.is_pdf_delimeter());

macro int! @read_int(String #s, bool $proceed = true) => @read_num(int, #s, $proceed);
macro long! @read_long(String #s, bool $proceed = true) => @read_num(long, #s, $proceed);

<*
 @require values::@is_lvalue(#s)
*>
macro @read_num($Type, String #s, bool $proceed = true)
{
	bool found_digit = false;

	$Type res = 0;
	usz i = 0;

	String s = #s;
	$if $proceed:
		defer #s = s[i..];
	$endif

	while (i < s.len && s[i].is_pdf_space())
	{
		++i;
	}

	if (i >= s.len)
	{
		if (!found_digit) return PdfFault.INVALID_NUMBER?;
		return res;
	}

	char c;
	while (i < s.len && (c = s[i]).is_digit())
	{
		found_digit = true;

		$Type val = ($Type)(c - '0');
		res = (10 * res) + val;
		++i;
	}

	if (!found_digit) return PdfFault.INVALID_NUMBER?;
	return res;
}

<*
 @require start < s.len
*>
macro usz String.line_start_from(s, usz start)
{
	while (start > 0)
	{
		char c = s[start - 1];
		if (c == '\r' || c == '\n')
		{
			break;
		}
		--start;
	}
	return start;
}

macro bool char.is_octal_char(self) => ('0' <= self && self <= '7');

macro bool char.is_hex_char(self)
{
	switch
	{
		case (self.is_digit()):
		case ('a' <= self && self <= 'f'):
		case ('A' <= self && self <= 'F'):
			return true;
		default:
			return false;
	}
}

macro char! String.hex_to_char(self) => self.hex_to_num(char);

macro String.hex_to_num(self, $Type)
{
	$Type res = 0;
	foreach (c: self)
	{
		if (!c.is_hex_char()) return PdfFault.UNEXPECTED_CHARACTER?;

		char val = {|
			if (c.is_digit()) { return c - '0'; }
			else if ('a' <= c && c <= 'f') { return 10 + (c - 'a'); }
			else if ('A' <= c && c <= 'F') { return 10 + (c - 'A'); }
			unreachable();
		|};
		res = (16 * res) + val;
	}
	return res;
}
