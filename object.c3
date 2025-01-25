module pdf::object;

import std::collections::list;
import std::collections::map;
import std::io;

import zlib;
import pdf::common;

def ZLibStream = zlib::ZLibStream(<void>);

enum ObjectType
{
	NULL,
	BOOL,
	NUMBER,
	NAME,
	STRING,
	ARRAY,
	DICT,
	STREAM,
	IND_REF,
	KEYWORD,
}

struct PdfObject (Printable)
{
	ObjectType type;
	union as
	{
		void* n;
		bool b;
		PdfNumber number;
		PdfName name;
		PdfString string;
		PdfArray array;
		PdfDict dict;
		PdfStream stream;
		PdfIndRef ref;
		PdfKeyword keyword;
	}
}

const PdfObject PDF_NULL  = {NULL, {.n = null}};
const PdfObject PDF_TRUE  = {BOOL, {.b = true}};
const PdfObject PDF_FALSE = {BOOL, {.b = false}};

macro bool PdfObject.is(self, ObjectType $type) => self.type == $type;
macro PdfObject.get_as(self, ObjectType $type) => self.try_get_as($type)!!;
macro PdfObject.try_get_as(self, ObjectType $type)
{
	if (self.type != $type) return PdfFault.INVALID_OBJ_TYPE?;
	// TODO(bhester): can this be "warn on unmatched type"?
	$switch ($type)
		$case NULL:
			return self.as.n;
		$case BOOL:
			return self.as.b;
		$case NUMBER:
			return self.as.number;
		$case NAME:
			return self.as.name;
		$case STRING:
			return self.as.string;
		$case ARRAY:
			return self.as.array;
		$case DICT:
			return self.as.dict;
		$case STREAM:
			return self.as.stream;
		$case IND_REF:
			return self.as.ref;
		$case KEYWORD:
			return self.as.keyword;
	$endswitch
}

enum NumberType
{
	REAL,
	INTEGER,
}

struct PdfNumber (Printable)
{
	NumberType type;
	bool signed;
	union as
	{
		double real;
		long integer;
	}
}

macro bool PdfNumber.is(self, NumberType $type) => self.type == $type;
macro PdfNumber.get_as(self, NumberType $type) => self.try_get_as($type)!!;
macro PdfNumber.try_get_as(self, NumberType $type)
{
	if (self.type != $type) return PdfFault.INVALID_OBJ_TYPE?;
	// TODO(bhester): can this be "warn on unmatched type"?
	$switch ($type)
		$case INTEGER:
			return self.as.integer;
		$case REAL:
			return self.as.real;
	$endswitch
}

distinct PdfName (Printable) = inline String;
const PdfName CATALOG_NAME      = (PdfName)"Catalog";
const PdfName CONTENTS_NAME     = (PdfName)"Contents";
const PdfName COUNT_NAME        = (PdfName)"Count";
const PdfName DECODEPARMS_NAME  = (PdfName)"DecodeParms";
const PdfName FILTER_NAME       = (PdfName)"Filter";
const PdfName FLATE_DECODE_NAME = (PdfName)"FlateDecode";
const PdfName FONT_NAME         = (PdfName)"Font";
const PdfName KIDS_NAME         = (PdfName)"Kids";
const PdfName LENGTH_NAME       = (PdfName)"Length";
const PdfName PAGE_NAME         = (PdfName)"Page";
const PdfName PAGES_NAME        = (PdfName)"Pages";
const PdfName PREV_NAME         = (PdfName)"Prev";
const PdfName RESOURCES_NAME    = (PdfName)"Resources";
const PdfName ROOT_NAME         = (PdfName)"Root";
const PdfName SUBTYPE_NAME      = (PdfName)"Subtype";
const PdfName TYPE_NAME         = (PdfName)"Type";

// TODO(bhester): when (if) the names get interned, add an initializer method
// that adds all these predefined names. Also add a comment to remind editors
// about adding the name to the initializer method as well. Also need to think
// about how to handle free. If the interning _does_ happen, may want to do some
// sort of reference counting on them so that we can ensure we are only freeing
// it once.

enum StringType
{
	PAREN,
	BRACE,
}

struct PdfString (Printable)
{
	StringType type;
	String text;
}

struct PdfArray (Printable)
{
	List(<PdfObject>) list;
}

struct PdfDict (Printable)
{
	HashMap(<PdfName, PdfObject>) map;
}

struct PdfStream (Printable)
{
	PdfDict dict;
	String contents;
}

struct PdfIndRef (Printable)
{
	ulong obj_num;
	uint gen_num;
}

enum PdfKeyword (Printable) : (String value)
{
	Q_U   = "Q",
	W_U   = "W",
	F_L   = "f",
	N_L   = "n",
	Q_L   = "q",
	BT_UU = "BT",
	ET_UU = "ET",
	TD_UU = "TD",
	TJ_UU = "TJ",
	TL_UU = "TL",
	TC_UL = "Tc",
	TD_UL = "Td",
	TF_UL = "Tf",
	TJ_UL = "Tj",
	TM_UL = "Tm",
	TR_UL = "Tr",
	TS_UL = "Ts",
	TW_UL = "Tw",
	TZ_UL = "Tz",
	GS_LL = "gs",
	RE_LL = "re",
	RG_LL = "rg",
}

fn bool PdfObject.equals(self, PdfObject other)
{
	if (self.type != other.type) return false;
	switch (self.type)
	{
		case NULL:
			return true;
		case BOOL:
			return self.as.b == other.as.b;
		case NUMBER:
			return equals(self.as.number, other.as.number);
		case NAME:
			return equals(self.as.name, other.as.name);
		case STRING:
			return equals(self.as.string, other.as.string);
		case ARRAY:
			return equals(self.as.array, other.as.array);
		case DICT:
			return equals(self.as.dict, other.as.dict);
		case STREAM:
			return equals(self.as.stream, other.as.stream);
		case IND_REF:
			return equals(self.as.ref, other.as.ref);
		case KEYWORD:
			return self.as.keyword == other.as.keyword;
	}
}

fn void PdfObject.free(self, Allocator allocator)
{
	switch (self.type)
	{
		case NULL:
		case BOOL:
		case NUMBER:
		case IND_REF:
		case KEYWORD:
			(void)self; // nothing to free
		case NAME:
			self.as.name.free(allocator);
		case STRING:
			self.as.string.free(allocator);
		case ARRAY:
			self.as.array.free(allocator);
		case DICT:
			self.as.dict.free(allocator);
		case STREAM:
			self.as.stream.free(allocator);
	}
}

fn usz! PdfObject.to_format(&self, Formatter* formatter) @dynamic
{
	switch (self.type)
	{
		case NULL:
			return formatter.print("null");
		case BOOL:
			return formatter.print(self.as.b ? "true" : "false");
		case NUMBER:
			return self.as.number.to_format(formatter);
		case NAME:
			return self.as.name.to_format(formatter);
		case STRING:
			return self.as.string.to_format(formatter);
		case ARRAY:
			return self.as.array.to_format(formatter);
		case DICT:
			return self.as.dict.to_format(formatter);
		case STREAM:
			return self.as.stream.to_format(formatter);
		case IND_REF:
			return self.as.ref.to_format(formatter);
		case KEYWORD:
			return self.as.keyword.to_format(formatter);
	}
}

fn bool PdfNumber.equals(self, PdfNumber other)
{
	if (self.type != other.type) return false;
	switch (self.type)
	{
		case INTEGER:
			return self.as.integer == other.as.integer;
		case REAL:
			return self.as.real == other.as.real;
	}
}

fn usz! PdfNumber.to_format(&self, Formatter* formatter) @dynamic
{
	switch (self.type)
	{
		case INTEGER:
			return formatter.printf("%d", self.as.integer);
		case REAL:
			return formatter.printf("%f", self.as.real);
	}
}

fn usz! PdfName.to_format(&self, Formatter* formatter) @dynamic
{
	// TODO(bhester): unescape characters?
	String s = *self;
	// FIXME(bhester) check on why `printf("/%s", (String)*self)` doesn't work
	return formatter.printf("/%s", s);
}

fn bool PdfString.equals(self, PdfString other)
{
	if (self.type != other.type) return false;
	return equals(self.text, other.text);
}

fn usz! PdfString.to_format(&self, Formatter* formatter) @dynamic
{
	switch (self.type)
	{
		case PAREN:
			return formatter.printf("(%s)", self.text);
		case BRACE:
			usz cnt = 0;
			cnt += formatter.print("<")!;
			foreach (c: self.text)
			{
				char fst = (c >> 4) & 0x0F;
				char snd = (c >> 0) & 0x0F;

				cnt += formatter.printf("%c", fst >= 10 ? (fst - 10 + 'A') : (fst + '0'))!;
				cnt += formatter.printf("%c", snd >= 10 ? (snd - 10 + 'A') : (snd + '0'))!;
			}
			cnt += formatter.print(">")!;
			return cnt;
	}
}

fn void PdfString.free(self, Allocator allocator) => self.text.free(allocator);

fn bool PdfArray.equals(self, PdfArray other)
{
	return equals(self.list, other.list);
}

fn usz! PdfArray.to_format(&self, Formatter* formatter) @dynamic
{
	usz l = self.list.len();
	switch (l)
	{
		case 0:
			return formatter.print("[]");
		case 1:
			return formatter.printf("[%s]", self.list[0]);
		default:
			usz cnt = 0;

			cnt += formatter.print("[")!;
			cnt += formatter.printf("%s", self.list[0])!;
			foreach (d, entry: self.list)
			{
				if (d == 0) continue;
				cnt += formatter.printf(" %s", entry)!;
			};
			cnt += formatter.print("]")!;
			return cnt;
	}
}

fn void PdfArray.free(self, Allocator allocator)
{
	(void)allocator;
	foreach (obj: self.list)
	{
		obj.free(allocator);
	}
	self.list.free();
}

macro bool PdfDict.has_entry(self, PdfName name) => self.map.has_key(name);
macro PdfObject! PdfDict.get_entry(self, PdfName name) => self.map.get(name);
macro PdfObject! PdfDict.get_ind_entry(self, PdfName name, Pdf* pdf)
{
	PdfObject obj = self.get_entry(name)!;
	if (obj.is(IND_REF))
	{
		obj = pdf.get_ref(obj.get_as(IND_REF))!;
	}
	return obj;
}

fn bool PdfDict.equals(self, PdfDict other)
{
	HashMap(<PdfName, PdfObject>) s = self.map;
	HashMap(<PdfName, PdfObject>) o = other.map;

	if (s.len() != o.len()) return false;
	foreach (e: s.iter())
	{
		if (!o.has_key(e.key)) return false;
		if (!equals(e.value, o.get(e.key)!!)) return false;
	}
	return true;
}

fn usz! PdfDict.to_format(&self, Formatter* formatter) @dynamic
{
	if (self.map.is_empty())
	{
		return formatter.print("<<>>");
	}
	else
	{
		usz cnt = 0;

		cnt += formatter.print("<<")!;
		self.map.@each(; PdfName name, PdfObject value)
		{
			cnt += formatter.printf("%s %s", name, value)!;
		};
		cnt += formatter.print(">>")!;
		return cnt;
	}
}

fn void PdfDict.free(self, Allocator allocator)
{
	self.map.@each(; PdfName name, PdfObject entry)
	{
		name.free(allocator);
		entry.free(allocator);
	};
	self.map.free();
}

fn bool PdfStream.equals(self, PdfStream other)
{
	// TODO(bhester): verify this is a correct optimization
	return self.contents.ptr == other.contents.ptr;
}

fn usz! PdfStream.to_format(&self, Formatter* formatter) @dynamic
{
	return formatter.printf("%s stream ... endstream", self.dict);
}

fn void PdfStream.free(self, Allocator allocator) => self.dict.free(allocator);

fn usz! PdfIndRef.to_format(&self, Formatter* formatter) @dynamic
{
	return formatter.printf("%d %d R", self.obj_num, self.gen_num);
}

fn usz! PdfKeyword.to_format(&self, Formatter* formatter) @dynamic
{
	return formatter.printf("%s", self.value);
}

fn bool PdfIndRef.equals(self, PdfIndRef other) => self.obj_num == other.obj_num && self.gen_num == other.gen_num;

macro try_read_expecting(ObjectType $type, String contents, Pdf* pdf, usz* len = null, anyfault $fault = PdfFault.INVALID_OBJ_TYPE)
{
	PdfObject! obj = read_from(contents, pdf, len);
	if (try obj)
	{
		if (obj.type == $type)
		{
			return obj.get_as($type);
		}
		// wrong type, free resources
		obj.free(pdf.allocator);
	}
	return $fault?;
}

<*
 Reads a PdfObject from the string starting at the given offset. Sets the
 value of len to the length of the object's text representation if it is
 passed.

 @require pdf != null
*>
fn PdfObject! read_from(String contents, Pdf* pdf, usz* len = null)
{
	if (!contents.len) return PdfFault.END_OF_FILE?;

	char c = contents[0];
	switch (c)
	{
		case '/':
			return {NAME, {.name = read_name_at(contents, pdf.allocator, len)}};
		case '[':
			return {ARRAY, {.array = read_array_at(contents, pdf, len)}};
		case '(':
			return {STRING, {.string = read_pstring_at(contents, pdf.allocator, len)}};
		case '<':
			if (contents.len < 2) return PdfFault.END_OF_FILE?;
			if (contents[1] == '<')
			{
				usz dict_len;
				PdfDict dict = read_dict_at(contents, pdf, &dict_len)!;
				defer catch dict.free(pdf.allocator);

				String after_obj = contents[dict_len..];
				common::@skip_whitespace(after_obj);

				if (after_obj.starts_with("stream"))
				{
					usz stream_len;
					PdfStream stream = read_stream_at(after_obj, dict, pdf, &stream_len)!;

					if (len != null) *len = stream_len + (after_obj.ptr - contents.ptr);
					return {STREAM, {.stream = stream}};
				}
				else
				{
					if (len != null) *len = dict_len;
					return {DICT, {.dict = dict}};
				}
			}
			return {STRING, {.string = read_bstring_at(contents, pdf.allocator, len)}};
		default:
			if (contents.starts_with("null"))
			{
				if (len != null) *len = 4;
				return PDF_NULL;
			}
			else if (contents.starts_with("true"))
			{
				if (len != null) *len = 4;
				return PDF_TRUE;
			}
			else if (contents.starts_with("false"))
			{
				if (len != null) *len = 5;
				return PDF_FALSE;
			}
			else if (c.is_numeric_char())
			{
				usz num_len;
				PdfNumber num = read_number_at(contents, &num_len)!;

				if (num.type == INTEGER && !num.signed)
				{
					String after_num = contents[num_len..];
					common::@skip_whitespace(after_num);

					usz ref_len;
					PdfIndRef! ref = cont_read_ind_ref_at(after_num, num, &ref_len);
					if (try ref)
					{
						if (len != null) *len = after_num[ref_len..].ptr - contents.ptr;
						return {IND_REF, {.ref = ref}};
					}
				}

				if (len != null) *len = num_len;
				return {NUMBER, {.number = num}};
			}

			PdfKeyword! kwd = try_read_keyword(contents);
			if (catch kwd)
			{
				return PdfFault.UNEXPECTED_CHARACTER?;
			}

			if (len != null) *len = kwd.value.len;
			return {KEYWORD, {.keyword = kwd}};
	}
}

fn PdfNumber! read_number_at(String contents, usz* len)
{
	bool digit = false;
	bool decimal = false;
	bool signed = false;
	bool negative = false;

	// TODO(bhester): decide what to do about types here
	ulong res = 0;
	ulong den = 1;

	char c;
	usz ind = 0;
	while (ind < contents.len && (c = contents[ind]).is_numeric_char())
	{
		if (c == '.')
		{
			if (decimal) break;
			decimal = true;
		}
		else if (c == '+' || c == '-')
		{
			if (digit || signed) break;
			signed = true;

			if (c == '-') negative = true;
		}
		else
		{
			digit = true;

			if (decimal) den *= 10;
			res = (10 * res) + (ulong)(c - '0');
		}
		ind += 1;
	}

	if (!digit && ind >= contents.len) return PdfFault.END_OF_FILE?;
	if (!digit) return PdfFault.UNEXPECTED_CHARACTER?;

	if (len != null) *len = ind;

	// TODO(bhester): check the number fits in the bounds
	long signed_res = negative ? -res : res;
	if (decimal)
	{
		return {REAL, signed, {.real = (double)signed_res / (double)den}};
	}
	else
	{
		return {INTEGER, signed, {.integer = signed_res}};
	}
}

fn PdfName! read_name_at(String contents, Allocator allocator, usz* len)
{
	// TODO(bhester): work on interning the strings!
	common::@assert_starts_with(contents, '/');
	@pool()
	{
		DString tmp = dstring::temp_new();

		char c;
		usz ind = 0;
		while (ind < contents.len && (c = contents[ind]).is_name_char())
		{
			if (c == '#')
			{
				if (ind + 2 >= contents.len) return PdfFault.END_OF_FILE?;
				if (!(contents[ind + 1].is_name_char() && contents[ind + 2].is_name_char()))
				{
					return PdfFault.UNEXPECTED_CHARACTER?;
				}

				char escaped = contents[1:2].hex_to_char()!;
				tmp.append(escaped);

				ind += 3;
			}
			else
			{
				tmp.append(c);
				ind += 1;
			}
		}

		if (len != null) *len = 1 /* slash */ + ind;
		return (PdfName)tmp.copy_str(allocator);
	};
}

fn PdfArray! read_array_at(String contents, Pdf* pdf, usz* len)
{
	Allocator allocator = pdf.allocator;
	String cur_contents = contents;

	List(<PdfObject>) res;
	res.new_init(allocator: allocator);
	defer catch
	{
		foreach (obj: res)
		{
			obj.free(allocator);
		}
		res.free();
	}

	common::@assert_starts_with(cur_contents, '[');
	while (true)
	{
		common::@skip_whitespace(cur_contents);
		if (!cur_contents.len) return PdfFault.END_OF_FILE?;

		if (cur_contents[0] == ']')
		{
			break;
		}

		usz obj_len;
		PdfObject obj = read_from(cur_contents, pdf, &obj_len)!;
		res.push(obj);

		cur_contents = cur_contents[obj_len..];
	}

	if (len != null)
	{
		common::@assert_starts_with(cur_contents, ']');
		*len = cur_contents.ptr - contents.ptr;
	}
	return {res};
}

fn PdfString! read_pstring_at(String contents, Allocator allocator, usz* len)
{
	common::@assert_starts_with(contents, '(', false);
	@pool()
	{
		DString tmp = dstring::temp_new();

		char c;
		uint paren_stack = 0;
		usz ind = 1; // open paren
		while (ind < contents.len &&
			   !((c = contents[ind]) == ')' && paren_stack == 0))
		{
			if (c == '\\')
			{
				if (ind + 1 >= contents.len) return PdfFault.END_OF_FILE?;

				char next = contents[ind + 1];
				switch (next)
				{
					case 'n':
						tmp.append('\n');
						ind += 2;
					case 'r':
						tmp.append('\r');
						ind += 2;
					case 't':
						tmp.append('\t');
						ind += 2;
					case 'b':
						tmp.append('\b');
						ind += 2;
					case 'f':
						tmp.append('\f');
						ind += 2;
					case '(':
						tmp.append('(');
						ind += 2;
					case ')':
						tmp.append(')');
						ind += 2;
					case '\\':
						tmp.append('\\');
						ind += 2;
					default:
						// ending a line with a backslash means to EXCLUDE the line
						// ending in the string
						if (next == '\n')
						{
							// checking for a LF ending
							ind += 2;
							continue;
						}
						else if (next == '\r')
						{
							// checking for a CRLF or just a CR ending
							if (ind + 2 < contents.len &&
								contents[ind + 2] == '\n')
							{
								ind += 3;
							}
							else
							{
								ind += 2;
							}
							continue;
						}
						else if (next.is_octal_char())
						{
							char to_add;

							char octal_ch;
							usz octal_count = 0;
							while (octal_count < 3 &&
								   ind + octal_count < contents.len &&
								   (octal_ch = contents[ind + 1 + octal_count]).is_octal_char())
							{
								to_add = (8 * to_add) + (octal_ch - '0');
								octal_count += 1;
							}

							tmp.append(to_add);
							ind += 1 /* slash */ + octal_count;
							continue;
						}
				} // switch (next)
			} // if (c == '\\')
			else if (c == '\r')
			{
				// line breaks in the string should be replaced with a single '\n'
				// regardless of the line ending used
				// TODO(bhester): validate this is true even for CR (mac) line
				// endings
				if (ind + 1 < contents.len && contents[ind + 1] == '\n')
				{
					ind += 2;
				}
				else
				{
					ind += 1;
				}
				tmp.append('\n');
			}
			else
			{
				if (c == '(') ++paren_stack;
				if (c == ')') --paren_stack;

				tmp.append(c);
				ind += 1;
			}
		}

		if (paren_stack != 0) return PdfFault.END_OF_FILE?;
		if (c != ')') return PdfFault.UNEXPECTED_CHARACTER?;

		if (len != null) *len = ind + 1 /* close paren */;
		return {PAREN, tmp.copy_str(allocator)};
	};
}

fn PdfString! read_bstring_at(String contents, Allocator allocator, usz* len)
{
	common::@assert_starts_with(contents, '<', false);
	@pool()
	{
		DString tmp = dstring::temp_new();

		String cur_contents = contents[1..]; // open brace

		char c;
		while (cur_contents.len != 0 && (c = cur_contents[0]) != '>')
		{
			String after_first = cur_contents[1..];
			common::@skip_whitespace(after_first);

			if (!after_first.len) return PdfFault.END_OF_FILE?;

			bool early_close = after_first[0] == '>';
			if (early_close)
			{
				char[*] char_arr = {c, '0'};
				String char_str = (String)&char_arr;
				tmp.append(char_str.hex_to_char())!;

				cur_contents = after_first;
				break;
			}
			else
			{
				char[*] char_arr = {c, after_first[0]};
				String char_str = (String)&char_arr;
				tmp.append(char_str.hex_to_char())!;

				after_first = after_first[1..];
				common::@skip_whitespace(after_first);

				cur_contents = after_first;
			}
		}

		if (cur_contents.len == 0) return PdfFault.END_OF_FILE?;
		common::@assert_starts_with(cur_contents, '>');

		if (len != null) *len = cur_contents.ptr - contents.ptr;
		return {BRACE, tmp.copy_str(allocator)};
	};
}

fn PdfDict! read_dict_at(String contents, Pdf* pdf, usz* len)
{
	Allocator allocator = pdf.allocator;
	String cur_contents = contents;

	HashMap(<PdfName, PdfObject>) res;
	res.new_init(allocator: allocator);
	defer catch
	{
		res.@each(; PdfName name, PdfObject entry)
		{
			name.free(allocator);
			entry.free(allocator);
		};
		res.free();
	}

	common::@assert_starts_with(cur_contents, "<<");
	while (true)
	{
		common::@skip_whitespace(cur_contents);
		if (!cur_contents.len) return PdfFault.END_OF_FILE?;

		if (cur_contents.starts_with(">>"))
		{
			break;
		}

		usz obj_len;
		PdfName name = try_read_expecting(NAME, cur_contents, pdf, &obj_len, PdfFault.EXPECTED_NAME)!;

		cur_contents = cur_contents[obj_len..];
		common::@skip_whitespace(cur_contents);

		PdfObject value_obj = read_from(cur_contents, pdf, &obj_len)!;

		res.set(name, value_obj);

		cur_contents = cur_contents[obj_len..];
	}

	if (len != null)
	{
		common::@assert_starts_with(cur_contents, ">>");
		*len = cur_contents.ptr - contents.ptr;
	}
	return {res};
}

fn PdfStream! read_stream_at(String contents, PdfDict dict, Pdf* pdf, usz* len)
{
	String after_obj = contents;

	common::@assert_starts_with(after_obj, "stream");
	if (after_obj.len < 9 /* "endstream" len */) return PdfFault.END_OF_FILE?;

	if (after_obj.starts_with("\r")) after_obj = after_obj[1..];
	if (after_obj.starts_with("\n")) after_obj = after_obj[1..];

	PdfObject len_obj = dict.get_entry(LENGTH_NAME)!;
	if (len_obj.is(IND_REF))
	{
		len_obj = pdf.get_ref(len_obj.get_as(IND_REF))!;
	}
	usz stream_len = len_obj.get_as(NUMBER).get_as(INTEGER);
	String stream_contents = after_obj[:stream_len];

	String after_stream = after_obj[stream_len..];
	common::@skip_whitespace(after_stream);
	common::@assert_starts_with(after_stream, "endstream");

	if (len != null) *len = after_stream.ptr - contents.ptr;
	return {dict, stream_contents};
}

<*
 @require num.type == INTEGER
 @require !num.signed
*>
fn PdfIndRef! cont_read_ind_ref_at(String contents, PdfNumber num, usz* len)
{
	if (!contents.len) return PdfFault.END_OF_FILE?;
	if (!contents[0].is_digit()) return PdfFault.UNEXPECTED_CHARACTER?;
	// this must be a digit, the gen number cannot be signed
	// TODO(bhester): verify this

	ulong obj_num = num.as.integer;
	uint gen_num = 0;

	char c;
	usz ind = 0;
	while (ind < contents.len && (c = contents[ind]).is_digit())
	{
		gen_num = (10 * gen_num) + (c - '0');
		ind += 1;
	}

	if (ind >= contents.len) return PdfFault.END_OF_FILE?;
	if (!contents[ind].is_pdf_space()) return PdfFault.UNEXPECTED_CHARACTER?;

	String after_gen = contents[ind..];
	common::@skip_whitespace(after_gen);

	if (ind >= after_gen.len) return PdfFault.END_OF_FILE?;
	if (!after_gen.starts_with("R")) return PdfFault.UNEXPECTED_CHARACTER?;

	if (len != null) *len = after_gen[1..].ptr - contents.ptr;
	return {obj_num, gen_num};
}

fn uint PdfIndRef.hash(self)
{
	const uint PRIME = 59;

	uint result = 1;
	result = (result * PRIME) + self.obj_num.hash();
	result = (result * PRIME) + self.gen_num.hash();

	return result;
}

fn PdfKeyword! try_read_keyword(String contents)
{
	$foreach ($x : PdfKeyword.values)
		if (contents.starts_with($x.value)) return $x;
	$endforeach
	return SearchResult.MISSING?;
}

macro bool char.is_numeric_char(self) => self.is_digit() || self == '+' || self == '-' || self == '.';
