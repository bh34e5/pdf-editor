module pdf;

import std::io::file;
import std::collections::map;

fault PdfFault
{
	INVALID_XREF,
	INVALID_NUMBER,
	END_OF_FILE,
	UNEXPECTED_CHARACTER,
	EXPECTED_NAME,
	INVALID_OBJ_TYPE,
	INVALID_IND_REF,
}

struct Pdf {
	Allocator allocator;
	String contents;
	Version version;
	XRef xref;
	PdfDict catalog;

	HashMap(<PdfIndRef, PdfObject>) objects;
}

struct Version
{
	uint major;
	uint minor;
}

struct XRef
{
	XRefSection[] sections;
	PdfDict trailer;
	XRef* prev;
}

struct XRefSection
{
	ulong base_entry;
	XRefEntry[] entries;
}

enum XRefEntryType
{
	USED,
	FREE,
}

struct XRefEntry
{
	XRefEntryType type;
	union as
	{
		struct used
		{
			ulong offset;
			uint gen_num;
		}

		struct free
		{
			ulong next_free;
			uint next_gen_num;
		}
	}
}

distinct PdfPage = inline PdfDict;

fn Pdf*! read_new(String filename, Allocator allocator = allocator::heap())
{
	String contents = (String)file::load_new(filename, allocator)!;
	defer catch contents.free(allocator);

	Pdf* pdf = allocator::new(allocator, Pdf);
	defer catch
	{
		foreach (val: pdf.objects.value_iter())
		{
			val.free(allocator);
		}
		pdf.objects.free();
		pdf.xref.cleanup(allocator);
	}

	pdf.allocator = allocator;
	pdf.contents = contents;
	pdf.objects.new_init(allocator: allocator);

	pdf.version = get_version(contents)!;
	pdf.xref = read_xref_new(contents, pdf, allocator)!;

	PdfDict trailer_dict = pdf.xref.trailer;
	PdfIndRef catalog_ref = trailer_dict.get_entry(object::ROOT_NAME).get_as(IND_REF)!;

	pdf.catalog = pdf.get_ref(catalog_ref).get_as(DICT)!;

	// TODO(bhester): validate that all the fields have been set properly?
	return pdf;
}

fn void Pdf.cleanup(self)
{
	foreach (val: self.objects.value_iter())
	{
		val.free(self.allocator);
	}
	self.objects.free();

	// NOTE(bhester): no need to cleanup up the catalog, it was an indirect
	// reference, so it gets cleaned up in the loop above
	// self.catalog.free(self.allocator);
	self.xref.cleanup(self.allocator);
	self.contents.free(self.allocator);
}

macro Pdf.get_ref_as(&self, ObjectType $type, PdfIndRef ref) => self.get_ref(ref).get_as($type);

<*
 @require self.contents.len > 0
*>
fn PdfObject! Pdf.get_ref(&self, PdfIndRef ref)
{
	if (self.objects.has_key(ref)) return self.objects.get(ref)!!;

	XRef* xref = &self.xref;
	while FIND_REF: (xref != null)
	{
		foreach (section: xref.sections)
		{
			ulong ind;
			if (section.base_entry <= ref.obj_num &&
			    (ind = ref.obj_num - section.base_entry) < section.entries.len)
			{
				XRefEntry entry = section.entries[ind];
				if (entry.type == XRefEntryType.USED &&
				    ref.gen_num == entry.as.used.gen_num)
				{
					usz offset = entry.as.used.offset;
					String text = self.contents[offset..];

					ulong obj_num = common::@read_num(ulong, text)!;
					common::@skip_whitespace(text);
					uint gen_num = common::@read_num(uint, text)!;
					common::@skip_whitespace(text);
					common::@assert_starts_with(text, "obj");
					common::@skip_whitespace(text);

					if (!(obj_num == ref.obj_num && gen_num == ref.gen_num))
					{
						return PdfFault.INVALID_XREF?;
					}

					PdfObject object = object::read_from(text, self)!;
					self.objects.set(ref, object);

					return object;
				}
				// we are in the "newest" xref, so if we see it here, but it's
				// invalid, we should stop looking for it.
				break FIND_REF;
			}
		}
		xref = xref.prev;
	}
	return PdfFault.INVALID_IND_REF?;
}

fn uint Pdf.page_count(&self)
{
	PdfIndRef page_ref = self.catalog.get_entry(object::PAGES_NAME).get_as(IND_REF)!!;
	PdfDict pages_root = self.get_ref(page_ref).get_as(DICT)!!;

	PdfObject count_obj = pages_root.get_entry(object::COUNT_NAME)!!;
	if (count_obj.is(IND_REF))
	{
		count_obj = self.get_ref(count_obj.get_as(IND_REF))!!;
	}
	usz count_num = count_obj.get_as(NUMBER).get_as(INTEGER);

	assert(count_num < uint.max);
	return (uint)count_num;
}

fn PdfPage! Pdf.get_page(&self, usz page_ind)
{
	PdfIndRef page_ref = self.catalog.get_entry(object::PAGES_NAME).get_as(IND_REF)!!;
	PdfDict pages_root = self.get_ref(page_ref).get_as(DICT)!!;

	PdfArray kids = pages_root.get_ind_entry(object::KIDS_NAME, self).get_as(ARRAY)!;
	usz kids_len = kids.list.len();

	PdfPageTree tree = {kids_len, pages_root};
	while (tree.count > 0)
	{
		usz kids_ind = 0;
		while (kids_ind < kids_len)
		{
			PdfPageTree kid_tree = page_tree_kid_ind(tree.page, kids_ind, self)!;
			usz kid_count = kid_tree.count;

			if ((page_ind == 0 && kid_count == 0) || (page_ind < kid_count))
			{
				tree = kid_tree;
				break;
			}
			else
			{
				page_ind -= kid_count ?: 1;
				kids_ind += 1;
			}
		}
	}
	return (PdfPage)tree.page;
}

module pdf @private;

import std::collections::list;

fn Version! get_version(String contents)
{
	common::@assert_starts_with(contents, "%PDF-");

	int major = common::@read_int(contents)!;
	common::@assert_starts_with(contents, '.');
	int minor = common::@read_int(contents)!;

	assert(major >= 0);
	assert(minor >= 0);

	return {major, minor};
}

fn void XRef.cleanup(self, Allocator allocator)
{
	if (self.prev != null) self.prev.cleanup(allocator);

	self.trailer.free(allocator);
	foreach (section: self.sections)
	{
		allocator::free(allocator, section.entries.ptr);
	}
	allocator::free(allocator, self.sections.ptr);
	allocator::free(allocator, self.prev);
}

fn XRef! read_xref_new(String contents, Pdf* pdf, Allocator allocator)
{
	usz eof_ind = last_eof(contents)!;
	assert(eof_ind > 0);

	usz end_of_p_line;
	if (contents[eof_ind - 1] == '\n')
	{
		if (eof_ind > 1 && contents[eof_ind - 2] == '\r')
		{
			end_of_p_line = eof_ind - 2;
		}
		else
		{
			end_of_p_line = eof_ind - 1;
		}
	}
	else
	{
		assert(contents[eof_ind - 1] == '\r');
		end_of_p_line = eof_ind - 1;
	}

	usz offset_ind = contents.line_start_from(end_of_p_line);
	int num = common::@read_int(contents[offset_ind..], false) ?? PdfFault.INVALID_XREF?!;
	if (num < 0) return PdfFault.INVALID_XREF?;

	return read_xref_inner(contents, num, pdf, allocator);
}

fn XRef! read_xref_inner(String contents, usz offset, Pdf* pdf, Allocator allocator)
{
	@pool()
	{
		List(<XRefSection>) section_list;
		section_list.temp_init();
		defer catch {
			foreach (section: section_list)
			{
				allocator::free(allocator, section.entries.ptr);
			}
		}

		String text = contents[offset..];
		common::@assert_starts_with(text, "xref");
		common::@skip_whitespace(text);

		while (text.len && text[0].is_digit())
		{
			List(<XRefEntry>) entry_list;
			entry_list.temp_init();

			int entry_start = common::@read_int(text)!;
			common::@assert_starts_with(text, ' ');
			int entry_count = common::@read_int(text)!;
			common::@skip_whitespace(text);

			for (int i = 0; i < entry_count; ++i)
			{
				long num_one = common::@read_long(text[0..], false)!;
				int num_two = common::@read_int(text[11..], false)!;
				char t = text[17];

				text = text[20..];

				XRefEntry entry;
				switch (t)
				{
					case 'n':
						entry = {
							USED,
							{
								.used = {
									.offset = num_one,
									.gen_num = num_two}}};
					case 'f':
						entry = {
							FREE,
							{
								.free = {
									.next_free = num_one,
									.next_gen_num = num_two}}};
					default:
						return PdfFault.INVALID_XREF?;
				}

				entry_list.push(entry);
			}

			XRefSection section = {
				.base_entry = entry_start,
				.entries = entry_list.to_new_array(allocator)};

			section_list.push(section);
		}

		common::@assert_starts_with(text, "trailer");
		common::@skip_whitespace(text);

		PdfDict trailer =
			object::try_read_expecting(DICT, text, pdf) ??
			PdfFault.INVALID_XREF?!;

		// TODO(bhester): check the number of entries matches the number given
		// in the trailer?

		PdfObject! entry = trailer.get_entry(object::PREV_NAME);
		if (try entry)
		{
			PdfNumber num = entry.try_get_as(NUMBER) ?? PdfFault.INVALID_XREF?!;
			usz prev_offset = num.try_get_as(INTEGER) ?? PdfFault.INVALID_XREF?!;

			XRef prev_xref = read_xref_inner(contents, prev_offset, pdf, allocator)!;
			return {
				section_list.to_new_array(allocator),
				trailer,
				allocator::clone(allocator, prev_xref)};
		}
		return {section_list.to_new_array(allocator), trailer, null};
	};
}

fn usz! last_eof(String s)
{
	bool found = false;
	usz ind = s.len;
	while (ind > 0)
	{
		char c = s[ind - 1];
		if (c == '\r' || c == '\n')
		{
			if (s[ind..].starts_with("%%EOF"))
			{
				found = true;
				break;
			}
		}
		--ind;
	}

	if (!found) return SearchResult.MISSING?;
	return ind;
}

struct PdfPageTree
{
	usz count;
	PdfDict page;
}

fn PdfPageTree! page_tree_kid_ind(PdfDict page_root, usz ind, Pdf* pdf)
{
	PdfName type = page_root.get_entry(object::TYPE_NAME).get_as(NAME)!;
	assert(equals(type, object::PAGES_NAME));

	PdfArray kids = page_root.get_ind_entry(object::KIDS_NAME, pdf).get_as(ARRAY)!;
	usz kids_len = kids.list.len();
	assert(ind < kids_len);

	PdfIndRef kid_ref = kids.list[ind].get_as(IND_REF);
	PdfDict kid = pdf.get_ref_as(DICT, kid_ref)!;
	PdfName kid_type = kid.get_entry(object::TYPE_NAME).get_as(NAME)!;

	PdfPageTree res;
	if (equals(kid_type, object::PAGE_NAME))
	{
		res.count = 0;
		res.page = kid;
	}
	else
	{
		PdfNumber count_num = kid.get_ind_entry(object::COUNT_NAME, pdf).get_as(NUMBER)!;
		ulong count = count_num.get_as(INTEGER);

		res.count = (usz)count; // TODO(bhester): safe cast??
		res.page = kid;
	}
	return res;
}
