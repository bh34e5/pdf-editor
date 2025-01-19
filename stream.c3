<*
 Utils for the stream object type, such as decoding.
*>
module pdf::object::stream;

import std::io;

<*
 Returns a copy of the contents. The caller is expected to free it.
*>
fn char[]! PdfStream.get_contents(self, Pdf* pdf)
{
	PdfDict d = self.dict;
	char[] result = self.contents;

	PdfObject! filters = d.get_entry(object::FILTER_NAME);
	if (catch e = filters)
	{
		if (e == SearchResult.MISSING)
		{
			// there are no filters, just return the stream contents
			char[] copy = allocator::new_array(pdf.allocator, char, result.len);
			copy[0:result.len] = result[0:result.len];
			return copy;
		}
		return e?; // rethrow
	}

	PdfObject! decode_parms = d.get_entry(object::DECODEPARMS_NAME);

	@pool()
	{
		if (filters.is(ObjectType.ARRAY))
		{
			PdfArray filters_arr = filters.get_as(ObjectType.ARRAY);
			PdfArray! decode_arr = decode_parms.get_as(ObjectType.ARRAY);
			foreach (i, filter: filters_arr.list)
			{
				PdfName filter_name = filter.get_as(ObjectType.NAME);
				PdfObject parm = @ok(decode_arr) ? decode_arr!.list[i] : object::PDF_NULL;
				result = decode_stream(result, filter_name, parm, allocator::temp())!;
			}
		}
		else
		{
			PdfName filter_name = filters.get_as(ObjectType.NAME);
			PdfObject parm = decode_parms ?? object::PDF_NULL;
			result = decode_stream(result, filter_name, parm, allocator::temp())!;
		}

		io::printf("after that: %s", result);

		char[] copy = allocator::new_array(pdf.allocator, char, result.len);
		mem::copy(copy.ptr, result.ptr, result.len);
		return copy;
	};
}

module pdf::object::stream @private;

fn char[]! decode_stream(char[] encoded, PdfName filter_name, PdfObject parm, Allocator allocator)
{
	switch
	{
		case equals(filter_name, object::FLATE_DECODE_NAME):
			if (!equals(parm, object::PDF_NULL))
			{
				unsupported("DecodeParms is currently unsupported");
			}

			ZLibStream zstream;
			zstream.new_init(allocator: allocator)!;
			defer zstream.free();

			return zstream.inflate_all(encoded, allocator: allocator);
		default:
			unsupported("Unsupported stream filter");
	}
}
