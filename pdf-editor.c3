module pdf::editor;

import std::io;

import pdf::object;

fn int main(String[] args)
{
	TrackingAllocator tracked;
	tracked.init(allocator::heap());
	defer
	{
		tracked.print_report();
		tracked.free();
	}

	Allocator allocator = &tracked;
	mem::@scoped(allocator)
	{
		return wrapper(args);
	};
}

fn int wrapper(String[] args, Allocator allocator = allocator::heap())
{
	if (args.len < 2)
	{
		io::eprintn("Expected filename as arguments");
		return 1;
	}

	String filename = args[1];
	Pdf* pdf = pdf::read_new(filename, allocator)!!;
	defer
	{
		pdf.cleanup();
		allocator::free(allocator, pdf);
	}

	io::printfn(
		"Version{.major = %d, .minor = %d}",
		pdf.version.major,
		pdf.version.minor);

	usz xref_ind = 0;
	XRef* cur_xref = &pdf.xref;
	while (cur_xref != null)
	{
		io::printfn("XRef %d has %d sections:", xref_ind, cur_xref.sections.len);
		foreach (i, section: cur_xref.sections)
		{
			io::printfn("\tSection[%d] has %d entries;", i, section.entries.len);
		}

		xref_ind += 1;
		cur_xref = cur_xref.prev;
	}

	// 109466
	// 109477
	PdfIndRef target_ref = {1354, 0};
	PdfStream stm = pdf.get_ref(target_ref).get_as(ObjectType.STREAM)!!;

	char[] stream_contents = stm.get_contents(pdf)!!;
	defer
	{
		allocator::free(allocator, stream_contents.ptr);
		stm.free(allocator);
	}

	io::printfn("The contents were:\n%s", (String)stream_contents);

	return 0;
}
