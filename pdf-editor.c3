module pdf::editor;

import pdf::object;
import pdf::render;

import raygui::rg;
import raylib55::rl;

import std::io;

const char* GUI_TITLE = "PDF Editor";

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
		return args.len < 2 ? test_gui() : test_pdf(args[1]);
	};
}

fn int test_pdf(String filename, Allocator allocator = allocator::heap())
{
	Pdf* pdf = pdf::read_new(filename, allocator)!!;
	defer
	{
		pdf.cleanup();
		allocator::free(allocator, pdf);
	}

	Version v = pdf.version;
	io::printfn("Version{.major = %d, .minor = %d}", v.major, v.minor);

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
	PdfStream stm = pdf.get_ref(target_ref).get_as(STREAM)!!;

	char[] stream_contents = stm.get_contents(pdf)!!;
	defer allocator::free(allocator, stream_contents.ptr);

	io::printfn("The contents were:\n%s", (String)stream_contents);

	return 0;
}

struct PdfState
{
	Pdf* pdf;
	uint page;
	uint total_pages;
	// TODO(bhester): maybe write the pages to textures so that once they've
	// been rendered once, I don't have to do it again...
}

fn int test_gui(Allocator allocator = allocator::heap())
{
	rl::initWindow(800, 600, GUI_TITLE);
	defer rl::closeWindow();

	GuiWindowFileDialogState dialog_state = rg::initGuiWindowFileDialog();
	String pdf_filter = ".pdf";
	mem::copy(&dialog_state.filterExt[0], pdf_filter.ptr, pdf_filter.len);
	dialog_state.filterExt[pdf_filter.len] = '\0';

	// increase font size
	rg::guiSetStyle(GuiControl.DEFAULT, GuiDefaultProperty.TEXT_SIZE, 16);

	rl::setTargetFps(60);

	PdfState pdf_state;
	defer
	{
		if (pdf_state.pdf != null)
		{
			pdf_state.pdf.cleanup();
			allocator::free(allocator, pdf_state.pdf);
		}
	}

	while (!rl::windowShouldClose())
	{
		if (dialog_state.selectFilePressed)
		{
			defer dialog_state.selectFilePressed = false;

			io::printn("Opening file!");

			char[] filename = &dialog_state.fileNameText;
			Pdf*! opened = pdf::read_new((String)filename, allocator);

			if (try opened)
			{
				uint total_pages = pdf::page_count(opened);

				pdf_state = {opened, 0, total_pages};
				rl::setWindowTitle(filename.ptr);
			}
			else
			{
				// TODO(bhester): display error message
			}
		}

		rl::beginDrawing();
		defer rl::endDrawing();

		rl::clearBackground({0, 0, 0x20, 0xFF});

		if (pdf_state.pdf == null)
		{
			select_pdf(&dialog_state);
		}
		else
		{
			display_pdf(&pdf_state, allocator);
		}
	}
	return 0;
}

fn void select_pdf(GuiWindowFileDialogState* state)
{
	if (state.windowActive) rg::guiLock();
	if (rg::guiButton({ 0, 0, 90, 40 }, "Open file")) state.windowActive = true;
	rg::guiUnlock();

	rg::guiWindowFileDialog(state);
}

macro @maybe_locked(bool locked; @body)
{
	if (locked)
	{
		rg::guiLock();
		defer rg::guiUnlock();

		@body();
	}
	else
	{
		@body();
	}
}

fn void display_pdf(PdfState* pdf_state, Allocator allocator)
{
	if (rg::guiButton({ 0, 0, 90, 40 }, "Close file"))
	{
		pdf_state.pdf.cleanup();
		allocator::free(allocator, pdf_state.pdf);

		*pdf_state = {};
		rl::setWindowTitle(GUI_TITLE);
	}

	rg::guiLabel({ 130, 0, 60, 40 }, "Page");

	bool enable_left = pdf_state.page > 0;
	bool enable_right = pdf_state.page < pdf_state.total_pages - 1;

	bool page_left, page_right;

	@maybe_locked(!enable_left) {page_left = rg::guiButton({ 200, 0, 30, 40 }, "<");};
	@maybe_locked(!enable_right) {page_right = rg::guiButton({ 320, 0, 30, 40 }, ">");};

	if (page_left)
	{
		pdf_state.page -= 1;
	}

	if (page_right)
	{
		pdf_state.page += 1;
	}

	@pool()
	{
		DString buf = dstring::temp_new();
		buf.appendf("%d / %d", pdf_state.page + 1, pdf_state.total_pages);

		rg::guiLabel({ 240, 0, 70, 40 }, buf.zstr_view());
	};
}
