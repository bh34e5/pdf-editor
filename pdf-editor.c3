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
	PdfPage page;
	uint page_ind;
	uint total_pages;
	// TODO(bhester): maybe write the pages to textures so that once they've
	// been rendered once, I don't have to do it again...
}

struct UIState
{
	bool open_file;
	bool close_file;
	int page_delta;
	GuiWindowFileDialogState dialog_state;
}

macro void char[*].cstr_copy(&self, String other)
{
	char[] slice = self;

	assert(slice.len > other.len);
	mem::copy(slice.ptr, other.ptr, other.len);
	slice[other.len] = '\0';
}

macro @with_drawing(; @body)
{
	rl::beginDrawing();
	defer rl::endDrawing();

	@body();
}

fn int test_gui(Allocator allocator = allocator::heap())
{
	PdfState pdf_state;
	defer
	{
		if (pdf_state.pdf != null)
		{
			pdf_state.pdf.cleanup();
			allocator::free(allocator, pdf_state.pdf);
		}
	}

	rl::initWindow(800, 600, GUI_TITLE);
	defer rl::closeWindow();

	rg::guiSetStyle(GuiControl.DEFAULT, GuiDefaultProperty.TEXT_SIZE, 16);
	rl::setTargetFps(60);

	UIState ui_state;
	ui_state.dialog_state = rg::initGuiWindowFileDialog();
	ui_state.dialog_state.filterExt.cstr_copy(".pdf");

	while (!rl::windowShouldClose())
	{
		if (ui_state.open_file)
		{
			ui_state.open_file = false;
			ui_state.dialog_state.windowActive = true;
		}

		if (ui_state.close_file)
		{
			ui_state.close_file = false;

			pdf_state.pdf.cleanup();
			allocator::free(allocator, pdf_state.pdf);

			pdf_state = {};
			rl::setWindowTitle(GUI_TITLE);
		}

		if (ui_state.page_delta != 0)
		{
			defer ui_state.page_delta = 0;
			pdf_state.page_ind += ui_state.page_delta;
			// FIXME(bhester): display error here?
			pdf_state.page = pdf_state.pdf.get_page(pdf_state.page_ind)!!;
			// TODO(bhester): do the render work (to texture) here?
		}

		if (ui_state.dialog_state.selectFilePressed)
		{
			ui_state.dialog_state.selectFilePressed = false;

			GuiWindowFileDialogState* dialog_state = &ui_state.dialog_state;
			char[] filename = &dialog_state.fileNameText;

			// TODO(bhester): handle moving directories?

			Pdf*! opened = pdf::read_new((String)filename, allocator);
			if (try opened)
			{
				uint total_pages = opened.page_count();
				uint page_ind = 0;

				pdf_state.pdf = opened;
				// FIXME(bhester): display error here?
				pdf_state.page = opened.get_page(page_ind)!!;
				pdf_state.page_ind = page_ind;
				pdf_state.total_pages = total_pages;

				rl::setWindowTitle(filename.ptr);
			}
			else
			{
				// TODO(bhester): display error message
				io::printn("Fialed to openf ile ");
			}
		}

		@with_drawing() {
			rl::clearBackground({0, 0, 0x20, 0xFF});

			if (pdf_state.pdf == null)
			{
				select_pdf(&ui_state);
			}
			else
			{
				display_pdf(&pdf_state, &ui_state);
			}
		};
	}
	return 0;
}

fn void select_pdf(UIState* ui_state)
{
	if (ui_state.dialog_state.windowActive) rg::guiLock();
	if (rg::guiButton({ 0, 0, 90, 40 }, "Open file")) ui_state.open_file = true;
	rg::guiUnlock();

	rg::guiWindowFileDialog(&ui_state.dialog_state);
}

fn void display_pdf(PdfState* pdf_state, UIState* ui_state)
{
	if (rg::guiButton({ 0, 0, 90, 40 }, "Close file"))
	{
		ui_state.close_file = true;
	}

	rg::guiLabel({ 130, 0, 60, 40 }, "Page");

	bool enable_left = pdf_state.page_ind > 0;
	bool enable_right = pdf_state.page_ind < pdf_state.total_pages - 1;

	bool page_left, page_right;

	@maybe_locked(!enable_left) {page_left = rg::guiButton({ 200, 0, 30, 40 }, "<");};
	@maybe_locked(!enable_right) {page_right = rg::guiButton({ 320, 0, 30, 40 }, ">");};

	if (page_left) {ui_state.page_delta -= 1;}
	if (page_right) {ui_state.page_delta += 1;}

	@pool()
	{
		DString buf = dstring::temp_new();
		buf.appendf("%d / %d", pdf_state.page_ind + 1, pdf_state.total_pages);

		rg::guiLabel({ 240, 0, 70, 40 }, buf.zstr_view());
	};

	display_page(pdf_state, ui_state);
}

fn void display_page(PdfState* pdf_state, UIState* ui_state)
{
	Pdf* pdf = pdf_state.pdf;
	PdfPage page = pdf_state.page;

	render::@for_render_objects(page, pdf; RenderObject obj)
	{
		io::printfn("%s", obj);
	};
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
