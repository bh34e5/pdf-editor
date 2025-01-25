module pdf::render;

import pdf::object;

struct RenderObject
{
	void* temp;
}

macro void @for_render_objects(PdfPage page; @body(RenderObject obj))
{
	callPrivate();
	// stub
	RenderObject cur;
	@body(cur);
}

module pdf::render @private;

fn void callPrivate() {}
