module pdf::render;

import pdf::math;
import pdf::object;

enum RenderType
{
	TEXT,
}

struct RenderObject
{
	RenderType type;
	union as
	{
		struct text
		{
			PdfMatrix tm;
			PdfString text;
		}
	}
}

struct GraphicsState
{
	PdfMatrix ctm; // current transformation matrix

	// TODO(bhester): fill in the appropriate types
	PdfObject line_width;
	PdfObject line_cap;
	PdfObject line_join;
	PdfObject miter_limit;
	PdfObject dash_array;
	PdfObject dash_phase;
	PdfObject intent;
	PdfObject flatness;
	PdfObject dict_name;
}

fn GraphicsState default_graphics_state() @inline
{
	return {};
}

struct TextState
{
	PdfNumber tc;    // character spacing
	PdfNumber tw;    // word spacing
	PdfNumber th;    // horizontal scaling
	PdfNumber tl;    // leading
	PdfName tf;      // text font
	PdfNumber tfs;   // text font size
	ulong tmode;     // text rendering mode
	PdfNumber trise; // text rise
	PdfNumber tk;    // text knockout

	PdfMatrix tm;    // text matrix
	PdfMatrix tlm;   // text line matrix
	PdfMatrix trm;   // text rendering matrix

	bool in_text;
}

fn TextState default_text_state() @inline
{
	TextState state;
	state.th = {INTEGER, false, {.integer = 100}};
	state.tm = math::identity();
	state.tlm = math::identity();
	state.trm = math::identity();
	return state;
}

macro void @for_render_objects(PdfPage page, Pdf* pdf; @body(RenderObject obj))
{
	@pool()
	{
		ContentIter iter = get_content_iter(page, pdf);
		defer
		{
			if (iter.contents.len != 0)
			{
				allocator::free(pdf.allocator, iter.contents.ptr);
			}
		}

		while (true)
		{
			RenderObject! next = iter.next();
			if (catch err = next)
			{
				if (err == SearchResult.MISSING) break;
				err?!!; // rethrow
			}
			@body(next);
		}
	};
}

module pdf::render @private;

import pdf::common;
import pdf::math;
import pdf::object;
import pdf::object::utils;
import stack;

fn ContentIter get_content_iter(PdfPage page, Pdf* pdf)
{
	ContentIter iter = {};

	if (page.has_entry(object::CONTENTS_NAME))
	{
		PdfObject contents_obj = page.get_ind_entry(object::CONTENTS_NAME, pdf)!!;
		if (contents_obj.is(STREAM))
		{
			PdfStream contents_stream = contents_obj.get_as(STREAM);
			char[] contents = contents_stream.get_contents(pdf)!!;

			iter.contents = (String)contents;
			iter.cur = iter.contents;
			iter.pdf = pdf;
			iter.cur_state = default_graphics_state();
			iter.text_state = default_text_state();
			iter.state_stack = stack::temp_new(<GraphicsState>)();
			iter.op_stack.temp_init();
			iter.pseudo_op_stack.temp_init();
		}
		else
		{
			unsupported("Array of streams is unsupported");
		}
	}
	return iter;
}

struct ContentIter
{
	String contents;
	String cur;

	Pdf* pdf;
	GraphicsState cur_state;
	Stack(<GraphicsState>) state_stack;

	TextState text_state;
	Stack(<PdfObject>) op_stack;
	Stack(<PseudoOperation>) pseudo_op_stack;
}

enum PseudoOp
{
	HORI_SHIFT,
}

struct PseudoOperation
{
	bool keyword;
	union as
	{
		PdfKeyword keyword;
		PseudoOp op;
	}
}

fn RenderObject! ContentIter.next(&self)
{
	RenderObject r_object;

	String cur = self.cur;
	defer self.cur = cur; // update cur state

	while (cur.len) // should be while true, when I find the break condition in the loop...
	{
		while (self.pseudo_op_stack.len() > 0)
		{
			PseudoOperation op = self.pseudo_op_stack.pop();
			if (op.keyword)
			{
				PdfKeyword kwd = op.as.keyword;
				bool complete = do_operation(kwd, self, &r_object);
				if (complete) return r_object;
			}
			else
			{
				switch (op.as.op)
				{
					case HORI_SHIFT:
						PdfNumber offset = self.op_stack.pop().get_as(NUMBER);
						PdfMatrix* tm = &self.text_state.tm;
						tm.e = tm.e.plus(offset);
				}
			}
		}

		usz ind;
		PdfObject obj = object::read_from(cur, self.pdf, &ind)!;
		cur = cur[ind..];
		common::@skip_whitespace(cur);

		switch (obj.type)
		{
			case KEYWORD:
				bool complete = do_operation(obj.get_as(KEYWORD), self, &r_object);
				if (complete) return r_object;
			default:
				self.op_stack.push(obj);
		}
	}
	return SearchResult.MISSING?;
}

fn bool do_operation(PdfKeyword keyword, ContentIter* iter, RenderObject* r_object)
{
	GraphicsState* gs = &iter.cur_state;
	TextState* ts = &iter.text_state;
	Stack(<PdfObject>)* op_stack = &iter.op_stack;
	Stack(<PseudoOperation>)* pseudo_op_stack = &iter.pseudo_op_stack;

	switch (keyword)
	{
		// graphics state operators
		case Q_L:
			iter.state_stack.push(iter.cur_state);
		case Q_U:
			// TODO(bhester): better error handling... also, does
			// this hold _across_ content streams in the content
			// array...
			iter.cur_state = iter.state_stack.pop();
		case W_L:
			gs.line_width = op_stack.pop();
		case J_U:
			gs.line_cap = op_stack.pop();
		case J_L:
			gs.line_join = op_stack.pop();
		case M_U:
			gs.miter_limit = op_stack.pop();
		case D_L:
			gs.dash_phase = op_stack.pop();
			gs.dash_array = op_stack.pop();
		case RI_LL:
			gs.intent = op_stack.pop();
		case I_L:
			gs.flatness = op_stack.pop();
		case GS_LL:
			gs.dict_name = op_stack.pop();
		case CM_LL:
			gs.ctm.f = op_stack.pop().get_as(NUMBER);
			gs.ctm.e = op_stack.pop().get_as(NUMBER);
			gs.ctm.d = op_stack.pop().get_as(NUMBER);
			gs.ctm.c = op_stack.pop().get_as(NUMBER);
			gs.ctm.b = op_stack.pop().get_as(NUMBER);
			gs.ctm.a = op_stack.pop().get_as(NUMBER);

		// text state operators
		case TC_UL:
			ts.tc = op_stack.pop().get_as(NUMBER);
		case TW_UL:
			ts.tw = op_stack.pop().get_as(NUMBER);
		case TZ_UL:
			ts.th = op_stack.pop().get_as(NUMBER);
		case TL_UU:
			ts.tl = op_stack.pop().get_as(NUMBER);
		case TF_UL:
			ts.tfs = op_stack.pop().get_as(NUMBER);
			ts.tf = op_stack.pop().get_as(NAME);
		case TR_UL:
			ts.tmode = op_stack.pop().get_as(NUMBER).get_as(INTEGER);
		case TS_UL:
			ts.trise = op_stack.pop().get_as(NUMBER);

		// text object operators
		case BT_UU:
			assert(!ts.in_text);
			ts.in_text = true;

			ts.tm  = math::identity();
			ts.tlm = math::identity();
		case ET_UU:
			assert(ts.in_text);
			ts.in_text = false;

		// text positioning operators
		case TD_UL:
			PdfMatrix offset = math::identity();
			offset.f = op_stack.pop().get_as(NUMBER);
			offset.e = op_stack.pop().get_as(NUMBER);

			offset = offset.times(ts.tlm);
			ts.tm = offset;
			ts.tlm = offset;
		case TD_UU:
			op_stack.push({NUMBER, {.number = op_stack.peek().get_as(NUMBER).negated()}});
			iter.pseudo_op_stack.push({true, {.keyword = TD_UL}});
			iter.pseudo_op_stack.push({true, {.keyword = TL_UU}});
		case TM_UL:
			PdfMatrix m;
			m.f = op_stack.pop().get_as(NUMBER);
			m.e = op_stack.pop().get_as(NUMBER);
			m.d = op_stack.pop().get_as(NUMBER);
			m.c = op_stack.pop().get_as(NUMBER);
			m.b = op_stack.pop().get_as(NUMBER);
			m.a = op_stack.pop().get_as(NUMBER);

			ts.tm = m;
			ts.tlm = m;
		case T_STAR:
			op_stack.push({NUMBER, {}});
			op_stack.push({NUMBER, {.number = ts.tl}});
			iter.pseudo_op_stack.push({true, {.keyword = TD_UL}});

		// text showing operators
		case TJ_UL:
			r_object.type = TEXT;
			r_object.as.text.tm = ts.tm;
			r_object.as.text.text = op_stack.pop().get_as(STRING);
			return true;
		case S_QUOTE:
			iter.pseudo_op_stack.push({true, {.keyword = TJ_UL}});
			iter.pseudo_op_stack.push({true, {.keyword = T_STAR}});
			// not returning true, the pseudo op will do that
		case D_QUOTE:
			PdfObject string = op_stack.pop();
			PdfObject ac = op_stack.pop();
			PdfObject aw = op_stack.pop();
			op_stack.push(string);
			op_stack.push(ac);
			op_stack.push(aw);
			pseudo_op_stack.push({true, {.keyword = S_QUOTE}});
			pseudo_op_stack.push({true, {.keyword = TC_UL}});
			pseudo_op_stack.push({true, {.keyword = TW_UL}});
			// not returning true, the pseudo op will do that
		case TJ_UU:
			PdfArray arr = op_stack.pop().get_as(ARRAY);
			foreach (obj: arr.rev_iter())
			{
				switch (obj.type)
				{
					case NUMBER:
						// TODO(bhester): handle both vertical and horizontal
						// text... the operators should be different
						PdfObject n_off = {NUMBER, {.number = obj.get_as(NUMBER).negated()}};
						op_stack.push({NUMBER, {.number = math::ZERO}});
						op_stack.push(n_off);
						pseudo_op_stack.push({false, {.op = HORI_SHIFT}});
					case STRING:
						op_stack.push(obj);
						pseudo_op_stack.push({true, {.keyword = TJ_UL}});
					default:
						unreachable("Invalid object in TJ array");
				}
			}

		// unhandled
		default:
			unsupported("Unsupported operation");
	}

	assert((iter.pseudo_op_stack.len() > 0) || (op_stack.len() == 0));

	return false;
}
