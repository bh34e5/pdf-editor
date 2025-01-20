module raygui::rg;

import raylib55::rl;

enum GuiControl : (int value)
{
	// Default -> populates to all controls when set
	DEFAULT = 0,

	// Basic controls
	LABEL = 1,          // Used also for: LABELBUTTON
	BUTTON = 2,
	TOGGLE = 3,         // Used also for: TOGGLEGROUP
	SLIDER = 4,         // Used also for: SLIDERBAR, TOGGLESLIDER
	PROGRESSBAR = 5,
	CHECKBOX = 6,
	COMBOBOX = 7,
	DROPDOWNBOX = 8,
	TEXTBOX = 9,        // Used also for: TEXTBOXMULTI
	VALUEBOX = 10,
	SPINNER = 11,        // Uses: BUTTON, VALUEBOX
	LISTVIEW = 12,
	COLORPICKER = 13,
	SCROLLBAR = 14,
	STATUSBAR = 15,
}

enum GuiControlProperty : (int value)
{
	BORDER_COLOR_NORMAL = 0,        // Control border color in STATE_NORMAL
	BASE_COLOR_NORMAL = 1,          // Control base color in STATE_NORMAL
	TEXT_COLOR_NORMAL = 2,          // Control text color in STATE_NORMAL
	BORDER_COLOR_FOCUSED = 3,       // Control border color in STATE_FOCUSED
	BASE_COLOR_FOCUSED = 4,         // Control base color in STATE_FOCUSED
	TEXT_COLOR_FOCUSED = 5,         // Control text color in STATE_FOCUSED
	BORDER_COLOR_PRESSED = 6,       // Control border color in STATE_PRESSED
	BASE_COLOR_PRESSED = 7,         // Control base color in STATE_PRESSED
	TEXT_COLOR_PRESSED = 8,         // Control text color in STATE_PRESSED
	BORDER_COLOR_DISABLED = 9,      // Control border color in STATE_DISABLED
	BASE_COLOR_DISABLED = 10,        // Control base color in STATE_DISABLED
	TEXT_COLOR_DISABLED = 11,        // Control text color in STATE_DISABLED
	BORDER_WIDTH = 12,               // Control border size, 0 for no border
	//TEXT_SIZE,                  // Control text size (glyphs max height) -> GLOBAL for all controls
	//TEXT_SPACING,               // Control text spacing between glyphs -> GLOBAL for all controls
	//TEXT_LINE_SPACING           // Control text spacing between lines -> GLOBAL for all controls
	TEXT_PADDING = 13,               // Control text padding, not considering border
	TEXT_ALIGNMENT = 14,             // Control text horizontal alignment inside control text bound (after border and padding)
	//TEXT_WRAP_MODE              // Control text wrap-mode inside text bounds -> GLOBAL for all controls
}

enum GuiDefaultProperty : (int value)
{
	TEXT_SIZE = 16,             // Text size (glyphs max height)
	TEXT_SPACING = 17,               // Text spacing between glyphs
	LINE_COLOR = 18,                 // Line control color
	BACKGROUND_COLOR = 19,           // Background color
	TEXT_LINE_SPACING = 20,          // Text spacing between lines
	TEXT_ALIGNMENT_VERTICAL = 21,    // Text vertical alignment inside text bounds (after border and padding)
	TEXT_WRAP_MODE = 22,              // Text wrap-mode inside text bounds
	//TEXT_DECORATION             // Text decoration: 0-None = 23, 1-Underline, 2-Line-through, 3-Overline
	//TEXT_DECORATION_THICK       // Text decoration line thickness
}

macro int guiGetStyle(GuiControl control, guiProperty)
{
	$switch ($typeof(guiProperty))
		$case GuiControlProperty:
		$case GuiDefaultProperty:
			return _guiGetStyle(control, guiProperty.value);
		$default:
			// TODO(bhester): list the valid property types
			$error "Invalid property type";
	$endswitch
}

extern fn int _guiGetStyle(GuiControl, int) @extern("GuiGetStyle") @private;
extern fn bool guiButton(Rectangle, CString) @extern("GuiButton");
extern fn int guiMessageBox(Rectangle, CString, CString, CString) @extern("GuiMessageBox");
//extern fn gui() @extern("");
