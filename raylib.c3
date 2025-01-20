module raylib55::rl;

struct Color
{
	char r;
	char b;
	char g;
	char a;
}

struct Rectangle
{
	float x;
	float y;
	float width;
	float height;
}

distinct CString = inline void*;

extern fn void initWindow(CInt, CInt, CString) @extern("InitWindow");
extern fn void closeWindow() @extern("CloseWindow");
extern fn void setTargetFps(CInt) @extern("SetTargetFPS");
extern fn bool windowShouldClose() @extern("WindowShouldClose");
extern fn void beginDrawing() @extern("BeginDrawing");
extern fn void endDrawing() @extern("EndDrawing");
extern fn void clearBackground(Color color) @extern("ClearBackground");
extern fn Color getColor(int) @extern("GetColor");
