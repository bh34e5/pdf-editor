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

//def Vector2 = float[<2>];
struct Vector2
{
	float x;
	float y;
}

struct FilePathList {
	uint capacity;          // Filepaths max entries
	uint count;             // Filepaths entries count
	char** paths;           // Filepaths entries
}

extern fn void initWindow(CInt, CInt, char*) @extern("InitWindow");
extern fn void closeWindow() @extern("CloseWindow");
extern fn void setTargetFps(CInt) @extern("SetTargetFPS");
extern fn void setWindowTitle(char*) @extern("SetWindowTitle");
extern fn bool windowShouldClose() @extern("WindowShouldClose");
extern fn void beginDrawing() @extern("BeginDrawing");
extern fn void endDrawing() @extern("EndDrawing");
extern fn void clearBackground(Color color) @extern("ClearBackground");
extern fn Color getColor(int) @extern("GetColor");
