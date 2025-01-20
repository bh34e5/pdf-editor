DIRS = build lib
LIBS = lib/libz.a lib/libraylib.a lib/libraygui.a
CONFIG-LOCK = .is-configured

UNAME_S := $(shell uname -s)
TARGET = windows-x64

ifeq ($(UNAME_S),Darwin)
	TARGET = macos-x64
endif

.PHONY: clean

pdf-editor: $(LIBS) *.c3 project.json Makefile | $(CONFIG-LOCK)
	c3c build $(TARGET)

lib/libz.a: build/libz_shim.o deps/zlib/libz.a | $(DIRS)
	cp deps/zlib/libz.a lib/
	ar -r lib/libz.a build/libz_shim.o

lib/libraylib.a: deps/raylib/raylib/libraylib.a | $(DIRS)
	cp deps/raylib/raylib/libraylib.a lib/

lib/libraygui.a: build/raygui_shim.o lib/libraylib.a | $(DIRS)
	ar -r lib/libraygui.a build/raygui_shim.o

build/libz_shim.o: deps/shims/zlib.c | $(DIRS)
	gcc -c deps/shims/zlib.c -o build/libz_shim.o

build/raygui_shim.o: deps/shims/raygui.c | $(DIRS)
	gcc -Ideps/raylib/src/ -c deps/shims/raygui.c -o build/raygui_shim.o

deps/zlib/libz.a:
	$(MAKE) -C deps/zlib/

deps/raylib/raylib/libraylib.a:
	$(MAKE) -C deps/raylib/

$(DIRS):
	mkdir -p $(DIRS)

$(CONFIG-LOCK):
	bash -c "cd deps/zlib/ && ./configure"
	bash -c "cd deps/raylib/ && cmake -S ."
	touch $(CONFIG-LOCK)

test_ref: pdf-editor
	./pdf-editor reference1.0.pdf

test_sobel: pdf-editor
	./pdf-editor sobel.pdf

clean:
	$(MAKE) -C deps/zlib/ clean
	$(MAKE) -C deps/raylib/ clean
	c3c clean
	rm -rf $(DIRS)
	rm -f $(CONFIG-LOCK)
	rm -f pdf-editor
	rm -rf pdf-editor.dSYM/
