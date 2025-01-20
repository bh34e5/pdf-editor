DIRS = build lib
LIBS = lib/libz.a lib/libraylib.a lib/libraygui.a
CONFIG-LOCK = .is-configured

UNAME_S := $(shell uname -s)
TARGET = windows-x64

ifeq ($(UNAME_S),Darwin)
	TARGET = macos-x64
endif

.PHONY: clean configure libs

pdf-editor: libs *.c3 project.json Makefile
	c3c build $(TARGET)

configure: $(CONFIG-LOCK)

libs: $(CONFIG-LOCK) $(LIBS)

$(CONFIG-LOCK):
	bash -c "cd deps/zlib/ && ./configure"
	bash -c "cd deps/raylib/ && cmake -S ."
	touch $(CONFIG-LOCK)

lib/libz.a: | $(DIRS)
	$(MAKE) -C deps/zlib/
	gcc -c deps/shims/zlib.c -o build/libz_shim.o
	cp deps/zlib/libz.a lib/
	ar -r lib/libz.a build/libz_shim.o

lib/libraylib.a: | $(DIRS)
	$(MAKE) -C deps/raylib/
	cp deps/raylib/raylib/libraylib.a lib/

lib/libraygui.a: lib/libraylib.a | $(DIRS)
	gcc -Ideps/raylib/src/ -c deps/shims/raygui.c -o build/raygui_shim.o
	ar -r lib/libraygui.a build/raygui_shim.o

test_ref: pdf-editor
	./pdf-editor reference1.0.pdf

test_sobel: pdf-editor
	./pdf-editor sobel.pdf

$(DIRS):
	mkdir -p $(DIRS)

clean:
	$(MAKE) -C deps/zlib/ clean
	$(MAKE) -C deps/raylib/ clean
	c3c clean
	rm -rf $(DIRS)
	rm -f $(CONFIG-LOCK)
	rm -f pdf-editor
	rm -rf pdf-editor.dSYM/
