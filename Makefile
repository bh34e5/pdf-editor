DIRS = build lib
LIBS = lib/libz.a lib/libraylib.a
CONFIG-LOCK = .is-configured

.PHONY: clean configure libs

pdf-editor: libs *.c3 project.json Makefile
	c3c build pdf-editor

configure: $(CONFIG-LOCK)

libs: $(CONFIG-LOCK) $(LIBS)

$(CONFIG-LOCK):
	bash -c "cd deps/zlib/ && ./configure"
	bash -c "cd deps/raylib/ && cmake -S ."
	touch $(CONFIG-LOCK)

lib/libz.a: $(DIRS)
	$(MAKE) -C deps/zlib/
	gcc -c deps/shims/zlib.c -o build/libc_shim.o
	cp deps/zlib/libz.a lib/
	ar -r lib/libz.a build/libc_shim.o

lib/libraylib.a: $(DIRS)
	$(MAKE) -C deps/raylib/
	cp deps/raylib/raylib/libraylib.a lib/

test_ref: pdf-editor
	./pdf-editor reference1.0.pdf

$(DIRS):
	mkdir -p $(DIRS)

clean:
	$(MAKE) -C deps/zlib/ clean
	$(MAKE) -C deps/raylib/ clean
	rm -rf $(DIRS)
	rm -f $(CONFIG-LOCK)
	rm -f pdf-editor
	rm -rf pdf-editor.dSYM/
