DIRS = build lib

pdf-editor: lib/libz.a *.c3 project.json
	c3c build pdf-editor

lib/libz.a: $(DIRS)
	mkdir -p lib/

	$(MAKE) -C deps/zlib-1.3.1/
	gcc -c deps/shims/zlib.c -o build/libc_shim.o
	cp deps/zlib-1.3.1/libz.a lib/
	ar -r lib/libz.a build/libc_shim.o

test_ref: pdf-editor
	./pdf-editor reference1.0.pdf

$(DIRS):
	mkdir -p $(DIRS)

.PHONY: clean
clean:
	$(MAKE) -C deps/zlib-1.3.1/ clean
	rm -rf build/
	rm -rf lib/
	rm -f pdf-editor
	rm -rf pdf-editor.dSYM/
