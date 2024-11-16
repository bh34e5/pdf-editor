SRC = pdf_editor.c \
	memory.c \
	object.c
BUILD = build
DEPS = deps
LIB = lib
TARGET = pdf_editor

LIBS = libz.a
PREF_LIBS = $(foreach L,$(LIBS),$(LIB)/$(L))
STRIPPED_LIBS = $(patsubst lib%.a,%,$(LIBS))
IWYU = ~/install/include-what-you-use/build/bin/include-what-you-use

CC = gcc
FLAGS += -ggdb
FLAGS += -MMD

OBJ = $(patsubst %.c,$(BUILD)/%.o,$(SRC))
DEP = $(patsubst %.c,$(BUILD)/%.d,$(SRC))
IWYU_SRC = $(patsubst %.c,iwyu/%.c,$(SRC))

.PHONY: all clean dirs iwyu gdb

all: $(BUILD)/$(TARGET)

gdb: $(BUILD)/$(TARGET)
	gdb $(BUILD)/$(TARGET)

$(BUILD)/$(TARGET): $(OBJ)
	$(CC) $^ -L$(LIB) $(foreach L,$(STRIPPED_LIBS),-l$(L)) -o $@

$(OBJ): $(BUILD)/%.o: %.c | $(PREF_LIBS) dirs
	$(CC) $(FLAGS) -c $< -o $@

$(LIB)/libz.a: | dirs
	cd $(DEPS)/zlib-1.3.1 && ./configure --static
	$(MAKE) -C $(DEPS)/zlib-1.3.1
	cp $(DEPS)/zlib-1.3.1/libz.a $(LIB)

dirs: $(LIB) $(BUILD)

$(BUILD):
	mkdir -p $(BUILD)

$(LIB):
	mkdir -p $(LIB)

clean:
	rm -f $(TARGET)
	rm -rf $(LIB)
	rm -rf $(BUILD)

iwyu: $(IWYU_SRC)

$(IWYU_SRC): iwyu/%.c: %.c
	$(IWYU) $<

-include $(DEP)
