SRC = pdf_editor.c \
	memory.c \
	object.c
BUILD = build
TARGET = pdf_editor

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
	$(CC) $^ -o $@

$(OBJ): $(BUILD)/%.o: %.c | dirs
	$(CC) $(FLAGS) -c $< -o $@

dirs: $(BUILD)

$(BUILD):
	mkdir -p $(BUILD)

clean:
	rm -f $(TARGET)
	rm -rf $(BUILD)

iwyu: $(IWYU_SRC)

$(IWYU_SRC): iwyu/%.c: %.c
	$(IWYU) $<

-include $(DEP)
