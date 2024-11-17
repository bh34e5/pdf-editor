#include <alloca.h>
#include <assert.h>
#include <stdlib.h>

#include "common.h"
#include "memory.h"
#include "object.h"

struct arena;

#define X(obj, str, l) struct pdf_name const obj = {.text = str, .len = l};
NAMES
#undef X

static long hex_str_to_num(char const *text, ssize_t len) {
    long num = 0;

    char c;
    while (len-- > 0) {
        assert(is_hex_char((c = *(text++))));
        if (is_digit(c)) {
            num = (16 * num) + (c - '0');
        } else if ('a' <= c && c <= 'f') {
            num = (16 * num) + (c - 'a' + 10);
        } else {
            num = (16 * num) + (c - 'A' + 10);
        }
    }
    return num;
}

static inline bool is_delimeter_char(char c) {
    switch (c) {
    case '(':
    case ')':
    case '<':
    case '>':
    case '[':
    case ']':
    case '{':
    case '}':
    case '/':
    case '%':
        return true;
    default:
        return false;
    }
}

static inline bool is_valid_name_char(char c) {
    return !(is_whitespace(c) || is_delimeter_char(c));
}

static struct pdf_name read_name_at(struct file file, ssize_t offset,
                                    struct arena *arena, ssize_t *end) {
    // TODO(bhester): since names are "atomic", do I want to intern these
    // possibly?

    char const *contents = file.contents;
    ssize_t len = file.len;

    assert(offset < len);
    assert_char_at(contents, len, offset, '/');
    ++offset;

    ssize_t name_len = 0;
    ssize_t cur_offset = offset;
    char c;
    while (offset < len && is_valid_name_char((c = contents[cur_offset]))) {
        if (c == '#') {
            assert(offset + 2 < len);
            assert(is_valid_name_char(contents[cur_offset + 1]));
            assert(is_valid_name_char(contents[cur_offset + 2]));
            cur_offset += 3;
        } else {
            ++cur_offset;
        }
        ++name_len;
    }

    char *name_contents = ARENA_PUSH_N(char, arena, name_len);
    struct pdf_name name = {
        .len = name_len,
        .text = name_contents,
    };

    cur_offset = offset;
    for (ssize_t i = 0; i < name_len; ++i) {
        c = contents[cur_offset];
        if (c == '#') {
            long val = hex_str_to_num(contents + 1, 2);
            // only two hex characters, it can't be bigger than this...
            assert(0 <= val && val < (1 << 8));
            name_contents[i] = (char)val;
            cur_offset += 3;
        } else {
            name_contents[i] = c;
            ++cur_offset;
        }
    }

    if (end != NULL) {
        *end = cur_offset;
    }
    return name;
}

static struct pdf_array read_array_at(struct file file, ssize_t offset,
                                      struct arena *arena, ssize_t *end) {
    char const *contents = file.contents;
    ssize_t len = file.len;

    assert(offset < len);
    assert_char_at(contents, len, offset, '[');
    ++offset;

    ssize_t array_len = 0;
    struct temp_object {
        struct pdf_object object;
        struct temp_object *next;
    };
    struct temp_object *head = NULL;
    struct temp_object **head_next = &head;

    ssize_t cur_offset = offset;
    while (true) {
        ssize_t obj_start = skip_whitespace(file, cur_offset);
        assert(obj_start < len);

        if (contents[obj_start] == ']') {
            cur_offset = obj_start;
            break;
        }

        struct pdf_object obj =
            read_object_at(file, obj_start, arena, &cur_offset);

        struct temp_object *tmp = alloca(sizeof(*tmp));
        *tmp = (struct temp_object){
            .object = obj,
            .next = NULL,
        };

        ++array_len;
        *head_next = tmp;
        head_next = &tmp->next;
    }
    assert_char_at(contents, len, cur_offset, ']');

    struct pdf_array array = {
        .count = array_len,
        .objects = ARENA_PUSH_N(struct pdf_object, arena, array_len),
    };

    struct temp_object *cur_temp_obj = head;
    for (ssize_t obj_i = 0; obj_i < array.count; ++obj_i) {
        array.objects[obj_i] = cur_temp_obj->object;
        cur_temp_obj = cur_temp_obj->next;
    }

    if (end != NULL) {
        *end = cur_offset + 1; // after the close bracket
    }
    return array;
}

static struct pdf_string read_pstring_at(struct file file, ssize_t offset,
                                         struct arena *arena, ssize_t *end) {
    char const *contents = file.contents;
    ssize_t len = file.len;

    assert(offset < len);
    assert_char_at(contents, len, offset, '(');
    ++offset;

    ssize_t str_len = 0;

    ssize_t cur_offset = offset;
    unsigned int paren_stack = 0;
    char c;
    while (cur_offset < len &&
           !(((c = contents[cur_offset]) == ')') && paren_stack == 0)) {
        if (c == '\\') {
            assert(cur_offset + 1 < len);
            char next = contents[cur_offset + 1];

            switch (next) {
            case 'n':
            case 'r':
            case 't':
            case 'b':
            case 'f':
            case '(':
            case ')':
            case '\\': {
                ++str_len;
                cur_offset += 2;
                continue;
            }
            default: {
                if (next == '\n') {
                    cur_offset += 2;
                    continue;
                } else if (next == '\r') {
                    if (cur_offset + 2 < len &&
                        contents[cur_offset + 2] == '\n') {
                        cur_offset += 3;
                    } else {
                        cur_offset += 2;
                    }
                    continue;
                } else if (is_octal_digit(next)) {
                    ++str_len;
                    ++cur_offset;

                    unsigned int octal_count = 0;
                    while (octal_count <= 3 && cur_offset + octal_count < len &&
                           is_octal_digit(contents[cur_offset + octal_count])) {
                        ++octal_count;
                    }

                    cur_offset += octal_count;
                    continue;
                }
            }
            }
        } else if (c == '\r' && cur_offset + 1 < len &&
                   contents[cur_offset + 1] == '\n') {
            ++str_len;
            cur_offset += 2;
            continue;
        }

        if (c == '(') {
            ++paren_stack;
        } else if (c == ')') {
            --paren_stack;
        }

        ++str_len;
        ++cur_offset;
    }
    assert_char_at(contents, len, cur_offset, ')');

    char *str_contents = ARENA_PUSH_N(char, arena, str_len);
    struct pdf_string string = {
        .type = ST_PAREN,
        .text = str_contents,
        .len = str_len,
    };

    cur_offset = offset;
    ssize_t str_i = 0;
    while (str_i < string.len) {
        c = contents[cur_offset];
        if (c == '\\') {
            char next = contents[cur_offset + 1];
            switch (next) {
            case 'n': {
                str_contents[str_i] = '\n';
                cur_offset += 2;
                ++str_i;
                continue;
            }
            case 'r': {
                str_contents[str_i] = '\r';
                cur_offset += 2;
                ++str_i;
                continue;
            }
            case 't': {
                str_contents[str_i] = '\t';
                cur_offset += 2;
                ++str_i;
                continue;
            }
            case 'b': {
                str_contents[str_i] = '\b';
                cur_offset += 2;
                ++str_i;
                continue;
            }
            case 'f': {
                str_contents[str_i] = '\f';
                cur_offset += 2;
                ++str_i;
                continue;
            }
            case '(': {
                str_contents[str_i] = '(';
                cur_offset += 2;
                ++str_i;
                continue;
            }
            case ')': {
                str_contents[str_i] = ')';
                cur_offset += 2;
                ++str_i;
                continue;
            }
            case '\\': {
                str_contents[str_i] = '\\';
                cur_offset += 2;
                ++str_i;
                continue;
            }
            default:
                if (next == '\n') {
                    cur_offset += 2;
                    continue;
                } else if (next == '\r') {
                    if (cur_offset + 2 < len &&
                        contents[cur_offset + 2] == '\n') {
                        cur_offset += 3;
                    } else {
                        cur_offset += 2;
                    }
                    continue;
                } else if (is_octal_digit(next)) {
                    long val = 0;

                    ++cur_offset; // the slash

                    unsigned int octal_count = 0;
                    char c2;
                    while (octal_count <= 3 && cur_offset + octal_count < len &&
                           is_octal_digit(
                               (c2 = contents[cur_offset + octal_count]))) {
                        val = (8 * val) + (c2 - '0');
                        ++octal_count;
                    }

                    // FIXME(bhester): is this a valid assert?
                    assert(0 <= val && val < (1 << 8));
                    str_contents[str_i] = (char)val;

                    cur_offset += octal_count;
                    ++str_i;
                    continue;
                }
            }
        } else if (c == '\r' && cur_offset + 1 < len &&
                   contents[cur_offset + 1] == '\n') {
            str_contents[str_i] = '\n';
            cur_offset += 2;
            ++str_i;
            continue;
        }

        str_contents[str_i] = c;
        ++cur_offset;
        ++str_i;
    }
    assert_char_at(contents, len, cur_offset, ')');

    if (end != NULL) {
        assert(cur_offset + 1 < len);
        *end = cur_offset + 1;
    }
    return string;
}

static struct pdf_string read_bstring_at(struct file file, ssize_t offset,
                                         struct arena *arena, ssize_t *end) {
    char const *contents = file.contents;
    ssize_t len = file.len;

    assert(offset < len);
    assert_char_at(contents, len, offset, '<');
    ++offset;

    ssize_t str_len = 0;

    ssize_t cur_offset = skip_whitespace(file, offset);
    char c;
    while (cur_offset < len && (c = contents[cur_offset]) != '>') {
        ssize_t fst_offset = cur_offset;
        ssize_t snd_offset = skip_whitespace(file, fst_offset + 1);
        assert(snd_offset < len);

        bool early_close_string = contents[snd_offset] == '>';

        if (early_close_string) {
            ++str_len;
            cur_offset = snd_offset;
            break;
        }

        ++str_len;
        cur_offset = skip_whitespace(file, snd_offset + 1);
    }
    assert_char_at(contents, len, cur_offset, '>');

    char *str_contents = ARENA_PUSH_N(char, arena, str_len);
    struct pdf_string string = {
        .type = ST_BRACE,
        .text = str_contents,
        .len = str_len,
    };

    cur_offset = skip_whitespace(file, offset);
    for (ssize_t str_i = 0; str_i < string.len; ++str_i) {
        ssize_t fst_offset = cur_offset;
        ssize_t snd_offset = skip_whitespace(file, fst_offset + 1);

        bool early_close_string = contents[snd_offset] == '>';

        char chars[2] = {0};
        if (early_close_string) {
            chars[0] = contents[fst_offset];
            chars[1] = '0';
        } else {
            chars[0] = contents[fst_offset];
            chars[1] = contents[snd_offset];
        }

        long val = hex_str_to_num(chars, 2);
        // shouldn't be possible to be larger with two characters
        assert(0 <= val && val < (1 << 8));
        str_contents[str_i] = (char)val;

        cur_offset = early_close_string ? snd_offset
                                        : skip_whitespace(file, snd_offset + 1);
    }
    assert_char_at(contents, len, cur_offset, '>');

    if (end != NULL) {
        assert(cur_offset + 1 < len);
        *end = cur_offset + 1;
    }
    return string;
}

static struct pdf_dict read_dict_at(struct file file, ssize_t offset,
                                    struct arena *arena, ssize_t *end) {
    char const *contents = file.contents;
    ssize_t len = file.len;

    assert(offset < len);
    assert_char_at(contents, len, offset + 0, '<');
    assert_char_at(contents, len, offset + 1, '<');
    offset += 2;

    ssize_t dict_len = 0;
    struct temp_entry {
        struct pdf_name name;
        struct pdf_object value;
        struct temp_entry *next;
    };
    struct temp_entry *head = NULL;
    struct temp_entry **head_next = &head;

    ssize_t cur_offset = offset;
    while (true) {
        ssize_t name_start = skip_whitespace(file, cur_offset);
        assert(name_start < len);

        if (contents[name_start] == '>') {
            assert(name_start + 1 < len);
            assert_char_at(contents, len, name_start + 1, '>');

            cur_offset = name_start;
            break;
        }

        assert_char_at(contents, len, name_start, '/');
        struct pdf_name name =
            read_name_at(file, name_start, arena, &cur_offset);

        ssize_t obj_start = skip_whitespace(file, cur_offset);
        assert(obj_start < len);

        struct pdf_object obj =
            read_object_at(file, obj_start, arena, &cur_offset);

        struct temp_entry *tmp = alloca(sizeof(*tmp));
        *tmp = (struct temp_entry){
            .name = name,
            .value = obj,
            .next = NULL,
        };

        ++dict_len;
        *head_next = tmp;
        head_next = &tmp->next;
    }
    assert_char_at(contents, len, cur_offset + 0, '>');
    assert_char_at(contents, len, cur_offset + 1, '>');

    struct pdf_dict dict = {
        .count = dict_len,
        .names = ARENA_PUSH_N(struct pdf_name, arena, dict_len),
        .objects = ARENA_PUSH_N(struct pdf_object, arena, dict_len),
    };

    struct temp_entry *cur_temp_entry = head;
    for (ssize_t entry_i = 0; entry_i < dict.count; ++entry_i) {
        dict.names[entry_i] = cur_temp_entry->name;
        dict.objects[entry_i] = cur_temp_entry->value;
        cur_temp_entry = cur_temp_entry->next;
    }

    if (end != NULL) {
        *end = cur_offset + 2; // after close brace
    }
    return dict;
}

static struct pdf_object read_dict_or_stream_at(struct file file,
                                                ssize_t offset,
                                                struct arena *arena,
                                                ssize_t *end) {
    ssize_t dict_end;
    struct pdf_dict dict = read_dict_at(file, offset, arena, &dict_end);

    char c;
    while (dict_end < file.len &&
           is_whitespace((c = file.contents[dict_end]))) {
        ++dict_end;
    }

    if (dict_end < file.len && c == 's') {
        if (string_matches_at(file, dict_end, "stream")) {
            struct pdf_stream stream;

            ssize_t stream_start = next_line_start(file, dict_end);

            assert(has_entry(dict, length_name));
            struct pdf_object length_obj = get_entry(dict, length_name);
            switch (length_obj.type) {
            case OBJ_NUMBER: {
                assert(length_obj.number.type == NT_INTEGER);
                stream = (struct pdf_stream){
                    .meta = dict,
                    .offset = stream_start,
                    .length_type = LT_DIRECT,
                    .direct = length_obj.number.integer,
                };

                ssize_t stream_end = stream_start + stream.direct;
                ssize_t after_space = skip_whitespace(file, stream_end);
                assert(after_space < file.len);
                assert_string_at(file, after_space, "endstream");

                if (end != NULL) {
                    assert(after_space + 9 < file.len);
                    *end = after_space + 9;
                }
            } break;
            case OBJ_IND_REF: {
                stream = (struct pdf_stream){
                    .meta = dict,
                    .offset = stream_start,
                    .length_type = LT_INDIRECT,
                    .indirect = length_obj.ref,
                };

                if (end != NULL) {
                    *end = (ssize_t)-1; // FIXME(bhester): what to do here...
                }
            } break;
            default:
                assert(0 && "Invalid type of length key");
            }

            return (struct pdf_object){
                .type = OBJ_STREAM,
                .stream = stream,
            };
        }
    }

    if (end != NULL) {
        *end = dict_end;
    }

    return (struct pdf_object){
        .type = OBJ_DICT,
        .dict = dict,
    };
}

static inline bool is_numeric_char(char c) {
    return is_digit(c) || c == '+' || c == '-' || c == '.';
}

struct pdf_number read_number_at(struct file file, ssize_t offset,
                                 struct arena *arena, ssize_t *end) {
    (void)arena;

    char const *contents = file.contents;
    ssize_t len = file.len;

    bool found_decimal = false;
    bool found_sign = false;
    bool negative = false;

    long lresult = 0;
    unsigned long denominator = 1;

    char c;
    while (offset < len && is_numeric_char((c = contents[offset]))) {
        if (c == '.') {
            if (found_decimal) {
                break;
            }
            found_decimal = true;
        } else if (c == '+' || c == '-') {
            if (found_sign) {
                break;
            }
            found_sign = true;

            if (c == '-') {
                negative = true;
            }
        } else {
            if (found_decimal) {
                denominator *= 10;
            }
            // TODO(what if this overflows longs? need to convert to float?
            lresult = (10 * lresult) + (c - '0');
        }
        ++offset;
    }

    if (end != NULL) {
        *end = offset;
    }

    long signed_result = negative ? -lresult : lresult;
    if (found_decimal) {
        return (struct pdf_number){
            .type = NT_REAL,
            .real = (double)signed_result / (double)denominator,
        };
    } else {
        return (struct pdf_number){
            .type = NT_INTEGER,
            .integer = signed_result,
        };
    }
}

static struct pdf_object read_number_or_ref_at(struct file file, ssize_t offset,
                                               struct arena *arena,
                                               ssize_t *end) {
    char const *contents = file.contents;
    ssize_t len = file.len;

    ssize_t first_number_end;
    struct pdf_number number =
        read_number_at(file, offset, arena, &first_number_end);

    if (number.type == NT_INTEGER) {
        // we just read an integer, so if this is an indirect reference, we
        // expect to read another integer followed by the keyword "R"

        long gen_num = 0;

        ssize_t cur_offset = skip_whitespace(file, first_number_end);
        char c;

        // loop to read the gen_num
        while (cur_offset < len) {
            if (!is_digit((c = contents[cur_offset]))) {
                if (!is_whitespace(c)) {
                    // expecting to read an integer followed by the "R" keyword
                    // and we saw something neither a digit nor whitespace, so
                    // break and return the number we first read
                    break;
                }

                // read the "R" keyword and return the ref if correct
                ssize_t after_space = skip_whitespace(file, cur_offset);
                if (string_matches_at(file, after_space, "R")) {
                    if (after_space + 1 < len &&
                        (is_delimeter_char(contents[after_space + 1]) ||
                         is_whitespace(contents[after_space + 1]))) {
                        // this is a reference
                        if (end != NULL) {
                            *end = after_space + 1;
                        }
                        return (struct pdf_object){
                            .type = OBJ_IND_REF,
                            .ref =
                                {
                                    .obj_num = number.integer,
                                    .gen_num = gen_num,
                                },
                        };
                    }
                }
                // didn't get the ref, break and return the number we first read
                break;
            }
            gen_num = (10 * gen_num) + (c - '0');
            ++cur_offset;
        }
    }

    if (end != NULL) {
        *end = first_number_end;
    }

    return (struct pdf_object){
        .type = OBJ_NUMBER,
        .number = number,
    };
}

struct pdf_object read_object_at(struct file file, ssize_t offset,
                                 struct arena *arena, ssize_t *end) {
    char const *contents = file.contents;
    ssize_t len = file.len;

    assert(offset < len);

    char c;
    switch ((c = contents[offset])) {
    case '/':
        return (struct pdf_object){
            .type = OBJ_NAME,
            .name = read_name_at(file, offset, arena, end),
        };
    case '[':
        return (struct pdf_object){
            .type = OBJ_ARRAY,
            .array = read_array_at(file, offset, arena, end),
        };
    case '(':
        return (struct pdf_object){
            .type = OBJ_STRING,
            .string = read_pstring_at(file, offset, arena, end),
        };
    case '<': {
        assert(offset + 1 < len);
        if (contents[offset + 1] == '<') {
            return read_dict_or_stream_at(file, offset, arena, end);
        }

        return (struct pdf_object){
            .type = OBJ_STRING,
            .string = read_bstring_at(file, offset, arena, end),
        };
    }
    default:
        if (string_matches_at(file, offset, "true")) {
            if (end != NULL) {
                assert(offset + 4 < len);
                *end = offset + 4;
            }
            return (struct pdf_object){
                .type = OBJ_BOOL,
                .b = true,
            };
        } else if (string_matches_at(file, offset, "false")) {
            if (end != NULL) {
                assert(offset + 5 < len);
                *end = offset + 5;
            }
            return (struct pdf_object){
                .type = OBJ_BOOL,
                .b = false,
            };
        } else if (string_matches_at(file, offset, "null")) {
            if (end != NULL) {
                assert(offset + 4 < len);
                *end = offset + 4;
            }
            return (struct pdf_object){
                .type = OBJ_NULL,
                .null = NULL,
            };
        } else if (is_numeric_char(c)) {
            return read_number_or_ref_at(file, offset, arena, end);
        }

        assert(0 && "Unimplemented");
    }
}

static bool names_equal(struct pdf_name lhs, struct pdf_name rhs) {
    if (lhs.len != rhs.len) {
        return false;
    }

    for (ssize_t i = 0; i < lhs.len; ++i) {
        if (lhs.text[i] != rhs.text[i]) {
            return false;
        }
    }
    return true;
}

static bool strings_equal(struct pdf_string lhs, struct pdf_string rhs) {
    if (lhs.type != rhs.type) {
        return false;
    }

    if (lhs.len != rhs.len) {
        return false;
    }

    for (ssize_t i = 0; i < lhs.len; ++i) {
        if (lhs.text[i] != rhs.text[i]) {
            return false;
        }
    }
    return true;
}

static bool arrays_equal(struct pdf_array lhs, struct pdf_array rhs) {
    if (lhs.count != rhs.count) {
        return false;
    }

    for (ssize_t i = 0; i < lhs.count; ++i) {
        if (!objects_equal(lhs.objects[i], rhs.objects[i])) {
            return false;
        }
    }
    return true;
}

static bool dicts_equal(struct pdf_dict lhs, struct pdf_dict rhs) {
    if (lhs.count != rhs.count) {
        return false;
    }

    for (ssize_t i = 0; i < lhs.count; ++i) {
        struct pdf_name name_i = lhs.names[i];
        if (!has_entry(rhs, name_i)) {
            return false;
        }

        struct pdf_object lhs_obj_i = lhs.objects[i];
        struct pdf_object rhs_obj_i = get_entry(rhs, name_i);
        if (!objects_equal(lhs_obj_i, rhs_obj_i)) {
            return false;
        }
    }
    return true;
}

static bool streams_equal(struct pdf_stream lhs, struct pdf_stream rhs) {
    if (lhs.offset != rhs.offset) {
        return false;
    }

    // they have the same offset, so they should be the same stream. assert the
    // meta dict is the same for added assurance
    assert(dicts_equal(lhs.meta, rhs.meta));
    return true;
}

bool objects_equal(struct pdf_object lhs, struct pdf_object rhs) {
    if (lhs.type != rhs.type) {
        return false;
    }

    switch (lhs.type) {
    case OBJ_NULL:
        return true;
    case OBJ_BOOL:
        return lhs.b == rhs.b;
    case OBJ_NUMBER: {
        struct pdf_number nl = lhs.number;
        struct pdf_number nr = rhs.number;

        if (nl.type != nr.type)
            return false;

        switch (nl.type) {
        case NT_REAL:
            return nl.real == nr.real;
        case NT_INTEGER:
            return nl.integer == nr.integer;
        }
    } break;
    case OBJ_NAME:
        return names_equal(lhs.name, rhs.name);
    case OBJ_STRING:
        return strings_equal(lhs.string, rhs.string);
    case OBJ_ARRAY:
        return arrays_equal(lhs.array, rhs.array);
    case OBJ_DICT:
        return dicts_equal(lhs.dict, rhs.dict);
    case OBJ_STREAM:
        return streams_equal(lhs.stream, rhs.stream);
    case OBJ_IND_REF:
        return lhs.ref.obj_num == rhs.ref.obj_num &&
               lhs.ref.gen_num == rhs.ref.gen_num;
    }
    return false;
}

static bool _has_entry(struct pdf_dict dict, struct pdf_name name,
                       ssize_t *ind) {
    for (ssize_t i = 0; i < dict.count; ++i) {
        struct pdf_name name_i = dict.names[i];
        if (names_equal(name, name_i)) {
            if (ind != NULL) {
                *ind = i;
            }
            return true;
        }
    }
    return false;
}

bool has_entry(struct pdf_dict const dict, struct pdf_name const name) {
    return _has_entry(dict, name, NULL);
}

struct pdf_object get_entry(struct pdf_dict const dict,
                            struct pdf_name const name) {
    ssize_t ind;
    assert(_has_entry(dict, name, &ind));

    return dict.objects[ind];
}
