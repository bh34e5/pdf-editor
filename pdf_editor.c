#include <alloca.h>
#include <assert.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

#include "common.h"
#include "memory.h"
#include "object.h"

#include "deps/zlib-1.3.1/zconf.h"
#include "deps/zlib-1.3.1/zlib.h"

struct arena;

struct version {
    int major;
    int minor;
};

static struct version read_version(struct file file) {
    assert_string_at(file, 0, "%PDF-");

    char const *contents = file.contents;
    ssize_t len = file.len;
    struct version version = {0};

    char c;
    ssize_t ind = 5;
    while (ind < len && is_digit((c = contents[ind]))) {
        version.major = (10 * version.major) + (c - '0');
        ++ind;
    }

    assert_char_at(contents, len, ind++, '.');

    while (ind < len && is_digit((c = contents[ind]))) {
        version.minor = (10 * version.minor) + (c - '0');
        ++ind;
    }

    return version;
}

static ssize_t get_last_eof_from(struct file file, ssize_t start) {
    char end_of_line = '\0';
    ssize_t start_of_line = 0;

    for (ssize_t i = start; i > 0; --i) {
        char c = file.contents[i - 1];
        if (c == '\r' || c == '\n') {
            end_of_line = c;
            start_of_line = i;
            break;
        }
    }
    assert(end_of_line != '\0' && "Could not find EOF marker");

    if (string_matches_at(file, start_of_line, "%%EOF")) {
        return start_of_line;
    }

    ssize_t next_start;
    if (start_of_line > 1 && end_of_line == '\n' &&
        file.contents[start_of_line - 2] == '\r') {
        next_start = start_of_line - 2;
    } else {
        next_start = start_of_line - 1;
    }

    return get_last_eof_from(file, next_start);
}

enum xref_entry_type {
    ET_IN_USE,
    ET_FREE,
};

struct xref_entry {
    enum xref_entry_type type;

    union {
        struct {
            ssize_t byte_off;
            ssize_t gen_num;
        } in_use;

        struct {
            ssize_t next_free_obj;
            ssize_t next_gen_num;
        } free;
    } as;
};

struct xref_section {
    ssize_t entry_start;
    ssize_t entry_count;
    struct xref_entry *entries;
};

struct xref {
    ssize_t section_count;
    struct xref_section *sections;
    struct pdf_dict trailer;
    struct xref *prev;
};

static struct xref read_xref(struct file file, ssize_t xrefstart,
                             struct arena *arena) {
    assert_string_at(file, xrefstart, "xref");
    ssize_t section_start = next_line_start(file, xrefstart);

    char const *contents = file.contents;
    ssize_t len = file.len;

    ssize_t section_count = 0;
    struct temp_section {
        ssize_t entry_start;
        ssize_t entry_count;
        ssize_t first_entry;
        struct temp_section *next;
    };
    struct temp_section *head = NULL;
    struct temp_section **head_next = &head;

    ssize_t cur_section_start = section_start;
    while (true) {
        if (!is_digit(contents[cur_section_start])) {
            break;
        }

        struct temp_section *tmp = alloca(sizeof(*tmp));
        *tmp = (struct temp_section){
            .entry_start = 0,
            .entry_count = 0,
            .first_entry = 0,
            .next = NULL,
        };

        ++section_count;
        *head_next = tmp;
        head_next = &tmp->next;

        char c;
        ssize_t ind = cur_section_start;
        while (ind < len && is_digit((c = contents[ind]))) {
            tmp->entry_start = (10 * tmp->entry_start) + (c - '0');
            ++ind;
        }

        assert_char_at(contents, len, ind++, ' ');

        while (ind < len && is_digit((c = contents[ind]))) {
            tmp->entry_count = (10 * tmp->entry_count) + (c - '0');
            ++ind;
        }

        ssize_t first_entry = next_line_start(file, cur_section_start);
        ssize_t section_length = 20 * tmp->entry_count;

        tmp->first_entry = first_entry;
        cur_section_start = first_entry + section_length;
    }
    assert(head != NULL && "Found no valid xref sections");

    struct xref xref = {
        .section_count = section_count,
        .sections = ARENA_PUSH_N(struct xref_section, arena, section_count),
        .trailer = {0},
        .prev = NULL,
    };

    struct temp_section *cur_temp_section = head;
    for (ssize_t section_i = 0; section_i < section_count; ++section_i) {
        struct xref_section *section = xref.sections + section_i;
        *section = (struct xref_section){
            .entry_start = cur_temp_section->entry_start,
            .entry_count = cur_temp_section->entry_count,
            .entries = ARENA_PUSH_N(struct xref_entry, arena,
                                    cur_temp_section->entry_count),
        };

        ssize_t first_entry = cur_temp_section->first_entry;
        for (ssize_t entry_i = 0; entry_i < section->entry_count; ++entry_i) {
            ssize_t entry_base = first_entry + 20 * entry_i;

            long num_one = atol(contents + entry_base + 0);
            long num_two = atol(contents + entry_base + 11);
            char type = contents[entry_base + 17];

            struct xref_entry *entry = section->entries + entry_i;

            switch (type) {
            case 'n': {
                *entry = (struct xref_entry){
                    .type = ET_IN_USE,
                    .as = {.in_use =
                               {
                                   .byte_off = num_one,
                                   .gen_num = num_two,
                               }},
                };
            } break;
            case 'f': {
                *entry = (struct xref_entry){
                    .type = ET_FREE,
                    .as = {.free =
                               {
                                   .next_free_obj = num_one,
                                   .next_gen_num = num_two,
                               }},
                };
            } break;
            default:
                assert(!"Unexpected xref entry type");
            }
        }

        cur_temp_section = cur_temp_section->next;
    }

    assert_string_at(file, cur_section_start, "trailer");
    ssize_t trailer_start = skip_whitespace(file, cur_section_start + 7);

    struct pdf_object trailer_obj =
        read_object_at(file, trailer_start, arena, NULL);

    assert(trailer_obj.type == OBJ_DICT);
    struct pdf_dict trailer_dict = trailer_obj.dict;

    xref.trailer = trailer_dict;

    struct pdf_name prev_name = {.text = "Prev", .len = 4};
    if (has_entry(trailer_dict, prev_name)) {
        xref.prev = ARENA_PUSH_N(struct xref, arena, 1);

        struct pdf_object prev_off_obj = get_entry(trailer_dict, prev_name);
        assert(prev_off_obj.type == OBJ_NUMBER &&
               prev_off_obj.number.type == NT_INTEGER);

        long prev_offset = prev_off_obj.number.integer;
        *xref.prev = read_xref(file, prev_offset, arena);
    }

    return xref;
}

struct pdf_file {
    struct file file;
    struct xref xref;
    struct arena *arena;
};

static struct pdf_object get_obj_ref(struct pdf_file pdf_file,
                                     struct pdf_ind_ref ref) {
    char const *contents = pdf_file.file.contents;
    ssize_t len = pdf_file.file.len;
    struct arena *arena = pdf_file.arena;

    ssize_t obj_num = ref.obj_num;
    ssize_t gen_num = ref.gen_num;

    struct xref cur_xref = pdf_file.xref;
    while (true) {
        for (ssize_t sect_i = 0; sect_i < cur_xref.section_count; ++sect_i) {
            struct xref_section section = cur_xref.sections[sect_i];
            if (obj_num >= section.entry_start &&
                obj_num < (section.entry_start + section.entry_count)) {
                ssize_t entry_i = obj_num - section.entry_start;
                struct xref_entry entry = section.entries[entry_i];

                assert(entry.type == ET_IN_USE);
                assert(gen_num == entry.as.in_use.gen_num);

                ssize_t obj_offset = entry.as.in_use.byte_off;

                ssize_t num_one = 0;
                ssize_t num_two = 0;
                char c;
                while (obj_offset < len &&
                       is_digit((c = contents[obj_offset]))) {
                    num_one = (10 * num_one) + (c - '0');
                    ++obj_offset;
                }
                assert(obj_offset < len);
                assert(is_whitespace(c));
                obj_offset = skip_whitespace(pdf_file.file, obj_offset);

                while (obj_offset < len &&
                       is_digit((c = contents[obj_offset]))) {
                    num_two = (10 * num_two) + (c - '0');
                    ++obj_offset;
                }
                assert(obj_offset < len);
                assert(is_whitespace(c));
                obj_offset = skip_whitespace(pdf_file.file, obj_offset);

                assert(obj_offset < len);
                assert_string_at(pdf_file.file, obj_offset, "obj");

                assert(obj_offset + 3 < len);
                assert(is_whitespace(contents[obj_offset + 3]));
                obj_offset = skip_whitespace(pdf_file.file, obj_offset + 3);

                assert(obj_offset < len);

                ssize_t obj_end;
                struct pdf_object return_obj =
                    read_object_at(pdf_file.file, obj_offset, arena, &obj_end);

                if (return_obj.type != OBJ_STREAM ||
                    return_obj.stream.length_type != LT_INDIRECT) {
                    assert(obj_end < len);
                    ssize_t endobj_start =
                        skip_whitespace(pdf_file.file, obj_end);
                    assert_string_at(pdf_file.file, endobj_start, "endobj");
                }

                return return_obj;
            }
        }

        if (cur_xref.prev == NULL) {
            break;
        }
        cur_xref = *cur_xref.prev;
    }

    assert(0 && "Invalid object number");
}

static inline struct pdf_object
get_ind_obj_from_dict(struct pdf_file pdf_file, struct pdf_dict const dict,
                      struct pdf_name const name) {
    assert(has_entry(dict, name));
    struct pdf_object ref_obj = get_entry(dict, name);
    assert(ref_obj.type == OBJ_IND_REF);

    struct pdf_ind_ref ref = ref_obj.ref;
    struct pdf_object target = get_obj_ref(pdf_file, ref);
    return target;
}

static inline struct pdf_dict assert_dict_type(struct pdf_object dict_obj,
                                               struct pdf_name const exp_type) {
    assert(dict_obj.type == OBJ_DICT);

    struct pdf_dict dict = dict_obj.dict;
    assert(has_entry(dict, type_name));

    struct pdf_object type_obj = get_entry(dict, type_name);
    struct pdf_object exp_name_obj = {.type = OBJ_NAME, .name = exp_type};

    assert(objects_equal(type_obj, exp_name_obj));

    return dict;
}

static void print_stream_contents(struct file file, struct pdf_stream stream,
                                  ssize_t len) {
    if (has_entry(stream.meta, filter_name)) {
        struct pdf_object filter_obj = get_entry(stream.meta, filter_name);
        if (filter_obj.type == OBJ_ARRAY) {
            assert(0 && "Unimplemented");
        }

        assert(filter_obj.type == OBJ_NAME);

        struct pdf_object flate_decode_obj = {
            .type = OBJ_NAME,
            .name = flate_decode_name,
        };

        if (!objects_equal(filter_obj, flate_decode_obj)) {
            assert(0 && "Unimplemented");
        }

#define BUF_LEN (1 << 20)
        char out_buf[BUF_LEN] = {0};

        Bytef *contents = (Bytef *)file.contents;
        z_stream ztream = {
            .next_in = contents + stream.offset,
            .avail_in = len,
            .next_out = (Bytef *)out_buf,
            .avail_out = BUF_LEN,
            .zalloc = NULL,
            .zfree = NULL,
            .opaque = NULL,
        };
#undef BUF_LEN

        assert(Z_OK == inflateInit(&ztream));
        int err = inflate(&ztream, Z_NO_FLUSH);
        printf("got err %d\n", err);
        assert(Z_STREAM_END == err);
        assert(Z_OK == inflateEnd(&ztream));

        printf("Contents (len: %lu):\n%.*s\n", ztream.total_out,
               (int)ztream.total_out, out_buf);
    }
}

int main(int argc, char const *argv[]) {
    assert(argc > 1 && "Expected filename");

    char const *filename = argv[1];
    int fd = open(filename, O_RDONLY);
    if (fd == -1) {
        handle_error("Failed to open file");
    }

    struct stat stat_buf;
    int stat_ret = stat(filename, &stat_buf);
    if (stat_ret == -1) {
        handle_error("Failed to stat file");
    }

    ssize_t file_size = stat_buf.st_size;
    void *mapped = mmap(NULL, file_size, PROT_READ, MAP_PRIVATE, fd, 0);

    struct file file = {.contents = mapped, .len = file_size};

    struct version pdf_version = read_version(file);
    ssize_t last_eof = get_last_eof_from(file, file.len);
    ssize_t byte_off_start = prev_line_start(file, last_eof);
    assert_string_at(file, prev_line_start(file, byte_off_start), "startxref");

    long xrefstart = atol(file.contents + byte_off_start);
    assert(xrefstart > 0);

    struct arena *arena = alloc_arena();
    assert(arena != NULL);
    assert(arena_begin(arena));

    struct xref xref = read_xref(file, xrefstart, arena);

    struct pdf_file pdf_file = {
        .file = file,
        .xref = xref,
        .arena = arena,
    };

    struct pdf_object root_obj =
        get_ind_obj_from_dict(pdf_file, xref.trailer, root_name);
    struct pdf_dict root_dict = assert_dict_type(root_obj, catalog_name);

    struct pdf_object pages_obj =
        get_ind_obj_from_dict(pdf_file, root_dict, pages_name);
    struct pdf_dict pages_dict = assert_dict_type(pages_obj, pages_name);

    assert(has_entry(pages_dict, count_name));
    struct pdf_object count_obj = get_entry(pages_dict, count_name);

    long page_count;
    if (count_obj.type == OBJ_NUMBER) {
        assert(count_obj.number.type == NT_INTEGER);
        page_count = count_obj.number.integer;
    } else {
        assert(count_obj.type == OBJ_IND_REF);
        struct pdf_ind_ref num_ref = count_obj.ref;
        struct pdf_object rcount_obj = get_obj_ref(pdf_file, num_ref);
        assert(rcount_obj.number.type == NT_INTEGER);
        page_count = rcount_obj.number.integer;
    }

    printf("The pdf %s has %ld pages.\n", filename, page_count);

    if (page_count == 1) {
        assert(has_entry(pages_dict, kids_name));
        struct pdf_object kids_obj = get_entry(pages_dict, kids_name);
        assert(kids_obj.type == OBJ_ARRAY);
        struct pdf_array kids_arr = kids_obj.array;
        assert(kids_arr.count == 1);
        struct pdf_object pone_obj_ref = kids_arr.objects[0];

        assert(pone_obj_ref.type == OBJ_IND_REF);
        struct pdf_object pone_obj = get_obj_ref(pdf_file, pone_obj_ref.ref);
        struct pdf_dict pone_dict = assert_dict_type(pone_obj, page_name);

        struct pdf_object contents_obj =
            get_ind_obj_from_dict(pdf_file, pone_dict, contents_name);

        printf("The contents has type %d. ", contents_obj.type);
        if (contents_obj.type == OBJ_STREAM) {
            struct pdf_stream stream = contents_obj.stream;

            ssize_t stream_len;
            switch (stream.length_type) {
            case LT_DIRECT: {
                stream_len = stream.direct;
            } break;
            case LT_INDIRECT: {
                struct pdf_ind_ref length_ref = stream.indirect;
                struct pdf_object length_obj =
                    get_obj_ref(pdf_file, length_ref);
                assert(length_obj.type == OBJ_NUMBER &&
                       length_obj.number.type == NT_INTEGER);

                stream_len = length_obj.number.integer;
            } break;
            }
            printf("The stream length is %ld\n", stream_len);

            print_stream_contents(file, stream, stream_len);
        }
    }

    arena_pop(arena);

    munmap(mapped, file_size);
    close(fd);

    return EXIT_SUCCESS;
}
