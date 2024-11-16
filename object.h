#pragma once

#include <stdbool.h>
#include <sys/types.h>

struct arena;
struct file;

struct pdf_ind_ref {
    long obj_num;
    long gen_num;
};

enum number_type {
    NT_REAL,
    NT_INTEGER,
};

struct pdf_number {
    enum number_type type;
    union {
        double real;
        long integer;
    };
};

struct pdf_name {
    char const *text;
    ssize_t len;
};

enum string_type {
    ST_PAREN,
    ST_BRACE,
};

struct pdf_string {
    enum string_type type;
    char const *text;
    ssize_t len;
};

struct pdf_array {
    ssize_t count;
    struct pdf_object *objects;
};

struct pdf_dict {
    ssize_t count;
    struct pdf_name *names;
    struct pdf_object *objects;
};

struct pdf_stream {
    struct pdf_dict meta;
    ssize_t offset;
};

enum object_type {
    OBJ_NULL,
    OBJ_BOOL,
    OBJ_NUMBER,
    OBJ_NAME,
    OBJ_STRING,
    OBJ_ARRAY,
    OBJ_DICT,
    OBJ_STREAM,
    OBJ_IND_REF,
};

struct pdf_object {
    enum object_type type;
    union {
        void *null;
        bool b;
        struct pdf_number number;
        struct pdf_name name;
        struct pdf_string string;
        struct pdf_array array;
        struct pdf_dict dict;
        struct pdf_stream stream;
        struct pdf_ind_ref ref;
    };
};

struct pdf_object read_object_at(struct file file, ssize_t offset,
                                 struct arena *arena, ssize_t *end);

bool objects_equal(struct pdf_object lhs, struct pdf_object rhs);

bool has_entry(struct pdf_dict dict, struct pdf_name name);
struct pdf_object get_entry(struct pdf_dict dict, struct pdf_name name);
