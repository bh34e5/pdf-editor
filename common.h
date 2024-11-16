#pragma once

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>

struct file {
    char const *contents;
    ssize_t len;
};

#define handle_error(msg)                                                      \
    do {                                                                       \
        perror(msg);                                                           \
        exit(EXIT_FAILURE);                                                    \
    } while (0)

#define assert_char_at(contents, len, ind, c)                                  \
    ({                                                                         \
        ssize_t __ind = (ind);                                                 \
        assert(__ind < (len));                                                 \
        char __at = (contents)[__ind];                                         \
        assert(__at == (c));                                                   \
        __at;                                                                  \
    })

#define is_digit(c)                                                            \
    ({                                                                         \
        char __c = (c);                                                        \
        '0' <= __c &&__c <= '9';                                               \
    })

#define is_octal_digit(c)                                                      \
    ({                                                                         \
        char __c = (c);                                                        \
        '0' <= __c &&__c <= '7';                                               \
    })

#define is_hex_char(c)                                                         \
    ({                                                                         \
        char __c = (c);                                                        \
        ('0' <= __c && __c <= '9') || ('a' <= __c && __c <= 'f') ||            \
            ('A' <= __c && __c <= 'F');                                        \
    })

#define assert_string_at(file, ind, str)                                       \
    ({                                                                         \
        char const __str[] = (str);                                            \
        ssize_t __len = sizeof(__str) - 1;                                     \
        _assert_string_at((file), (ind), __str, __len);                        \
    })

static inline void _assert_string_at(struct file file, ssize_t ind,
                                     char const *str, ssize_t s_len) {
    for (ssize_t s_ind = 0; s_ind < s_len; ++s_ind, ++ind) {
        assert_char_at(file.contents, file.len, ind, str[s_ind]);
    }
}

#define string_matches_at(file, ind, str)                                      \
    ({                                                                         \
        char const __str[] = (str);                                            \
        ssize_t __len = sizeof(__str) - 1;                                     \
        _string_matches_at((file), (ind), __str, __len);                       \
    })

static bool _string_matches_at(struct file file, ssize_t ind, char const *str,
                               ssize_t s_len) {
    if (ind + s_len >= file.len) {
        return false;
    }

    for (ssize_t s_ind = 0; s_ind < s_len; ++s_ind, ++ind) {
        if (file.contents[ind] != str[s_ind]) {
            return false;
        }
    }
    return true;
}

static inline bool is_whitespace(char c) {
    switch (c) {
    case 0x00: // nul
    case 0x09: // tab
    case 0x0A: // line feed
    case 0x0C: // form feed
    case 0x0D: // carriage return
    case 0x20: // space
        return true;
    default:
        return false;
    }
}

static inline ssize_t skip_whitespace(struct file file, ssize_t start) {
    while (start < file.len && is_whitespace(file.contents[start])) {
        ++start;
    }
    return start;
}

static ssize_t prev_line_start(struct file file, ssize_t line_start) {
    assert(line_start > 0);

    char line_ending = file.contents[line_start - 1];

    ssize_t prev_line_end;
    if (line_start > 1 && line_ending == '\n' &&
        file.contents[line_start - 2] == '\r') {
        prev_line_end = line_start - 2;
    } else {
        prev_line_end = line_start - 1;
    }

    for (ssize_t i = prev_line_end; i > 0; --i) {
        char c = file.contents[i - 1];
        if (c == '\r' || c == '\n') {
            return i;
        }
    }
    assert(!"There was no previous line");
}

static ssize_t next_line_start(struct file file, ssize_t line_start) {
    assert(line_start < file.len);

    char const *contents = file.contents;
    ssize_t len = file.len;

    for (ssize_t i = line_start; i < len; ++i) {
        char c = contents[i];
        if (c == '\r') {
            assert(i + 1 < len);
            if (contents[i + 1] == '\n') {
                assert(i + 2 < len);
                return i + 2;
            } else {
                return i + 1;
            }
        } else if (c == '\n') {
            assert(i + 1 < len);
            return i + 1;
        }
    }
    assert(!"There was no end of line");
}
