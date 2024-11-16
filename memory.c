#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "memory.h"

#define MEGABYTES(n) (n << 20)

#define BASE_SIZE MEGABYTES(4)
#define ARENA_END(a) ((a)->bytes + BASE_SIZE)

struct header {
    struct header *prev;
    char *next_byte;
};

struct arena {
    struct header *cur;
    char bytes[BASE_SIZE];
};

struct arena *alloc_arena(void) {
    // zero allocate the information
    struct arena *arena = (struct arena *)calloc(1, sizeof(*arena));

    arena->cur = (struct header *)&arena->bytes;
    *arena->cur = (struct header){
        .prev = NULL,
        .next_byte = (char *)(arena->cur + 1),
    };

    return arena;
}

void free_arena(struct arena *arena) { free(arena); }

bool arena_begin(struct arena *arena) {
    if ((arena == NULL) ||
        ((arena->cur->next_byte + sizeof(struct header)) >= ARENA_END(arena))) {
        return false;
    }

    struct header *next_header = (struct header *)(arena->cur->next_byte);
    *next_header = (struct header){
        .prev = arena->cur,
        .next_byte = (char *)(next_header + 1),
    };
    arena->cur = next_header;

    return true;
}

void *arena_push_size(struct arena *arena, uint32_t size, bool clear) {
    if ((arena == NULL) ||
        ((arena->cur->next_byte + size) >= ARENA_END(arena))) {
        return NULL;
    }

    void *res = (void *)arena->cur->next_byte;
    arena->cur->next_byte += size;

    if (clear) {
        memset(res, 0, size);
    }

    return res;
}

void arena_pop(struct arena *arena) {
    if (arena == NULL || arena->cur->prev == NULL) {
        // either no arena or empty arena
        // TODO(bhester): make this an error?
        return;
    }

    arena->cur = arena->cur->prev;
}
