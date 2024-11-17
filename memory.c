#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "memory.h"

#define MEGABYTES(n) (n << 20)

#define BASE_SIZE MEGABYTES(4)
#define ARENA_END(a) ((a)->bytes + BASE_SIZE)

struct allocation {
    struct allocation *prev;
    uint32_t size;
    char *start;
};

struct header {
    struct header *prev;
    struct allocation *last_allocation;
    char *next_byte;
};

struct arena {
    struct header *cur;
    char bytes[BASE_SIZE];
};

struct arena *alloc_arena(void) {
    // zero allocate the information
    struct arena *arena = (struct arena *)calloc(1, sizeof(*arena));
    if (arena == NULL) {
        return NULL;
    }

    struct header *first_header = (struct header *)&arena->bytes;

    *first_header = (struct header){
        .prev = NULL,
        .last_allocation = NULL,
        .next_byte = (char *)(first_header + 1),
    };

    arena->cur = first_header;
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
        .last_allocation = NULL,
        .next_byte = (char *)(next_header + 1),
    };

    arena->cur = next_header;

    return true;
}

void *arena_push_size(struct arena *arena, uint32_t size, bool clear) {
    uint32_t alloc_size = sizeof(struct allocation) + size;

    if ((arena == NULL) ||
        ((arena->cur->next_byte + alloc_size) >= ARENA_END(arena))) {
        return NULL;
    }

    struct header *cur_header = arena->cur;
    struct allocation *next_alloc = (struct allocation *)cur_header->next_byte;

    *next_alloc = (struct allocation){
        .prev = cur_header->last_allocation,
        .size = size,
        .start = (char *)(next_alloc + 1),
    };

    cur_header->last_allocation = next_alloc;
    cur_header->next_byte += alloc_size;

    void *res = (void *)next_alloc->start;

    if (clear) {
        memset(res, 0, size);
    }

    return res;
}

void *arena_realloc_size(struct arena *arena, void *ptr, uint32_t new_size,
                         bool clear) {
    if (arena == NULL) {
        return NULL;
    }

    if (ptr == NULL) {
        return arena_push_size(arena, new_size, clear);
    }

    struct header *cur_header = arena->cur;
    struct allocation *last_alloc = cur_header->last_allocation;

    if ((last_alloc == NULL) || ((void *)last_alloc->start != ptr)) {
        return NULL;
    }

    if (new_size == last_alloc->size) {
        return ptr;
    } else if (new_size > last_alloc->size) {
        if ((last_alloc->start + new_size) >= ARENA_END(arena)) {
            return NULL;
        }

        cur_header->next_byte += new_size - last_alloc->size;
        last_alloc->size = new_size;
        return ptr;
    } else {
        if (new_size == 0) {
            uint32_t alloc_size = sizeof(struct allocation) + last_alloc->size;

            cur_header->last_allocation = last_alloc->prev;
            cur_header->next_byte -= alloc_size;

            return ptr; // to indicate success, but it shouldn't be used.
        }

        cur_header->next_byte += last_alloc->size - new_size;
        last_alloc->size = new_size;
        return ptr;
    }
}

void arena_pop(struct arena *arena) {
    if (arena == NULL || arena->cur->prev == NULL) {
        // either no arena or empty arena
        // TODO(bhester): make this an error?
        return;
    }

    arena->cur = arena->cur->prev;
}
