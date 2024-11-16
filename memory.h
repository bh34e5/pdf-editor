#pragma once

#include <stdbool.h>
#include <stdint.h>

struct arena;

struct arena *alloc_arena(void);
void free_arena(struct arena *arena);

#define ARENA_PUSH_N(type, arena, n)                                           \
    ((type *)arena_push_size((arena), ((n) * sizeof(type)), 1))

bool arena_begin(struct arena *arena);
void *arena_push_size(struct arena *arena, uint32_t size, bool clear);
void arena_pop(struct arena *arena);
