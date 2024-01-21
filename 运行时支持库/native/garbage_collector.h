

#ifndef YY_GC_H
#define YY_GC_H

#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include "common_include.h"

void yy_gc_init();

void yy_register_gc_rootpoint(yyvalue* ptr);

void* yy_gc_malloc_bytes(uint64_t size);

void* yy_gc_realloc_bytes(void* ptr, uint64_t old_size, uint64_t new_size);
void yy_perform_gc(yyvalue* additional_root_point);

#endif /* YY_FASTGC_H */
