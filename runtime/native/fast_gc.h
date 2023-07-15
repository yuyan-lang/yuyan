

#ifndef YY_FASTGC_H
#define YY_FASTGC_H

#include <stdlib.h>
#include <string.h>


void yy_fastgc_init();

void* yy_fastgc_malloc(uint64_t size);

// void* yy_fastgc_realloc(void* ptr, uint64_t size);

#endif /* YY_FASTGC_H */
