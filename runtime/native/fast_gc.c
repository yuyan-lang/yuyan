#include <stdlib.h>
#include <string.h>
#include "native_include.h"
#include <inttypes.h>


#define BUFFER_SIZE 12684354560ULL // 12560 MB

void** buffers = NULL;
uint64_t buffer_count = 0;
uint64_t current_buffer_index = 0;
uint64_t buffer_size = 0;
uint64_t current_offset = 0;

void yy_fastgc_init() {
    buffer_count = 1;
    buffers = malloc(sizeof(void*) * buffer_count);
    buffers[0] = malloc(BUFFER_SIZE);
    buffer_size = BUFFER_SIZE;
    current_offset = 0;
}

void* yy_fastgc_malloc(uint64_t size) {
    if (size > BUFFER_SIZE) {
        fprintf(stderr, "Requested allocation size (%" PRIu64 " bytes) exceeds buffer size limit (%llu bytes).\n",
                size, BUFFER_SIZE);
        abort();
    }

    if (current_offset + size > buffer_size) {
        // Out of memory in the current buffer, allocate a new buffer
        current_buffer_index++;
        if (current_buffer_index >= buffer_count) {
            // Allocate more buffer pointers if needed
            buffer_count *= 2;
            buffers = realloc(buffers, sizeof(void*) * buffer_count);
        }

        buffers[current_buffer_index] = malloc(BUFFER_SIZE);
        buffer_size = BUFFER_SIZE;
        current_offset = 0;
    }

    void* ptr = (char*)buffers[current_buffer_index] + current_offset;
    current_offset += size;

    return ptr;
}


void* yy_fastgc_realloc(void* ptr, uint64_t size) {
    errorAndAbort("FASTGC REALLOC NOT IMPLEMENTED\n");
    return NULL;
}
