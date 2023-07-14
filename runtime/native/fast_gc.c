#include <stdlib.h>
#include <string.h>
#include "native_include.h"

void* buffer = NULL;
uint64_t buffer_size = 0;
uint64_t current_offset = 0;

void yy_fastgc_init(uint64_t initial_size) {
    buffer = malloc(initial_size);
    buffer_size = initial_size;
    current_offset = 0;
}

void* yy_fastgc_malloc(uint64_t size) {
    if (current_offset + size > buffer_size) {
        // Out of memory, resize the buffer
        uint64_t new_size = buffer_size * 2;
        while (current_offset + size > new_size) {
            new_size *= 2;
        }
        void* new_buffer = realloc(buffer, new_size);
        if (new_buffer == NULL) {
            // Handle out-of-memory error
            return NULL;
        }

        buffer = new_buffer;
        buffer_size = new_size;
    }

    void* ptr = (char*)buffer + current_offset;
    current_offset += size;
    return ptr;
}

// void* yy_fastgc_realloc(void* ptr, int64_t size) {
//     if (ptr == NULL) {
//         // Treat NULL pointer as malloc
//         return yy_fastgc_malloc(size);
//     }

//     // Allocate a new buffer
//     void* new_buffer = yy_fastgc_malloc(size);
//     if (new_buffer == NULL) {
//         // Handle out-of-memory error
//         return NULL;
//     }

//     // Copy the content from the original buffer
//     int64_t old_size = current_offset - ((char*)ptr - (char*)buffer);
//     memcpy(new_buffer, ptr, old_size);

//     // Update the buffer pointers and size
//     buffer = new_buffer;
//     buffer_size = current_offset + size;

//     return new_buffer;
// }
