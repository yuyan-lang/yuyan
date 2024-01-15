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


// void* buffer = NULL;
// uint64_t buffer_size = 0;
// uint64_t current_offset = 0;

// void yy_fastgc_init(uint64_t initial_size) {
//     buffer = malloc(initial_size);
//     buffer_size = initial_size;
//     current_offset = 0;
// }

// void* yy_fastgc_malloc(uint64_t size) {
//     if (current_offset + size > buffer_size) {
//         // Out of memory, resize the buffer
//         uint64_t new_size = buffer_size * 2;
//         while (current_offset + size > new_size) {
//             new_size *= 2;
//         }
//         void* new_buffer = realloc(buffer, new_size);
//         if (new_buffer == NULL) {
//             // Handle out-of-memory error
//             return NULL;
//         }

//         buffer = new_buffer;
//         buffer_size = new_size;
//     }

//     void* ptr = (char*)buffer + current_offset;
//     current_offset += size;

//     printf("Allocated Pointer: %p, Current Offset: %lu, Buffer Size: %lu, Buffer Address: %p\n",
//         ptr, current_offset, buffer_size, buffer);
//     return ptr;
// }

void* yy_fastgc_realloc(void* ptr, uint64_t size) {
    errorAndAbort("FASTGC REALLOC NOT IMPLEMENTED\n");
    return NULL;
    // if (ptr == NULL) {
    //     // Treat NULL pointer as malloc
    //     return yy_fastgc_malloc(size);
    // }

    // // Allocate a new buffer
    // void* new_buffer = yy_fastgc_malloc(size);
    // if (new_buffer == NULL) {
    //     // Handle out-of-memory error
    //     return NULL;
    // }

    // // Copy the content from the original buffer
    // int64_t old_size = current_offset - ((char*)ptr - (char*)buffer);
    // memcpy(new_buffer, ptr, old_size);

    // // Update the buffer pointers and size
    // buffer = new_buffer;
    // buffer_size = current_offset + size;

    // return new_buffer;
}
