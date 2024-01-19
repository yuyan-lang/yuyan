#include "native_include.h"
/*
complete this file, replace error and abort with actual implementation. I want a garbage collector that mallocs 512 MB blocks (the minor heap) from C runtime, and distribute it to user. We should also allocate a 512 MB block as a major heap. Upon getting a byte size, always round up to a multiples of 8, plus a header block before. For example, if the user wants 10 bytes, allocate 24 bytes (3 pointer sizes) from memory, and write a header. The first 3 bytes of a header is a magic number that should rarely appear as a string -- use something that is disjoint from the ASCII table, the next 1 byte is empty and the lower 4 bytes is an unsigned integer representing the length of the allocation block. The data stored in allocated things maybe pointers or others, and the way we check if the data is a pointer is to see if it resides in the 512MB block boundary, and its header has the magic number, and its size is within boundary. If so, it is treated as a pointer, otherwise it is treated as regular data. If spaces run out, we first check if the free space of the major heap is less than 512 MB, the minor heap size. If the space left on the major heap is enough, perform a copy and compaction gc copying the data from the minor heap to the major heap over starting with the root points, and contents already in the major heap. If the major heap doesn't have enough free space, realloate major heap to be twice as large and continue the copying process. The root points should be realized as a static array of 1000 elements. We may register only 1000 root points and errorAndAbort on over registration. Also, there is a extern void** stack and void** stack_ptr whose content should also be treated as root points. Needless to say, if the content of the stack and the gc root points fail to be a pointer, it is not a pointer. We never have pointers that point to the middle of the allocated heap. Thus, give a warning if there is a supposed pointer that points to allcoated memory but the magic number in the header doesn't match up. Before you produce any code, first ask any clarification questions. If everything is clear, simply say "I am ready to produce the code".
*/

#ifndef ROOT_POINTS_LIMIT
#define ROOT_POINTS_LIMIT 1000
#endif

void* gc_root_points[ROOT_POINTS_LIMIT] = {0};
int gc_root_points_count = 0;

void yy_register_gc_rootpoint(void* ptr) {
#ifdef OLD_ENTRY
    return;
#else
    if (gc_root_points_count >= ROOT_POINTS_LIMIT) {
        errorAndAbort("Root points limit exceeded");
    }
    gc_root_points[gc_root_points_count++] = ptr;
#endif

}

#ifndef INITIAL_HEAP_SIZE
#define INITIAL_HEAP_SIZE (512 * 1024 * 1024)  // 512 MB
#endif

#ifndef TINY_HEAP_SIZE
#define TINY_HEAP_SIZE (32 * 1024 * 1024)  // 32 MB
#endif

uint64_t* current_heap = NULL;
uint64_t* tiny_heap = NULL;
uint64_t current_heap_size = INITIAL_HEAP_SIZE;
uint64_t current_heap_offset = 0;
uint64_t tiny_heap_offset = 0;
bool should_expand_heap = false;


void yy_gc_init() {
    current_heap = malloc(INITIAL_HEAP_SIZE);
    if (!current_heap) errorAndAbort("Failed to initialize major heap");
    
    tiny_heap = malloc(TINY_HEAP_SIZE);
    if (!tiny_heap) errorAndAbort("Failed to initialize tiny heap");

}

static uint64_t MAGIC_NUMBER = 0xFAFBFC;

bool check_magic_number(uint64_t header) {
    return ((header >> 32) & 0xFFFFFF) == MAGIC_NUMBER;
}

uint64_t get_ptr_size(uint64_t header) {
    return (header & 0xFFFFFFFF);
}

bool is_a_pointer(void* raw_ptr) {

    uint64_t* ptr = (uint64_t*)raw_ptr;
    if (ptr - current_heap > 0 && ptr - current_heap < current_heap_size) {
        uint64_t header = *(ptr - 1);
        return check_magic_number(header) && get_ptr_size(header) <= current_heap_size;
    } else if (ptr - tiny_heap > 0 && ptr - tiny_heap < TINY_HEAP_SIZE) {
        uint64_t header = *(ptr - 1);
        return check_magic_number(header) && get_ptr_size(header) <= TINY_HEAP_SIZE;
    } else {
        return false;
    }
}

void* allocate_memory_with_implicit_header(uint64_t array_size, uint64_t* heap_base, uint64_t heap_size, uint64_t* heap_offset) {
    // Allocate memory block from the current heap
    size_t total_size = array_size + 1;

    if (*heap_offset + total_size > heap_size) {
        return NULL;  // Cannot allocate from this heap
    }
  
    *heap_offset += total_size;

    uint64_t *block = heap_base + *heap_offset;

    uint64_t header_value = 0;
    header_value |= (MAGIC_NUMBER << 32);
    header_value |= array_size;
    *block = header_value;

    return (void*)(block + 1);
}

void* yy_gc_malloc_bytes(uint64_t size) {
    if (size == 0) {
        return NULL;
    }

    uint64_t array_size = size / sizeof(uint64_t) + (size % sizeof(uint64_t) != 0);

    void* block = allocate_memory_with_implicit_header(array_size, current_heap, current_heap_size, &current_heap_offset);
    if (block != NULL) {
        return block;
    }
    
    // If no space left in the minor heap, try allocating from the tiny heap
    block = allocate_memory_with_implicit_header(size, tiny_heap, TINY_HEAP_SIZE, &tiny_heap_offset);
    if (block != NULL) {
        return block;
    }

    // If no space is available, trigger a garbage collection
    // This should not happen directly if a GC cannot be performed yet; handle appropriately
    errorAndAbort("No space left and garbage collection cannot be performed yet");
    return NULL;  // This line will not be executed because errorAndAbort exits the program
}


void* yy_gc_realloc_bytes(void* ptr, uint64_t size) {
    if (ptr == NULL) {
        return yy_gc_malloc_bytes(size);
    } else {
        uint64_t header = *((uint64_t*)ptr - 1);
        uint64_t ptr_size = get_ptr_size(header);
        if (ptr_size >= size) {
            return ptr;
        } else {
            void* new_ptr = yy_gc_malloc_bytes(size);
            if (new_ptr == NULL) {
                return NULL;
            }
            memcpy(new_ptr, ptr, ptr_size);
            return new_ptr;
        }
    }

}

void copy_root_point(void* ptr, uint64_t* new_heap, uint64_t* new_heap_offset){
    if (is_a_pointer(ptr)) {
        uint64_t header = *((uint64_t*)ptr - 1);
        uint64_t ptr_size = get_ptr_size(header);
        if (*new_heap_offset + ptr_size + 1 > current_heap_size) {
            errorAndAbort("No space left during GC, increase GC factor");
        }
        memcpy(new_heap + *new_heap_offset, ((uint64_t*)ptr - 1), ptr_size + 1);
        *new_heap_offset += ptr_size + 1;
    }
}


void yy_perform_gc(void* additional_root_point) {
    if (tiny_heap_offset == 0) {
        return;  // No garbage collection needed
    }

    uint64_t *new_heap;
    if (should_expand_heap) {
        // Expand the heap
        new_heap = malloc(current_heap_size * 2);
        if (!new_heap) errorAndAbort("Failed to expand major heap");
        current_heap_size *= 2;
        should_expand_heap = false;
    } else {
        // Reuse the current heap
        new_heap = malloc(current_heap_size);
    }

    uint64_t *scan_ptr = new_heap;
    uint64_t new_heap_offset = 0;

    // copy the root points
    for (int i = 0; i < gc_root_points_count; i++) {
        copy_root_point(gc_root_points[i], new_heap, &new_heap_offset);
    }
    yy_ptr* stack_ptr_gc = (yy_ptr*)stack;
    while(stack_ptr_gc < stack_ptr){
        copy_root_point((void*)(*stack_ptr_gc), new_heap, &new_heap_offset);
        stack_ptr_gc++;
    }
    copy_root_point(additional_root_point, new_heap, &new_heap_offset);

    while (scan_ptr < new_heap + new_heap_offset) {
        copy_root_point((void*)(*scan_ptr), new_heap, &new_heap_offset);
    }

    // we're all done
    free(current_heap);
    current_heap = new_heap;
    current_heap_offset = new_heap_offset;

    if ((double)new_heap_offset / current_heap_size > 0.75) {
        should_expand_heap = true;
    }
    

    // Clean up tiny heap at the end of a successful garbage collection
    tiny_heap_offset = 0;
}


