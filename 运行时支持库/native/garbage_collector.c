#include "garbage_collector.h"
#include "memory_verifier.h"
/*
complete this file, replace error and abort with actual implementation. I want a garbage collector that mallocs 512 MB blocks (the minor heap) from C runtime, and distribute it to user. We should also allocate a 512 MB block as a major heap. Upon getting a byte size, always round up to a multiples of 8, plus a header block before. For example, if the user wants 10 bytes, allocate 24 bytes (3 pointer sizes) from memory, and write a header. The first 3 bytes of a header is a magic number that should rarely appear as a string -- use something that is disjoint from the ASCII table, the next 1 byte is empty and the lower 4 bytes is an unsigned integer representing the length of the allocation block. The data stored in allocated things maybe pointers or others, and the way we check if the data is a pointer is to see if it resides in the 512MB block boundary, and its header has the magic number, and its size is within boundary. If so, it is treated as a pointer, otherwise it is treated as regular data. If spaces run out, we first check if the free space of the major heap is less than 512 MB, the minor heap size. If the space left on the major heap is enough, perform a copy and compaction gc copying the data from the minor heap to the major heap over starting with the root points, and contents already in the major heap. If the major heap doesn't have enough free space, realloate major heap to be twice as large and continue the copying process. The root points should be realized as a static array of 1000 elements. We may register only 1000 root points and errorAndAbort on over registration. Also, there is a extern void** stack and void** stack_ptr whose content should also be treated as root points. Needless to say, if the content of the stack and the gc root points fail to be a pointer, it is not a pointer. We never have pointers that point to the middle of the allocated heap. Thus, give a warning if there is a supposed pointer that points to allcoated memory but the magic number in the header doesn't match up. Before you produce any code, first ask any clarification questions. If everything is clear, simply say "I am ready to produce the code".
*/

#ifndef ROOT_POINTS_LIMIT
#define ROOT_POINTS_LIMIT 1000
#endif

yyvalue* gc_root_points[ROOT_POINTS_LIMIT] = {0};
int gc_root_points_count = 0;

// must pass pointers to gc root points, a dereference of it will be a gc root point
void yy_register_gc_rootpoint(yyvalue* ptr) {
    if (gc_root_points_count >= ROOT_POINTS_LIMIT) {
        errorAndAbort("Root points limit exceeded");
    }
    gc_root_points[gc_root_points_count++] = ptr;
}

// #ifndef INITIAL_HEAP_SIZE
// #define INITIAL_HEAP_SIZE (64 * 1024 * 1024)  // 1024 MB
// #define INITIAL_HEAP_SIZE (2 * 1024 * 1024)  // 32 MB
// #define INITIAL_HEAP_SIZE (2 * 1024)  // 32 KB
// #endif


yyvalue* current_heap = NULL;
yyvalue* tiny_heap = NULL;
yyvalue* new_heap = NULL;
uint64_t initial_heap_size = (2 * 1024 * 1024); //32 MB
uint64_t current_heap_size = 0;
uint64_t current_heap_offset = 0;
uint64_t tiny_heap_offset = 0;
uint64_t new_heap_size = 0;
bool should_expand_heap = false;

// #define MALLOC_FUNC(X) malloc(X * sizeof(yyvalue))
#define MALLOC_FUNC(X) calloc(sizeof(yyvalue), X)

bool yy_gc_debug_flag = false;

void yy_gc_init() {
    current_heap_size = initial_heap_size;
    current_heap = MALLOC_FUNC(initial_heap_size);
    if (!current_heap) errorAndAbort("Failed to initialize major heap");
    
    tiny_heap = MALLOC_FUNC(TINY_HEAP_SIZE);
    if (!tiny_heap) errorAndAbort("Failed to initialize tiny heap");

    yy_gc_debug_flag = (getenv("YY_GC_DEBUG_FLAG") != NULL && strcmp(getenv("YY_GC_DEBUG_FLAG"), "1") == 0);
}

static uint64_t MAGIC_NUMBER = 0xFAFBFC;

bool check_magic_number(uint64_t header) {
    return ((header >> 32) & 0xFFFFFF) == MAGIC_NUMBER;
}

uint64_t get_ptr_size(uint64_t header) {
    return (header & 0xFFFFFFFF);
}

bool is_an_old_pointer(yyvalue raw_ptr) {
    // fprintf(stderr, "\nchecking is_a_pointer: %p ", raw_ptr);
    // fflush(stderr);
    if (yyvalue_get_type(raw_ptr) != type_tuple){
        return false;
    } else {
        yyvalue* ptr = yyvalue_to_tuple(raw_ptr);
        if (ptr - current_heap >= 0 && ptr - current_heap < current_heap_size) {
            assert(yyvalue_get_length(raw_ptr) <= current_heap_size);
            return true;
        }
        else if (ptr - tiny_heap >= 0 && ptr - tiny_heap < TINY_HEAP_SIZE)
        {
            assert(yyvalue_get_length(raw_ptr) <= TINY_HEAP_SIZE);
            return true;
        }
        else
        {
            return false;
        }
    }
}


bool is_an_new_pointer(yyvalue raw_ptr) {
    if (yyvalue_get_type(raw_ptr) != type_tuple){
        return false;
    } else {
        yyvalue* ptr = yyvalue_to_tuple(raw_ptr);
        if (ptr - new_heap >= 0 && ptr - new_heap < new_heap_size) {
            assert(yyvalue_get_length(raw_ptr) <= new_heap_size);
            return true;
        }
        else
        {
            return false;
        }
    }
}

yyvalue* allocate_memory_without_implicit_header(uint64_t array_size, yyvalue* heap_base, uint64_t heap_size, uint64_t* heap_offset) {
    // Allocate memory block from the current heap
    size_t total_size = array_size;

    if (*heap_offset + total_size > heap_size) {
        return NULL;  // Cannot allocate from this heap
    }
  
    yyvalue *block = heap_base + *heap_offset;

    *heap_offset += total_size;

    return block;
}

void* yy_gc_malloc_bytes(uint64_t size) {
    if (size == 0) {
        return NULL;
    }

    uint64_t array_size = size / sizeof(yyvalue) + (size % sizeof(yyvalue) != 0);

    void* block = allocate_memory_without_implicit_header(array_size, current_heap, current_heap_size, &current_heap_offset);
    if (block != NULL) {
        // fprintf(stderr, "Allocated from %ld bytes at %p - %p ", size, block, block + array_size);
        return block;
    }
    
    // If no space left in the minor heap, try allocating from the tiny heap
    block = allocate_memory_without_implicit_header(size, tiny_heap, TINY_HEAP_SIZE, &tiny_heap_offset);
    if (block != NULL) {
        // fprintf(stderr, "Allocating from minor heap %p ", block);
        return block;
    }

    // If no space is available, trigger a garbage collection
    // This should not happen directly if a GC cannot be performed yet; handle appropriately
    errorAndAbort("No space left and garbage collection cannot be performed yet");
    return NULL;  // This line will not be executed because errorAndAbort exits the program
}


void* yy_gc_realloc_bytes(void* ptr, uint64_t old_size, uint64_t new_size) {
    if (ptr == NULL) {
        return yy_gc_malloc_bytes(new_size);
    } else {
        if (old_size >= new_size) {
            return ptr;
        } else {
            void* new_ptr = yy_gc_malloc_bytes(new_size);
            if (new_ptr == NULL) {
                return NULL;
            }
            memcpy(new_ptr, ptr, old_size);
            return new_ptr;
        }
    }

}



void copy_root_point(yyvalue* ptr_ptr, yyvalue* new_heap, uint64_t* new_heap_offset, uint64_t new_heap_size){
    if (is_an_new_pointer(*ptr_ptr)) {
        
    } else if (is_an_old_pointer(*ptr_ptr)) {
        yyvalue header = yy_read_tuple(*ptr_ptr, 0);
        if (yyvalue_get_type(header) == type_pointer_transfer_address)
        {
            yyvalue new_ptr = raw_tuple_to_yyvalue(yyvalue_get_length(*ptr_ptr), yyvalue_to_transfer_address(header));
            *ptr_ptr = new_ptr;
        }
        else
        {
            uint64_t ptr_size = yyvalue_get_length(*ptr_ptr);
            if (*new_heap_offset + ptr_size + 1 >= new_heap_size) {
                #ifdef __APPLE__
                    fprintf(stderr, "No space left during GC, BUG, requested offset %llu, size %llu\n", *new_heap_offset + ptr_size + 1, new_heap_size);
                #else
                    fprintf(stderr, "No space left during GC, BUG, requested offset %ld, size %ld\n", *new_heap_offset + ptr_size, new_heap_size);
                #endif
                errorAndAbort("This means header is not correctly marked");
            }
            memcpy(new_heap + *new_heap_offset, yyvalue_to_tuple(*ptr_ptr), (ptr_size) * sizeof(yyvalue));
            yyvalue* new_ptr = new_heap + *new_heap_offset;
            *new_heap_offset += ptr_size;
            yy_write_tuple(*ptr_ptr, 0, transfer_address_to_yyvalue(new_ptr));
            *ptr_ptr = raw_tuple_to_yyvalue(yyvalue_get_length(*ptr_ptr), new_ptr);
        }
    }

    verify_yyvalue_new_heap(*ptr_ptr, false, 0);
    // assert( (!is_a_pointer((void*)(*ptr_ptr))) || (!(check_magic_number(*(uint64_t*)(*ptr_ptr)))));
}

static uint64_t gc_count = 0;
bool during_gc = false;

void yy_perform_gc(yyvalue* additional_root_point) {
    
    during_gc = true;
    gc_count++;

    if (yy_gc_debug_flag){
        #ifdef __APPLE__
            fprintf(stderr, "%llu Performing garbage collection, current_heap_offset %llu, tiny_heap_offset %llu    ", gc_count, current_heap_offset, tiny_heap_offset);
        #else
            fprintf(stderr, "%ld Performing garbage collection, current_heap_offset %ld, tiny_heap_offset %ld    ", gc_count, current_heap_offset, tiny_heap_offset);
        #endif
        fflush(stderr);
    }

    if (should_expand_heap) {
        // Expand the heap
        new_heap_size = current_heap_size * 2 + tiny_heap_offset + 10;
        new_heap = MALLOC_FUNC(new_heap_size);
        if (!new_heap) errorAndAbort("Failed to expand major heap");
        should_expand_heap = false;
    } else {
        // Reuse the current heap
        new_heap_size = current_heap_size + tiny_heap_offset + 10;
        new_heap = MALLOC_FUNC(new_heap_size);
    }

    uint64_t scan_offset = 0;
    uint64_t new_heap_offset = 0;

    // copy the root points
    for (int i = 0; i < gc_root_points_count; i++) {
        copy_root_point((gc_root_points[i]), new_heap, &new_heap_offset, new_heap_size);
    }
    yyvalue* stack_ptr_gc = (yyvalue*)stack;
    while(stack_ptr_gc < stack_ptr){
        copy_root_point((stack_ptr_gc), new_heap, &new_heap_offset, new_heap_size);
        stack_ptr_gc++;
    }
    copy_root_point(additional_root_point, new_heap, &new_heap_offset, new_heap_size);
    // fprintf(stderr, "Scanning New Heap");
    // for (int64_t i = 0; i < new_heap_offset; i++) {
    //     fprintf(stderr, "%ld %p, ", i, *((void **)(new_heap + i)));
    // }

    while (scan_offset <  new_heap_offset) {
        // fprintf(stderr, "scan_offset %ld, new_heap_offset %ld, heap_value %p,   ", scan_offset, new_heap_offset, *(void**)(new_heap + scan_offset));
        // fflush(stderr);
        copy_root_point((new_heap + scan_offset), new_heap, &new_heap_offset, new_heap_size);
        scan_offset ++;
    }

    // remove the extra allocation from tracking
    if (new_heap_offset + tiny_heap_offset + 10 < new_heap_size) {
        new_heap_size = new_heap_size - tiny_heap_offset - 10;
    }

    // we're all done
    free(current_heap);

    if (yy_gc_debug_flag){
        #ifdef __APPLE__
        fprintf(stderr, "Finished garbage collection, collected %llu remaining, %llu total,  (* 8 bytes) ", new_heap_offset, new_heap_size);
        fprintf(stderr, "New heap %p - %p size %llu, Prev heap %p - %p size %llu \n", new_heap, new_heap + new_heap_offset, new_heap_size, current_heap, current_heap + current_heap_offset, current_heap_size);
        #else
        fprintf(stderr, "Finished garbage collection, collected %ld remaining, %ld total,  (* 8 bytes) ", new_heap_offset, new_heap_size);
        fprintf(stderr, "New heap %p - %p size %ld, Prev heap %p - %p size %ld \n", new_heap, new_heap + new_heap_offset, new_heap_size, current_heap, current_heap + current_heap_offset, current_heap_size);
        #endif
        fflush(stderr);
    }
    current_heap = new_heap;
    current_heap_offset = new_heap_offset;
    current_heap_size = new_heap_size;
    new_heap = NULL;
    new_heap_size = 0;
    

    if ((double)new_heap_offset / current_heap_size > 0.15) {
        should_expand_heap = true;
    }
    

    // Clean up tiny heap at the end of a successful garbage collection
    tiny_heap_offset = 0;
    verify_gc(additional_root_point);
    during_gc = false;
}


