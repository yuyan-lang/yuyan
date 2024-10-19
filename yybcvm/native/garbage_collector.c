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

#ifndef INITIAL_HEAP_SIZE
// #define INITIAL_HEAP_SIZE (64 * 1024 * 1024)  // 1024 MB
#ifdef NDEBUG
#define INITIAL_HEAP_SIZE (2 * 1024 * 1024)  // 32 MB
#else
// #define INITIAL_HEAP_SIZE (4 * 1024 )  // 32 KB
#define INITIAL_HEAP_SIZE (512 )  // 32 KB
// #define INITIAL_HEAP_SIZE (2 * 1024 * 1024)  // 32 MB
#endif
#endif

#define DEFAULT_MAX_HEAP_SIZE (2L * 1024 * 1024 * 1024) // 32 GB is the max heap size
#define GC_LIMIT 90.0 // percentage of heap if this is filled up

yyvalue* current_heap = NULL;
yyvalue* current_allocation_ptr = NULL;
yyvalue *current_heap_end = NULL;
yyvalue *current_heap_gc_limit = NULL;
yyvalue* new_heap = NULL;
yyvalue *new_heap_end = NULL;
uint64_t initial_heap_size = INITIAL_HEAP_SIZE; //32 MB
uint64_t max_heap_size = DEFAULT_MAX_HEAP_SIZE;
bool should_expand_heap = false;

// #define MALLOC_FUNC(X) malloc(X * sizeof(yyvalue))
#define MALLOC_FUNC(X) calloc(sizeof(yyvalue), X)

bool yy_gc_debug_flag = false;

void yy_gc_init() {
    if (getenv("YY_GC_INITIAL_HEAP_SIZE_MB") != NULL ) {
        const char* requested_size = getenv("YY_GC_INITIAL_HEAP_SIZE_MB");
        initial_heap_size = atoi(requested_size) * 1024 * 1024;
    }

    if (getenv("YY_GC_MAX_HEAP_SIZE_MB") != NULL ) {
        const char* requested_size = getenv("YY_GC_MAX_HEAP_SIZE_MB");
        max_heap_size = atoi(requested_size) * 1024 * 1024;
    }

    if (initial_heap_size != INITIAL_HEAP_SIZE) {
        fprintf(stderr, "YY initial heap size is set to %f * 16 MB\n", (double) initial_heap_size / (1024 * 1024));
    }

    if (max_heap_size != DEFAULT_MAX_HEAP_SIZE) {
        fprintf(stderr, "YY max heap size is set to %f * 16 MB\n", (double) max_heap_size / (1024 * 1024));
    }

    uint64_t current_heap_size = MIN(initial_heap_size, max_heap_size);
    current_heap = MALLOC_FUNC(initial_heap_size);
    current_heap_end = current_heap + current_heap_size;
    current_heap_gc_limit = current_heap + (uint64_t) (current_heap_size * GC_LIMIT / 100);
    current_allocation_ptr = current_heap;
    if (!current_heap) {
        errorAndAbort("Failed to initialize major heap");
    }
    
    yy_gc_debug_flag = (getenv("YY_GC_DEBUG_FLAG") != NULL && strcmp(getenv("YY_GC_DEBUG_FLAG"), "1") == 0);
}

bool is_a_null_pointer(yyvalue raw_ptr) {
    if (yyvalue_is_heap_pointer(raw_ptr)){
        yyvalue* ptr = yyvalue_to_heap_pointer(raw_ptr);
        if (ptr == NULL) {
            return true;
        } else {
            return false;
        }
    } else {
        return false;
    }
}

bool is_an_old_pointer(yyvalue raw_ptr) {
    if (! yyvalue_is_heap_pointer(raw_ptr)){
        return false;
    } else {
        yyvalue* ptr = yyvalue_to_heap_pointer(raw_ptr);
        if (current_heap <= ptr && ptr < current_heap_end) {
            assert(ptr + yyvalue_get_heap_pointer_length(raw_ptr) <= current_heap_end);
            return true;
        }
        else
        {
            assert(is_a_null_pointer(raw_ptr) || is_a_new_pointer(raw_ptr));
            return false;
        }
    }
}


bool is_a_new_pointer(yyvalue raw_ptr) {
    if (!yyvalue_is_heap_pointer(raw_ptr)){
        return false;
    } else {
        yyvalue* ptr = yyvalue_to_heap_pointer(raw_ptr);
        if ( new_heap <= ptr && ptr < new_heap_end) {
            assert(ptr + yyvalue_get_heap_pointer_length(raw_ptr) <= new_heap_end);
            return true;
        }
        else
        {
            assert(is_a_null_pointer(raw_ptr) || is_an_old_pointer(raw_ptr));
            return false;
        }
    }
}


    
#ifndef NDEBUG
void* yy_gc_malloc_array(uint64_t size) {
    if (size == 0) {
        return NULL;
    }

    yyvalue *ret = current_allocation_ptr;

    current_allocation_ptr += size;
    if (current_allocation_ptr > current_heap_end) {
        fprintf(stderr, "No space left and garbage collection cannot be performed yet. \n" \
        "Heap Start %p, Heap End %p, Heap GC Limit %p, Allocation Pointer %p \n" \
        "Heap Size %td, Heap Offset %td, GC Point Size %td, Allocation Size %" PRIu64 "\n", 
            current_heap, current_heap_end, current_heap_gc_limit, current_allocation_ptr,
            current_heap_end - current_heap, 
            current_allocation_ptr - current_heap, 
            current_heap_gc_limit - current_heap,
            size);
        errorAndAbort("No space left in the major heap, make GC occur earlier by adjusting the GC limit");
        return NULL;
    }
    return ret;

}
#else
void* yy_gc_malloc_array(uint64_t size) {
    if (size == 0) {
        return NULL;
    }

    yyvalue *ret = current_allocation_ptr;
    current_allocation_ptr += size;
    return ret;
}
#endif



void copy_root_point(yyvalue* ptr_ptr, yyvalue* new_heap, yyvalue** new_heap_allocation_ptr/* may modify allocation ptr */, yyvalue* new_heap_end){
    yyvalue ptr_ptr_prev_val = *ptr_ptr;
    // fprintf(stderr, "Copying root point %p, prev val %p %p,\n", ptr_ptr, (void*)(ptr_ptr_prev_val >> 64),  (void*)ptr_ptr_prev_val);
    int ptr_ptr_prev_val_type = yyvalue_get_type(ptr_ptr_prev_val);
    int is_a_new_pointer_flag = -1;
    int is_an_old_pointer_flag = -1;
    if (is_a_new_pointer(*ptr_ptr))
    {
        is_a_new_pointer_flag = 1;
    }
    else if (is_an_old_pointer(*ptr_ptr))
    {
        is_an_old_pointer_flag = 1;
        yyvalue header = yy_read_heap_pointer(*ptr_ptr, 0);
        if (yyvalue_get_type(header) == type_pointer_transfer_address)
        {
            yyvalue* transfer_address = yyvalue_to_transfer_address(header);
            assert((new_heap <= transfer_address) && (transfer_address < new_heap_end));
            yyvalue new_ptr = heap_pointer_to_yyvalue(
                yyvalue_get_type(*ptr_ptr),
                yyvalue_get_subtype(*ptr_ptr),
                yyvalue_get_raw_length(*ptr_ptr),
                transfer_address);
            *ptr_ptr = new_ptr;
        }
        else
        {
            uint64_t ptr_size = yyvalue_get_heap_pointer_length(*ptr_ptr);
            if (yyvalue_is_string_pointer(*ptr_ptr))
            {
                uint64_t l1 = yyvalue_get_strlen(*ptr_ptr);
                uint64_t l2 = strlen(yyvalue_to_string(*ptr_ptr));
                char* s = yyvalue_to_string(*ptr_ptr);
                assert(l1 == l2);
            }
            yyvalue* ret = *new_heap_allocation_ptr;
            yyvalue* incremented_new_heap_allocation_ptr = *new_heap_allocation_ptr + ptr_size;
            if (incremented_new_heap_allocation_ptr > new_heap_end) {
                errorAndAbort("No space left during GC BUG, should have allocated enough space on new heap" "This may mean that the header is not correctly marked");
                fprintf(stderr, "No space left during GC BUG. New Heap Size %td, Heap Offset %td, Requested Offset %td, Requested Size %" PRIu64 "\n",
                        new_heap_end - new_heap,
                        ret - new_heap,
                        incremented_new_heap_allocation_ptr - new_heap,
                        ptr_size);
                return;
            }

            memcpy(ret, yyvalue_to_heap_pointer(*ptr_ptr), (ptr_size) * sizeof(yyvalue));
            *new_heap_allocation_ptr = incremented_new_heap_allocation_ptr;
            yy_write_heap_pointer(*ptr_ptr, 0, transfer_address_to_yyvalue(ret));
            *ptr_ptr = heap_pointer_to_yyvalue(
                yyvalue_get_type(*ptr_ptr),
                yyvalue_get_subtype(*ptr_ptr),
                yyvalue_get_raw_length(*ptr_ptr), ret);
        }
    }

    verify_yyvalue_new_heap(*ptr_ptr, false, 0);
}

static uint64_t gc_count = 0;
bool during_gc = false;

// will treat registered root point and all stack_start - (stack_ptr ) as root points, stack_ptr + 1 is the top of the stack that should be filled with a root point
void yy_perform_gc() {
    
    during_gc = true;
    gc_count++;

    if (yy_gc_debug_flag){
        fprintf(stderr, "%" PRIu64 ": Performing garbage collection, current_heap_offset %td,  ", gc_count, current_allocation_ptr - current_heap);
        fflush(stderr);
    }
    verify_gc();

    if (yy_gc_debug_flag){
        fprintf(stderr, ", verify_gc() completed");
        fflush(stderr);
    }

    uint64_t current_heap_size = current_heap_end - current_heap;
    uint64_t new_heap_size = 0;

    if (should_expand_heap) {
        // Expand the heap
        new_heap_size = MIN(current_heap_size * 2, max_heap_size) ;
        new_heap = MALLOC_FUNC(new_heap_size);
        new_heap_end = new_heap + new_heap_size;
        should_expand_heap = false;
    } else {
        // Reuse the current heap
        new_heap_size = MIN(current_heap_size, max_heap_size);
        new_heap = MALLOC_FUNC(new_heap_size);
        new_heap_end = new_heap + new_heap_size;
    }
    if (!new_heap){
        errorAndAbort("Failed to expand major heap");
    }


    yyvalue* scan_ptr = new_heap;
    yyvalue* new_heap_allocation_ptr = new_heap;

    // copy the root points
    for (int i = 0; i < gc_root_points_count; i++) {
        copy_root_point(gc_root_points[i], new_heap, &new_heap_allocation_ptr, new_heap_end);
    }
    yyvalue* stack_ptr_gc = stack_start;
    while(stack_ptr_gc < stack_ptr){
        copy_root_point(stack_ptr_gc, new_heap, &new_heap_allocation_ptr, new_heap_end);
        stack_ptr_gc++;
    }

    while (scan_ptr <  new_heap_allocation_ptr) {
        // fprintf(stderr, "scan_offset %ld, new_heap_offset %ld, heap_value %p,   ", scan_offset, new_heap_offset, *(void**)(new_heap + scan_offset));
        // fflush(stderr);
        if (yyvalue_is_heap_string_header(*scan_ptr)){
            scan_ptr += yyvalue_get_heap_pointer_length(*scan_ptr);
        } else {
            copy_root_point(scan_ptr, new_heap, &new_heap_allocation_ptr, new_heap_end);
            scan_ptr++;
        }
    }


    //testing this, erase unused portion of stack to prevent memory leak (e.g. previous values on the stack that are not scanned)
    // so everything should be good for the ENTIRE stack
    memset(stack_ptr, 0, (highest_stack_ptr - stack_ptr) * sizeof(yyvalue));

    // we're all done
    free(current_heap);

    if (yy_gc_debug_flag){
        ptrdiff_t new_heap_used_size = new_heap_allocation_ptr - new_heap;
        ptrdiff_t previous_heap_used_size = current_allocation_ptr - current_heap;
        double freed_percent = (double)(previous_heap_used_size - new_heap_used_size) / previous_heap_used_size * 100;
        double new_heap_used_size_MB = (double)new_heap_used_size / (1024 * 1024);
        double previous_heap_used_size_MB = (double)previous_heap_used_size / (1024 * 1024);
        fprintf(stderr, "Finished garbage collection, %td active (%.2f M), %td total (%.2f M), (* 16 bytes) (%.2f%% freed) ",
                new_heap_used_size,
                new_heap_used_size_MB,
                previous_heap_used_size,
                previous_heap_used_size_MB,
                freed_percent);
        fprintf(stderr, "New heap %p - %p size %" PRIu64 ", Prev heap %p - %p size %" PRIu64 ", ", new_heap, new_heap_end, new_heap_size, current_heap, current_heap_end, current_heap_size);
        fflush(stderr);
    }
    current_heap = new_heap;
    current_allocation_ptr = new_heap_allocation_ptr;
    current_heap_size = new_heap_size;
    current_heap_gc_limit = current_heap + (uint64_t) (current_heap_size * GC_LIMIT / 100);
    current_heap_end = new_heap_end;
    new_heap = NULL;
    new_heap_end = NULL;
    new_heap_size = 0;
    

    if ((double)(current_allocation_ptr - current_heap) / current_heap_size > 0.1) {
        should_expand_heap = true;
    }
    

    // Clean up tiny heap at the end of a successful garbage collection
    // memset(tiny_heap, 0, tiny_heap_offset * sizeof(yyvalue));
    // if (current_heap_size / TINY_HEAP_DENOMINATOR > tiny_heap_size) {
    //     uint64_t origial_tiny_heap_size = tiny_heap_size;
    //     tiny_heap_size = current_heap_size / TINY_HEAP_DENOMINATOR;
    //     tiny_heap = realloc(tiny_heap, tiny_heap_size * sizeof(yyvalue));
    //     memset(tiny_heap + origial_tiny_heap_size, 0, (tiny_heap_size - origial_tiny_heap_size) * sizeof(yyvalue));
    // }
    // tiny_heap_offset = 0;
    verify_gc();

    if (yy_gc_debug_flag){
        fprintf(stderr, ", post verify_gc() completed\n");
        fflush(stderr);
    }


    during_gc = false;
}


