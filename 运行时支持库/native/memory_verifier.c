
#include "memory_verifier.h"
/**
 * GC VERIFICATION
 * 
 * TODO: THIS CAN BE MOVED TO A SEPARATE FILE
*/

#define VERIFY_REC_LIMIT 100

void verify_yyvalue_new_heap(yyvalue arg, bool recursive, int depth){
    if (depth >= VERIFY_REC_LIMIT) {
        return;
    }
    
    if (yyvalue_is_heap_pointer(arg)) {
        // fprintf(stderr, "Debug: is a heap pointer %p, %p, %d\n", (void*)(arg>>64), yyvalue_to_generic_ptr(arg), yyvalue_is_heap_pointer(arg));
        yyvalue* ptr = yyvalue_to_heap_pointer(arg);
        if (ptr == NULL){
            uint64_t length = yyvalue_get_heap_pointer_length(arg);
            assert(length == 0);
        } else if (ptr - new_heap >= 0 && ptr - new_heap < new_heap_size)
        {
            uint64_t length = yyvalue_get_heap_pointer_length(arg);
            assert(length <= new_heap_size);
            if (recursive && yyvalue_is_tuple(arg)) {
                for (int i = 0; i < length; i++) {
                    verify_yyvalue(yy_read_heap_pointer(arg, i), recursive, depth + 1);
                }
            }
            
        }
        else
        {
            fprintf(stderr, "Not a valid pointer %p, is a new pointer %d, is an old pointer %d\n", yyvalue_to_generic_ptr(arg), 
            is_an_new_pointer(arg), is_an_old_pointer(arg));
            errorAndAbort("Not a valid pointer");
        }
    }
}
void verify_yyvalue(yyvalue arg, bool recursive, int depth){
    if (depth >= VERIFY_REC_LIMIT) {
        return;
    }
    if (yyvalue_is_heap_pointer(arg)) {
        yyvalue* ptr = yyvalue_to_heap_pointer(arg);
        if (ptr == NULL){
            uint64_t length = yyvalue_get_heap_pointer_length(arg);
            assert(length == 0);
        } else if (ptr - current_heap >= 0 && ptr - current_heap < current_heap_size) {
            uint64_t length = yyvalue_get_heap_pointer_length(arg);
            assert(length <= current_heap_size);
            if (recursive && yyvalue_is_tuple(arg)) {
                for (int i = 0; i < length; i++) {
                    verify_yyvalue(yy_read_heap_pointer(arg, i), recursive, depth + 1);
                }
            }
        }
        else if (ptr - tiny_heap >= 0 && ptr - tiny_heap < TINY_HEAP_SIZE)
        {
            uint64_t length = yyvalue_get_heap_pointer_length(arg);
            assert(length <= TINY_HEAP_SIZE);
            if (recursive && yyvalue_is_tuple(arg)) {
                for (int i = 0; i < length; i++) {
                    verify_yyvalue(yy_read_heap_pointer(arg, i), recursive, depth + 1);
                }
            }
        } 
        else
        {
            fprintf(stderr, "Not a valid pointer %p", yyvalue_to_generic_ptr(arg));
            errorAndAbort("Not a valid pointer");
        }
    } else {
        assert(yyvalue_get_type(arg) <= type_heap_string);
    }
}

// verifies that all values are valid, stack doesn't contain pointers to outdated heap
void verify_gc(yyvalue* additional_root_point){
     // copy the root points
    for (int i = 0; i < gc_root_points_count; i++) {
        verify_yyvalue(*(gc_root_points[i]), true, 0);
    }
    yyvalue* stack_ptr_gc = (yyvalue*)stack;
    while(stack_ptr_gc < stack_ptr){
        verify_yyvalue(*(stack_ptr_gc), true, 0);
        stack_ptr_gc++;
    }
    verify_yyvalue(*additional_root_point, true, 0);

}