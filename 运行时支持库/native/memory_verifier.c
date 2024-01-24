
#include "memory_verifier.h"
/**
 * GC VERIFICATION
 * 
 * TODO: THIS CAN BE MOVED TO A SEPARATE FILE
*/



// do not perform verification when optimizing
#ifdef NDEBUG
void verify_yyvalue_new_heap(yyvalue arg, bool recursive, int depth){
}
void verify_yyvalue(yyvalue arg, bool recursive, int depth){
}
void verify_gc(yyvalue* additional_root_point){
}
void verify_current_heap(){
}
#else

#define VERIFY_REC_LIMIT 10

void verify_yyvalue_new_heap(yyvalue arg, bool recursive, int depth){
    if (depth >= VERIFY_REC_LIMIT) {
        return;
    }
    
    if (yyvalue_is_heap_pointer(arg)) {
         if (yyvalue_is_string_pointer(arg)){
            uint64_t l1 = yyvalue_get_strlen(arg);
            uint64_t l2 = strlen(yyvalue_to_string(arg));
            char* s = yyvalue_to_string(arg);
            assert(l1 == l2);
        } 
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
        }  else {
            fprintf(stderr, "[52] Not a valid pointer %p, is a new pointer %d, is an old pointer %d\n", yyvalue_to_generic_ptr(arg), 
            is_an_new_pointer(arg), is_an_old_pointer(arg));
            errorAndAbort("Not a valid pointer");
        } 
    }  else  if (yyvalue_is_string_pointer(arg)){
        assert(yyvalue_get_strlen(arg) == strlen(yyvalue_to_string(arg)));
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
        } else if (ptr - current_heap >= 0 && ptr - current_heap < current_heap_offset) {
            uint64_t length = yyvalue_get_heap_pointer_length(arg);
            assert(length <= current_heap_offset);
            if (recursive && yyvalue_is_tuple(arg)) {
                for (int i = 0; i < length; i++) {
                    verify_yyvalue(yy_read_heap_pointer(arg, i), recursive, depth + 1);
                }
            }
        }
        else if (ptr - tiny_heap >= 0 && ptr - tiny_heap < tiny_heap_offset)
        {
            uint64_t length = yyvalue_get_heap_pointer_length(arg);
            assert(length <= tiny_heap_offset);
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

        //separately check string   
        // if (yyvalue_is_string_pointer(arg)){
        //     yyvalue header = yy_read_heap_pointer(arg, 0);
        //     uint64_t l1 = yyvalue_get_strlen(arg);
        //     uint64_t l2 = strlen(yyvalue_to_string(arg));
        //     uint64_t l3 = yyvalue_get_strlen(header);
        //     uint64_t heap_offset = yyvalue_to_heap_pointer(arg) - current_heap;
        //     char* s = yyvalue_to_string(arg);
        //     assert(l1 == l2 && l2 == l3);
        // } 
    } else if (yyvalue_is_string_pointer(arg)){
        assert(yyvalue_get_strlen(arg) == strlen(yyvalue_to_string(arg)));
    } else {
        assert(yyvalue_get_type(arg) <= type_heap_string);
        assert(yyvalue_get_type(arg) != type_pointer_transfer_address);
    }
}

// verifies that all values are valid, stack doesn't contain pointers to outdated heap
void verify_gc(yyvalue* additional_root_point){
    verify_current_heap();
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

void verify_current_heap(){
    for (uint64_t i = 0; i < current_heap_offset ; ) {
        // fprintf(stderr, "Debug: " PRIu64 "\n", i);
        yyvalue* next_scan_pointer = current_heap + i;
        if (yyvalue_is_heap_string_header(*next_scan_pointer)){
            uint64_t skip = string_buffer_length_to_block_length(yyvalue_get_strlen(*next_scan_pointer));
            // char* s = (char*)(next_scan_pointer + 1);
            // uint64_t l1 = strlen(s);
            // uint64_t l2 = yyvalue_get_strlen(*next_scan_pointer);
            assert(yyvalue_get_strlen(*next_scan_pointer) == strlen((char*)(next_scan_pointer + 1)));
            for (int j = 1; j < skip; j++) {
                yyvalue *skipped_value = next_scan_pointer + j;
                // assert(!((0 <= yyvalue_get_type(*skipped_value))  && (yyvalue_get_type(*skipped_value) < 11)));
            }
            i += skip;
        }
        else
        {
            verify_yyvalue(*(next_scan_pointer), false, 0);
            i +=1;
        }
    }
}
#endif