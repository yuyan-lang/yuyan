#include "globalInclude.h"

char * addr_to_string(yy_ptr arg) {
    // return (char *)arg[1];
    return *((char **)&arg);
}

// type conversion function
yy_ptr data_to_addr(uint64_t elem){
    return *((yy_ptr *)&elem);
}
uint64_t addr_to_data(yy_ptr ptr){
    return *((uint64_t *)&ptr);
}

int64_t addr_to_int(yy_ptr arg) {
    return *((int64_t *)&arg);
}


uint64_t getHeader(uint64_t type, uint64_t length) {
    uint64_t header =  type ;
    header= header<< (62 - 6);
    if(length > 65535){
        /* errorAndAbort*/
        errorAndAbort("Attempt to allocate datatype of size > 65535 ");
    }
    uint64_t lengthMask = length << (62 - 22);
    header = header | lengthMask;
    // TODO: (Maybe? add gc bits (but we're doing gc through libgc, not sure if it is still necessary))
    return header;
}

yy_ptr allocateAndSetHeader(uint64_t type, uint64_t length) {
    yy_ptr returnStorage = allocateArray(length);
    // *returnStorage = getHeader(type, length);
    return returnStorage; 
}

yy_ptr unit_to_addr(){
    return allocateAndSetHeader(5, 0);
}

yy_ptr string_to_addr(const char * str){
    // yy_ptr returnStorage = allocateAndSetHeader(6, 1);
    // returnStorage[1] = (uint64_t ) str;
    return *(uint64_t **)&str;
}
yy_ptr int_to_addr(int64_t i){
    // yy_ptr returnStorage = allocateAndSetHeader(7, 1);
    // returnStorage[1] = *(uint64_t *)&i;
    // return returnStorage;

    return *(uint64_t **)&i;
}

yy_ptr double_to_addr(double i){
    // yy_ptr returnStorage = allocateAndSetHeader(8, 1);
    // returnStorage[1] = *(uint64_t *)&i;
    // return returnStorage;
    return *(uint64_t **)&i;
}

double  addr_to_double(yy_ptr arg) {
    // return *((double *)&arg[1]);
    return *((double *)&arg);
}


yy_ptr bool_to_addr(bool b){
    return *(uint64_t **)&b;
}
bool addr_to_bool(yy_ptr arg){
    return *(bool *)&arg;
}




yy_ptr injection_to_addr(uint64_t index, const char* label, yy_ptr elem){
    yy_ptr returnStorage = allocateAndSetHeader(4, 3);
    returnStorage[0] = index;
    returnStorage[1] = addr_to_data((yy_ptr)label);
    returnStorage[2] = addr_to_data(elem);
    return returnStorage;
}


// const char* bool_false_label = "阴";
// const char* bool_true_label = "阳";

yy_ptr tuple_to_addr(uint64_t length, const yy_ptr elems[]){

    yy_ptr returnStorage = allocateAndSetHeader(3, length);
    for (int i = 0; i < length; i ++){
        returnStorage[i] = (uint64_t) elems[i];
    }
    return returnStorage;
}

yy_ptr* addr_to_tuple(yy_ptr arg){
    return (yy_ptr *)arg;
}

// gets the length of a list of type rho t. (0 : 1) + (1 : a x t)
uint64_t iso_list_length_rec(const yy_ptr list, uint64_t acc) {
    // the type is a fold on the outer left
    // yy_ptr unfolded = (yy_ptr)list[1];
    // then it is a tuple, the label's position is stored in index 1
    uint64_t labelIndex = list[0];
    if (labelIndex == 1) {
        return acc;
    } else if (labelIndex == 2){
        // OLD 0: header 1: index 2: unit (type) 3: current element 4: next element
        // NOW:  0: index 1: unit (type) 2: current element 3: next element
        yy_ptr next = data_to_addr(list[3]);
        return iso_list_length_rec(next, acc+1);
    } else {
        errorAndAbort("iso_list_get_length error");
        return -1;
    }
}

// gets the length of a list of type rho t. (0 : 1) + (1 : a x t)
uint64_t iso_list_get_length(const yy_ptr list) {
    yy_ptr *tups = addr_to_tuple(list); // the tuple in c runtime is differnt from yuyanlang 0.2 tuple, which is an iterative linked list
    return addr_to_int(tups[1]);
}

// gets the elements of a list of type rho t. (0 : 1) + (1 : a x t)
yy_ptr* iso_list_get_elements(const yy_ptr list) {
    // uint64_t size = iso_list_get_length(list);

    // yy_ptr* result = (yy_ptr*)allocateArray(size);

    // uint64_t curIndex = 0;
    // yy_ptr curList = list;

    // while(curIndex < size) {
    //     result[curIndex] = data_to_addr(curList[2]);
    //     curList = data_to_addr(curList[3]);
    //     curIndex += 1;
    // }
    yy_ptr *tups = addr_to_tuple(list);
    return (addr_to_tuple((tups[0])));
}

// const char* iso_list_nil_label = "一";
// const char* iso_list_cons_label = "二";

// yy_ptr fold_to_addr(yy_ptr toFold) {
//     yy_ptr result = allocateAndSetHeader(2, 1);
//     result[1] = addr_to_data(toFold);
//     return result;
// }
yy_ptr function_to_addr(void* func) {
    yy_ptr result = allocateAndSetHeader(1, 1);
    result[0] = addr_to_data(func);
    return result;
}

// yy_ptr iso_list_nil_to_addr() {
//     // the unit here is the implicit type argument
//     yy_ptr tps[] = {unit_to_addr(0), int_to_addr(1)};
//     return tuple_to_addr(2, tps);
// }
// yy_ptr iso_list_cons_to_addr(yy_ptr elem, yy_ptr rest){
//     yy_ptr tps[] = {int_to_addr(2), unit_to_addr(), elem, rest};
//     return tuple_to_addr(4, tps);
// }

yy_ptr heap_array_to_addr(uint64_t length, const yy_ptr* elems){

    yy_ptr len = int_to_addr(length);

    yy_ptr res_tup[] = {(yy_ptr)elems, len};
    yy_ptr res = tuple_to_addr(2, res_tup);

    return res;
}

yy_ptr array_to_iso_addr(uint64_t length, const yy_ptr elems[]){

    yy_ptr tup = tuple_to_addr(length, elems);

    return heap_array_to_addr(length, (yy_ptr *)tup);
}



