#include "globalInclude.h"

char * addr_to_string(uint64_t* arg) {
    return (char *)arg[1];
}

uint64_t getHeader(uint64_t type, uint64_t length) {
    uint64_t header =  type ;
    header= header<< (62 - 6);
    uint64_t lengthMask = length << (62 - 22);
    header = header | lengthMask;
    // TODO: (Maybe? add gc bits (but we're doing gc through libgc, not sure if it is still necessary))
    return header;
}

uint64_t* unit_to_addr(){
    uint64_t* returnStorage = allocateArray(1);
    *returnStorage = getHeader(5, 0);
    return returnStorage;
}

uint64_t* string_to_addr(const char * str){
    uint64_t* returnStorage = allocateArray(2);
    *returnStorage =getHeader(6, 1) ;
    returnStorage[1] = (uint64_t ) str;
    return returnStorage;
}

uint64_t* injection_to_addr(uint64_t index, const char* label, uint64_t* elem){
    uint64_t* returnStorage = allocateArray(4);
    *returnStorage =getHeader(4, 3) ;
    returnStorage[1] = index;
    returnStorage[2] = label;
    returnStorage[3] = elem;
    return returnStorage;
}


const char* bool_false_label = "阴";
const char* bool_true_label = "阳";

/* bool is defined as 阴 ： 1 + 阳 ： 1 */
uint64_t* bool_to_addr(bool b){
    if(!b){
        return injection_to_addr(0, bool_false_label, unit_to_addr);
    } else {
        return injection_to_addr(1, bool_true_label, unit_to_addr);
    }
}

uint64_t* tuple_to_addr(uint64_t length, const uint64_t* elems[]){

    uint64_t* returnStorage = allocateArray(length+1);
    *returnStorage = getHeader(3, length);
    for (int i = 0; i < length; i ++){
        returnStorage[i+1] = (uint64_t) elems[i];
    }
    return returnStorage;
}

// gets the length of a list of type rho t. (0 : 1) + (1 : a x t)
uint64_t iso_list_length_rec(const uint64_t * list, uint64_t acc) {
    // the type is a fold on the outer left
    uint64_t* unfolded = (uint64_t *)list[1];
    // then it is a sum, the label's position is stored in index 1
    uint64_t labelIndex = unfolded[1];
    if (labelIndex == 0) {
        return acc;
    } else {
        uint64_t * underSum = unfolded[3];
        uint64_t * underProd = underSum[2];
        return iso_list_length_rec(underProd, acc+1);
    }
}

// gets the length of a list of type rho t. (0 : 1) + (1 : a x t)
uint64_t iso_list_get_length(const uint64_t * list) {
    return iso_list_length_rec(list, 0);
}

// gets the length of a list of type rho t. (0 : 1) + (1 : a x t)
uint64_t** iso_list_get_elements(const uint64_t * list) {
    uint64_t size = iso_list_get_length(list);

    uint64_t** result = allocateArray(size);

    uint64_t curIndex = 0;
    uint64_t* curListUnfolded = (uint64_t *)list[1];

    while(curIndex < size) {
        uint64_t * underSum = curListUnfolded[3];
        result[curIndex] = underSum[1];
        uint64_t * remainingListFolded = underSum[2];
        curListUnfolded = remainingListFolded[1];
        curIndex += 1;
    }
    return result;
}