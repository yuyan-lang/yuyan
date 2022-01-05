#include "globalInclude.h"

char * addr_to_string(uint64_t* arg) {
    return (char *)arg[1];
}

uint64_t* unit_to_addr(){
    uint64_t* returnStorage = allocateArray(1);
    uint64_t header =  5 ;
    header= header<< (62 - 6);
    *returnStorage = header;
    return returnStorage;
}

uint64_t* string_to_addr(const char * str){
    uint64_t* returnStorage = allocateArray(2);
    uint64_t header =  6 ;
    header= header<< (62 - 6);
    *returnStorage = header;
    returnStorage[1] = (uint64_t ) str;
    return returnStorage;
}

uint64_t* tuple_to_addr(uint64_t length, const uint64_t* elems[]){

    uint64_t* returnStorage = allocateArray(length+1);
    uint64_t header =  3 ;
    header= header<< (62 - 6);
    uint64_t lengthMask = length << (62 - 22);
    header = header | lengthMask;
    // TODO: (Maybe? add gc bits (but we're doing gc through libgc, not sure if it is still necessary))
    *returnStorage = header;
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