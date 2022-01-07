#include "globalInclude.h"

char * addr_to_string(yy_ptr arg) {
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

yy_ptr unit_to_addr(){
    yy_ptr returnStorage = allocateArray(1);
    *returnStorage = getHeader(5, 0);
    return returnStorage;
}

yy_ptr string_to_addr(const char * str){
    yy_ptr returnStorage = allocateArray(2);
    *returnStorage =getHeader(6, 1) ;
    returnStorage[1] = (uint64_t ) str;
    return returnStorage;
}

yy_ptr injection_to_addr(uint64_t index, const char* label, yy_ptr elem){
    yy_ptr returnStorage = allocateArray(4);
    *returnStorage =getHeader(4, 3) ;
    returnStorage[1] = index;
    returnStorage[2] = label;
    returnStorage[3] = elem;
    return returnStorage;
}


const char* bool_false_label = "阴";
const char* bool_true_label = "阳";

/* bool is defined as 阴 ： 1 + 阳 ： 1 */
yy_ptr bool_to_addr(bool b){
    if(!b){
        return injection_to_addr(0, bool_false_label, unit_to_addr);
    } else {
        return injection_to_addr(1, bool_true_label, unit_to_addr);
    }
}

yy_ptr tuple_to_addr(uint64_t length, const yy_ptr elems[]){

    yy_ptr returnStorage = allocateArray(length+1);
    *returnStorage = getHeader(3, length);
    for (int i = 0; i < length; i ++){
        returnStorage[i+1] = (uint64_t) elems[i];
    }
    return returnStorage;
}

// gets the length of a list of type rho t. (0 : 1) + (1 : a x t)
uint64_t iso_list_length_rec(const yy_ptr list, uint64_t acc) {
    // the type is a fold on the outer left
    yy_ptr unfolded = (yy_ptr)list[1];
    // then it is a sum, the label's position is stored in index 1
    uint64_t labelIndex = unfolded[1];
    if (labelIndex == 0) {
        return acc;
    } else {
        yy_ptr underSum = unfolded[3];
        yy_ptr underProd = underSum[2];
        return iso_list_length_rec(underProd, acc+1);
    }
}

// gets the length of a list of type rho t. (0 : 1) + (1 : a x t)
uint64_t iso_list_get_length(const yy_ptr list) {
    return iso_list_length_rec(list, 0);
}

// gets the elements of a list of type rho t. (0 : 1) + (1 : a x t)
yy_ptr* iso_list_get_elements(const yy_ptr list) {
    uint64_t size = iso_list_get_length(list);

    yy_ptr* result = allocateArray(size);

    uint64_t curIndex = 0;
    yy_ptr curListUnfolded = (yy_ptr)list[1];

    while(curIndex < size) {
        yy_ptr underSum = curListUnfolded[3];
        result[curIndex] = underSum[1];
        yy_ptr remainingListFolded = underSum[2];
        curListUnfolded = remainingListFolded[1];
        curIndex += 1;
    }
    return result;
}

const char* iso_list_nil_label = "一";
const char* iso_list_cons_label = "二";

yy_ptr fold_to_addr(yy_ptr toFold) {
    yy_ptr result = allocateArray(2);
    *result = getHeader(2, 1);
    result[1] = toFold;
    return result;
}

yy_ptr iso_list_nil_to_addr() {
    return fold_to_addr(injection_to_addr(0, iso_list_nil_label, unit_to_addr()));
}
yy_ptr iso_list_cons_to_addr(yy_ptr elem, yy_ptr rest){
    yy_ptr tps[] = {elem, rest};
    return fold_to_addr(injection_to_addr(1, iso_list_cons_label, 
        tuple_to_addr(2, tps)
    ));
}

yy_ptr array_to_iso_addr(uint64_t length, const yy_ptr elems[]){

    yy_ptr res = iso_list_nil_to_addr();

    for (int i = length - 1; i >= 0; i --){
        res = iso_list_cons_to_addr(elems[i], res);
    }

    return res;
}
