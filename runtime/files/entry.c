


#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include "gc.h" // https://hboehm.info/gc/ libgc 
extern int entryMain(); // the entry to yy llvm ir


int global_argc = 0;
char** global_argv = NULL;
int main(int argc, char* argv[]) {
    global_argc = argc;
    global_argv = argv;
     GC_INIT();
    return entryMain();
}



int informResultRec (uint64_t result[], int prevPred) {
    // assume it is a tuple
    uint64_t header = result[0];
    int type = (header >> (64 - 7)) &  0b11111;
    int length = (header >> (64 - 17)) &  0b1111111111;

    int preds[] = {0, 0, 660, 680, 670, 720};
    int shouldPrintQuote = 2 <= type && type <= 5 && preds[type] < prevPred;

    if(shouldPrintQuote){
        fprintf(stderr,"「");
    }


    switch (type)
    {
    case 1:
        fprintf(stderr,"Received a function closure (length is %d). Did you define a function?\n", length);
        // for (int i = 0; i < length ; i ++){
        //     fprintf(stderr,"%d : ", i);
        //     informResult((uint64_t *)result[i+1]);
        // }
        break;
    case 2:
        fprintf(stderr,"卷");
        informResultRec((uint64_t *) result[1], preds[type]);
        break;
   
    case 3:
        informResultRec((uint64_t *) result[1], preds[type]);
        for (int i = 1; i < length ; i++) {
            fprintf(stderr,"与");
            informResultRec((uint64_t *) result[1+i], preds[type]);
        }
        break;
     case 4:
        fprintf(stderr,"%s临", (char*)result[2]);
        informResultRec((uint64_t *) result[3], preds[type]);
        break;
    case 5:
        fprintf(stderr,"元");
        break;
    case 6:
        fprintf(stderr,"『");
        fprintf(stderr,"%s", (char *)result[1]);
        fprintf(stderr,"』");
        break;
    
    
    default:
        fprintf(stderr, "ERROR: result is not interpretable. because the type %d is not defined for the runtime.\n", type);
        break;
    }

    if(shouldPrintQuote){
        fprintf(stderr,"」");
    }


    return 0;
}
int informResult (uint64_t result[]) {
    return informResultRec(result, 0);
}

extern uint64_t * allocateArray(int size);
uint64_t* unit_return(){
    uint64_t* returnStorage = allocateArray(1);
    uint64_t header =  5 ;
    header= header<< (62 - 5);
    *returnStorage = header;
    return returnStorage;
}
uint64_t* yuyan_print(uint64_t s[]) {
    fprintf(stdout,"%s", (char *)s[1]);
    fflush(stdout);
    return unit_return();
}