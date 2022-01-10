#include "globalInclude.h"

yy_ptr yyPrintln(yy_ptr s) {
    fprintf(stdout,"%s\n", addr_to_string(s));
    fflush(stdout);
    return unit_to_addr();
}
int informResultRec (FILE *file, yy_ptr result, int prevPred) {
    // assume it is a tuple
    uint64_t header = result[0];
    int type = (header >> (62 - 6)) &  0b11111;
    int length = (header >> (62 - 22)) &  0b1111111111;

    int preds[] = {0, 0, 660, 680, 670, 720};
    int shouldPrintQuote = 2 <= type && type <= 5 && preds[type] < prevPred;

    if(shouldPrintQuote){
        fprintf(file,"「");
    }


    switch (type)
    {
    case 1:
        fprintf(file,"Received a function closure (length is %d). Did you define a function?\n", length);
        // for (int i = 0; i < length ; i ++){
        //     fprintf(stderr,"%d : ", i);
        //     informResult((yy_ptr)result[i+1]);
        // }
        break;
    case 2:
        fprintf(file,"卷");
        informResultRec(file, (yy_ptr) result[1], preds[type]);
        break;
   
    case 3:
        informResultRec(file, (yy_ptr) result[1], preds[type]);
        for (int i = 1; i < length ; i++) {
            fprintf(file,"与");
            informResultRec(file, (yy_ptr) result[1+i], preds[type]);
        }
        break;
     case 4:
        fprintf(file,"%s临", (char*)result[2]);
        informResultRec(file, (yy_ptr) result[3], preds[type]);
        break;
    case 5:
        fprintf(file,"元");
        break;
    case 6:
        fprintf(file,"『");
        fprintf(file,"%s", addr_to_string(result));
        fprintf(file,"』");
        break;
    case 7:
        fprintf(file,"%d", addr_to_int(result));
        break;
    case 8:
        fprintf(file,"%f", addr_to_double(result));
        break;

    
    
    default:
        fprintf(file, "ERROR: result is not interpretable. because the type %d is not defined for the runtime.\n", type);
        break;
    }

    if(shouldPrintQuote){
        fprintf(file,"」");
    }


    return 0;
}
yy_ptr yyPrintGeneric(yy_ptr obj) {
    informResultRec(stdout, obj, 0);
    return unit_to_addr();
}

yy_ptr yyPrintlnGeneric(yy_ptr obj) {
    informResultRec(stdout, obj, 0);
    fprintf(stdout, "\n");
    fflush(stdout);
    return unit_to_addr();
}