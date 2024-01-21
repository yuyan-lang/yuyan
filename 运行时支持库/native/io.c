#include "common_include.h"

yyvalue yyPrintln(yyvalue s) {
    fprintf(stdout,"%s\n", addr_to_string(s));
    fflush(stdout);
    return unit_to_addr();
}

yyvalue yyPrintlnStdErr(yyvalue s) {
    fprintf(stderr,"%s\n", addr_to_string(s));
    fflush(stderr);
    return unit_to_addr();
}

yyvalue yyPrintStr(yyvalue s) {
    fprintf(stdout,"%s", addr_to_string(s));
    fflush(stdout);
    return unit_to_addr();
}
int informResultRec (FILE *file, yyvalue result, int prevPred) {
    // if it is small, probably not an address
    if ((int64_t)result < 1000) {
        fprintf(file, "%lld",(long long)(int64_t) result);
        return 0;
    }
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
        //     informResult((yyvalue)result[i+1]);
        // }
        break;
    case 2:
        fprintf(file,"卷");
        informResultRec(file, (yyvalue) result[1], preds[type]);
        break;
   
    case 3:
        informResultRec(file, (yyvalue) result[1], preds[type]);
        for (int i = 1; i < length ; i++) {
            fprintf(file,"与");
            informResultRec(file, (yyvalue) result[1+i], preds[type]);
        }
        break;
     case 4:
        fprintf(file,"%s临", (char*)result[2]);
        informResultRec(file, (yyvalue) result[3], preds[type]);
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
        fprintf(file,"%lld", (long long) addr_to_int(result));
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
yyvalue yyPrintGeneric(yyvalue obj) {
    informResultRec(stdout, obj, 0);
    return unit_to_addr();
}

yyvalue yyPrintlnGeneric(yyvalue obj) {
    informResultRec(stdout, obj, 0);
    fprintf(stdout, "\n");
    fflush(stdout);
    return unit_to_addr();
}



char* yyReadAllStdIn() {
    // Allocate initial buffer size
    size_t bufferSize = 1024;
    char* buffer = (char*)yy_gcAllocateBytes(bufferSize);
    if (buffer == NULL) {
        return NULL; // Failed to allocate memory
    }

    size_t totalSize = 0; // Total size of the input
    size_t bytesRead; // Number of bytes read in each iteration

    // Read input from stdin until end-of-file
    while ((bytesRead = fread(buffer + totalSize, sizeof(char), bufferSize - totalSize, stdin)) > 0) {
        totalSize += bytesRead;

        // Expand buffer if necessary
        if (totalSize == bufferSize) {
            bufferSize *= 2;
            char* newBuffer = (char*)realloc(buffer, bufferSize);
            if (newBuffer == NULL) {
                free(buffer);
                return NULL; // Failed to reallocate memory
            }
            buffer = newBuffer;
        }
    }

    // Null-terminate the string
    buffer[totalSize] = '\0';

    // Shrink buffer to actual size
    char* finalString = (char*)realloc(buffer, totalSize + 1);
    if (finalString == NULL) {
        free(buffer);
        return NULL; // Failed to reallocate memory
    }

    return finalString;
}

char* yyReadLineFromStdin() {
    char* line = NULL;
    size_t bufferSize = 0;
    ssize_t bytesRead = getline(&line, &bufferSize, stdin);
    if (bytesRead == -1) {
        // Error or end-of-file encountered
        free(line);
        return NULL;
    }

    // Remove newline character, if present
    if (bytesRead > 0 && line[bytesRead - 1] == '\n') {
        line[bytesRead - 1] = '\0';
    }

    return line;
}