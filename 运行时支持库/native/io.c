#include "common_include.h"
#include "debug_print.h"

yyvalue yyPrintln(yyvalue s) {
    fprintf(stdout,"%s\n", yyvalue_to_string(s));
    fflush(stdout);
    return unit_to_yyvalue();
}

yyvalue yyPrintlnStdErr(yyvalue s) {
    fprintf(stderr,"%s\n", yyvalue_to_string(s));
    fflush(stderr);
    return unit_to_yyvalue();
}

yyvalue yyPrintStr(yyvalue s) {
    fprintf(stdout,"%s", yyvalue_to_string(s));
    fflush(stdout);
    return unit_to_yyvalue();
}

yyvalue yyReadAllStdIn() {
    // Allocate initial buffer size
    size_t bufferSize = 1024;
    char* buffer = (char*)malloc(bufferSize);
    if (buffer == NULL) {
        errorAndAbort("Failed to allocate memory");
        return unit_to_yyvalue();
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
                errorAndAbort("Failed to reallocate memory");
                // return 
            }
            buffer = newBuffer;
        }
    }

    // Null-terminate the string
    buffer[totalSize] = '\0';

    // Shrink buffer to actual size
    char *result = malloc(totalSize + 1);
    memcpy(result, buffer, totalSize + 1);

    yyvalue return_val = malloc_string_to_yyvalue(totalSize+1, result);
    free(result);
    return return_val;
}

yyvalue yyReadLineFromStdin() {
    char* line = NULL;
    size_t bufferSize = 0;
    ssize_t bytesRead = getline(&line, &bufferSize, stdin);
    if (bytesRead == -1) {
        // Error or end-of-file encountered
        free(line);
        errorAndAbort("Failed to read line from stdin");
        return unit_to_yyvalue();
    }

    yyvalue ret;
    // Remove newline character, if present
    if (bytesRead > 0 && line[bytesRead - 1] == '\n') {
        line[bytesRead - 1] = '\0';
        ret = malloc_string_to_yyvalue(bytesRead, line);
    } else {
        line[bytesRead] = '\0';
        ret = malloc_string_to_yyvalue(bytesRead + 1, line);
    }

    free(line);
    return ret;
}

yyvalue yyPrintGeneric(yyvalue msg, yyvalue obj) {
    fprintf(stderr, "[yy Generic Printing] %s: ", yyvalue_to_string(msg));
    yy_print_yyvalue(obj);
    fprintf(stderr, "\n");

    // errorAndAbort("Generic Printing Indicates Irrecoverable Error");

    return unit_to_yyvalue();
}
