#include "common_include.h"

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
    char* buffer = (char*)yy_gcAllocateBytes(bufferSize);
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
    char* finalString = (char*)realloc(buffer, totalSize + 1);
    if (finalString == NULL) {
        free(buffer);
        errorAndAbort("Failed to reallocate memory");
        // return NULL; // Failed to reallocate memory
        return unit_to_yyvalue();
    }

    return string_to_yyvalue(finalString);
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

    // Remove newline character, if present
    if (bytesRead > 0 && line[bytesRead - 1] == '\n') {
        line[bytesRead - 1] = '\0';
    }

    return string_to_yyvalue(line);
}