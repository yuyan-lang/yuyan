
#include "common_include.h"
#include "memory_verifier.h"

// returns if s1 is a substring of s2
yyvalue yyIsSubstring(yyvalue s1, yyvalue s2) {
    if (strstr(yyvalue_to_string(s2), yyvalue_to_string(s1)) != NULL) {
        return bool_to_yyvalue(true);
    } else {
        return bool_to_yyvalue(false);
    }
}

// returns if s1 is a substring of s2
yyvalue yyStringEq(yyvalue s1, yyvalue s2) {
    char *str1 = yyvalue_to_string(s1);
    char *str2 = yyvalue_to_string(s2);
    if (strcmp(str2, str1) == 0)
    {
        return bool_to_yyvalue(true);
    }
    else
    {
        return bool_to_yyvalue(false);
    }
}

yyvalue yyStringByteLength(yyvalue s1){
    char *s = yyvalue_to_string(s1);
    int64_t l = strlen(s);
    return int_to_yyvalue(l);
}

yyvalue yyStringByteArrayGetLength(yyvalue s1){
    char *s = yyvalue_to_string(s1);
    int64_t l = strlen(s);
    return int_to_yyvalue(l);
}


yyvalue yy_豫言字符串获取字节数组(yyvalue s1){
    return s1;
    // char *s = yyvalue_to_string(s1);
    // return string_to_yyvalue(s);
}

yyvalue yy_豫言字符转整数(yyvalue s1, yyvalue idx_ptr){
    char *s =  yyvalue_to_string(s1);
    uint64_t index = yyvalue_to_int(idx_ptr);
    char result = s[index];
    return int_to_yyvalue((unsigned char)result);
}

yyvalue yy_豫言子字符串从字节序数开始(yyvalue sVal, yyvalue idxVal){

    char* s = yyvalue_to_string(sVal);
    int64_t idx = yyvalue_to_int(idxVal);
    char *result = &s[idx];
    return malloc_string_to_yyvalue(strlen(result) + 1, result);
}

yyvalue yy_豫言字符串匹配(yyvalue searchVal, yyvalue startIdxVal, yyvalue matchVal) {
    char* search = yyvalue_to_string(searchVal);
    int64_t startIdx = yyvalue_to_int(startIdxVal);
    char* match = yyvalue_to_string(matchVal);
    char *matchTarget = &search[startIdx];

    char *p1 = matchTarget;
    char *p2 = match;
    // checks whether p1 and p2 share prefix, and the length of the prefix is 
    // the shorter of the p1 and p2
    while (*p1 && *p2 && *p1 == *p2) {
        p1++;
        p2++;
    }
    // if p2 is empty, then p1 and p2 share prefix, 
    // if p1 is empty but p2 is not, then it is not a match!
    if (!*p2) {
        return bool_to_yyvalue(true);
    } else {
        return bool_to_yyvalue(false);
    }

}

yyvalue yy_豫言字符串获取字节序数当前字符(yyvalue strVal, yyvalue idxVal){
    char* s = yyvalue_to_string(strVal);
    int64_t idx = yyvalue_to_int(idxVal);
    char *result = &s[idx];
    char *end = result;
    while(*end){
        end+=1;
        // when the highest bit is not 10, it is the start of a character
        if ((*end  &  0xC0) != 0x80) {
            break;
        }
    }

    int64_t len = end - result;
    char *newStr = malloc(end-result + 1);

    if (newStr != NULL) {
        memcpy(newStr, result, len);
        newStr[len] = '\0'; // null-terminate the string
    }
    yyvalue ret_value = malloc_string_to_yyvalue(len + 1, newStr);
    free(newStr);
    return ret_value;
}

// startIdx 必须是引号，返回获取的字符串与前进的字符数
yyvalue yy_豫言字符串获取JSON字符串(yyvalue strVal, yyvalue startIdxVal){
    char* s = yyvalue_to_string(strVal);
    int64_t startIdx = yyvalue_to_int(startIdxVal);
    char *start = &s[startIdx];
    if (*start != '"'){
        errorAndAbort("JSON字符串必须以引号开始");
    }
    char *end = start;
    while(*end){
        end+=1;
        if (*end == '\\') {
            end+=1;
            continue;
        }
        if (*end == '"') {
            break;
        }
    }
    if (*end != '"'){
        errorAndAbort("JSON字符串必须以引号结束");
    }
    int64_t byteLength = end-start + 1;
    char* escapedStr =  malloc(byteLength);
    char* originalPtr = start+1;
    char* escapedPtr = escapedStr;
    while(originalPtr != end){
        if (*originalPtr == '\\') {
            originalPtr+=1;
            switch (*originalPtr)
            {
            case 'n':
                *escapedPtr = '\n';
                break;
            case 't':
                *escapedPtr = '\t';
                break;
            case 'r':
                *escapedPtr = '\r';
                break;
            case 'b':
                *escapedPtr = '\b';
                break;
            case 'f':
                *escapedPtr = '\f';
                break;
            case '\\':
                *escapedPtr = '\\';
                break;
            case '/':
                *escapedPtr = '/';
                break;
            case '"':
                *escapedPtr = '"';
                break;
            default:
                errorAndAbort("JSON字符串中的转义字符不合法");
                break;
            }
        } else {
            *escapedPtr = *originalPtr;
        }
        originalPtr+=1;
        escapedPtr+=1;
    }
    *escapedPtr = '\0';
    yyvalue escapedStrValue = malloc_string_to_yyvalue(strlen(escapedStr) + 1, escapedStr);
    free(escapedStr);
    return tuple_to_yyvalue(2, (yyvalue[]){escapedStrValue, int_to_yyvalue(byteLength)});
}

//https://stackoverflow.com/questions/32936646/getting-the-string-length-on-utf-8-in-c
size_t count_utf8_code_points(const char *s) {
    size_t count = 0;
    while (*s) {
        count += (*s++ & 0xC0) != 0x80;
    }
    return count;
}

yyvalue yyGetCodePoints(yyvalue str_addr) {
    const char* start = yyvalue_to_string(str_addr);
    const char* end = start;
    while(*end){
        end+=1;
    }

    // Count the number of UTF-8 code points
    int numCodePoints = 0;
    const char* p = start;
    while (p != end) {
        if ((*p & 0xC0) != 0x80) {
            numCodePoints++;
        }
        p++;
    }

    // Allocate a heap array for the code points
    yyvalue codePoints = yy_gcAllocateTuple(numCodePoints);
    // if (codePoints == NULL) {
    //     // Handle allocation failure
    //     return NULL;
    // }

    // Extract the code points
    p = start;
    const char* codePointStart = p;
    int i = 0;
    while (p != end) {
        if ((*p & 0xC0) != 0x80) {
            if (p != codePointStart) {
                int len = p - codePointStart;
                char* newChar = malloc((len + 1) * sizeof(char));
                strncpy(newChar, codePointStart, len);
                newChar[len] = '\0';
                yyvalue newCharValue = malloc_string_to_yyvalue(len + 1, newChar);
                free(newChar);
                yy_write_tuple(codePoints, i, newCharValue);
                i++;
            }
            codePointStart = p;
        }
        p++;
    }

    // Handle the last code point
    if (p != codePointStart) {
        int len = p - codePointStart;
        char* newChar = malloc((len + 1) * sizeof(char));
        strncpy(newChar, codePointStart, len);
        newChar[len] = '\0';
        yyvalue newCharValue = malloc_string_to_yyvalue(len + 1, newChar);
        yy_write_tuple(codePoints, i, newCharValue);
        free(newChar);
    }

    yyvalue ret =  tuple_to_yyvalue(2, (yyvalue[]){codePoints, int_to_yyvalue(numCodePoints)});
    verify_yyvalue(ret, true, 0);
    return ret;
}

// EVEN MORE EFFICIENT
yyvalue yyCodePointsConcat(yyvalue str_list_addr) {
    verify_yyvalue(str_list_addr, true, 0);
    const int length = iso_list_get_length(str_list_addr);
    yyvalue* strs = iso_list_get_elements(str_list_addr);

    int totalLength = 0;

    for (int i = 0; i < length; i++) {
        yyvalue curStr = strs[i];
        // fprintf(stderr, "concating %d curStr: %s\n", i , yyvalue_to_string(curStr));
        // if(strlen(yyvalue_to_string(strs[i])) != yyvalue_get_strlen(strs[i])) {
        //     verify_yyvalue(strs[i], true, 0);
        // }
        assert(strlen(yyvalue_to_string(strs[i])) == yyvalue_get_strlen(strs[i]));
        totalLength += yyvalue_get_strlen(strs[i]);
    }

    yyvalue ret = yy_gcAllocateStringBuffer(totalLength + 1);

    char* resultString = yyvalue_to_heap_string_pointer(ret);
    char* currentPos = resultString;

    for (int i = 0; i < length; i++) {
        const char* currentStr = yyvalue_to_string(strs[i]);
        assert(strlen(currentStr) == yyvalue_get_strlen(strs[i]));
        int strLength = yyvalue_get_strlen(strs[i]);
        memcpy(currentPos, currentStr, strLength);
        currentPos += strLength;
    }

    *currentPos = '\0';


    assert(currentPos - resultString == totalLength);
    // char* resultStringRet = malloc(totalLength + 1);
    // memcpy(resultStringRet, resultString, totalLength + 1);
    // free(resultString);
    // yyvalue ret = malloc_string_to_yyvalue(totalLength + 1, resultStringRet);
    // fprintf(stderr, "result %s, strlen = %d, arraylen= " PRIu64 "\n", resultStringRet, totalLength, yyvalue_get_heap_pointer_length(ret));
    // free(resultStringRet);
    verify_yyvalue(ret, true, 0);
    return ret;
}
