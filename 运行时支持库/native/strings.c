
#include "common_include.h"


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
    char *str2 = yyvalue_to_string(s2);
    char *str1 = yyvalue_to_string(s1);
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
    char *s = yyvalue_to_string(s1);
    return string_to_yyvalue(s);
}

yyvalue yy_豫言字符转整数(yyvalue s1, yyvalue idx_ptr){
    char *s =  yyvalue_to_string(s1);
    uint64_t index = yyvalue_to_int(idx_ptr);
    char result = s[index];
    return int_to_yyvalue((unsigned char)result);
}

char* yy_豫言子字符串从字节序数开始(char* s, int64_t idx){
    char *result = &s[idx];
    return result;
}

yyvalue yy_豫言字符串匹配(char* search, int64_t startIdx, char* match) {
    char *matchTarget = yy_豫言子字符串从字节序数开始(search, startIdx);

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

char* yy_豫言字符串获取字节序数当前字符(char* s, int64_t idx){
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
    char *newStr = yy_gcAllocateBytes(end - result + 1);

    if (newStr != NULL) {
        memcpy(newStr, result, len);
        newStr[len] = '\0'; // null-terminate the string
    }
    return newStr;
}

// startIdx 必须是引号，返回获取的字符串与前进的字符数
yyvalue yy_豫言字符串获取JSON字符串(char* s, int64_t startIdx){
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
    int64_t byteLength = end-start+1;
    char* escapedStr = yy_gcAllocateBytes(byteLength);
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
    return tuple_to_yyvalue(2, (yyvalue[]){string_to_yyvalue(escapedStr), int_to_yyvalue(byteLength)});
}

//https://stackoverflow.com/questions/32936646/getting-the-string-length-on-utf-8-in-c
size_t count_utf8_code_points(const char *s) {
    size_t count = 0;
    while (*s) {
        count += (*s++ & 0xC0) != 0x80;
    }
    return count;
}

// get a list of utf8 code points from utf8 array
// yyvalue yyGetCodePoints(yyvalue str_addr) {
//     const char* start = yyvalue_to_string(str_addr);

//     const char * end = start;
//     while(*end){
//         end+=1;
//     }

//     const char* prevEnd = end;

//     yyvalue resultList = iso_list_nil_to_yyvalue();

//     while(end != start) {
//         end --;
//         if ((*end  &  0xC0) == 0x80) {
//             continue;
//         }
//         // extract current character
//         int charLength = prevEnd-end;
//         char* newChar = yy_gcAllocateBytes(charLength+1);
//         // newChar[charLength] = '\0'; // no need due to strlcpy
//         strlcpy(newChar, end, charLength+1);
//         resultList = iso_list_cons_to_yyvalue(string_to_yyvalue(newChar), resultList);
//         prevEnd = end;
//     }
//     return resultList;
// }
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
    yyvalue* codePoints = (yyvalue*)yy_gcAllocateBytes(numCodePoints * sizeof(yyvalue));
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
                char* newChar = (char*)yy_gcAllocateBytes((len + 1) * sizeof(char));
                // if (newChar == NULL) {
                //     // Handle allocation failure
                //     free(codePoints);
                //     return NULL;
                // }
                strncpy(newChar, codePointStart, len);
                newChar[len] = '\0';
                codePoints[i] = string_to_yyvalue(newChar);
                i++;
            }
            codePointStart = p;
        }
        p++;
    }

    // Handle the last code point
    int len = p - codePointStart;
    char* newChar = (char*)yy_gcAllocateBytes((len + 1) * sizeof(char));
    // if (newChar == NULL) {
    //     // Handle allocation failure
    //     free(codePoints);
    //     return NULL;
    // }
    strncpy(newChar, codePointStart, len);
    newChar[len] = '\0';
    codePoints[i] = string_to_yyvalue(newChar);

    return heap_array_to_yyvalue(numCodePoints, codePoints);
}


// yyvalue yyCodePointsConcat(yyvalue str_list_addr) {
//     const int length = iso_list_get_length(str_list_addr);
//     yyvalue* strs = iso_list_get_elements( str_list_addr);

//     int totalLength = 0;

//     for (int i = 0; i < length; i ++){
//         totalLength += strlen(yyvalue_to_string(strs[i]));
//     }

//     char * resultString = yy_gcAllocateBytes(totalLength + 1);
//     resultString[0] = '\0';

//     for (int i = 0; i < length; i ++){
//         strlcat(resultString, yyvalue_to_string(strs[i]), totalLength+1);
//     }

//     return string_to_yyvalue(resultString);
// }

// MORE MORE EFFICIENT
// yyvalue yyCodePointsConcat(yyvalue str_list_addr) {
//     const int length = iso_list_get_length(str_list_addr);
//     yyvalue* strs = iso_list_get_elements(str_list_addr);

//     int totalLength = 0;

//     for (int i = 0; i < length; i++) {
//         totalLength += strlen(yyvalue_to_string(strs[i]));
//     }

//     char* resultString = (char*)yy_gcAllocateBytes(totalLength + 1);
//     char* currentPos = resultString;

//     for (int i = 0; i < length; i++) {
//         const char* currentStr = yyvalue_to_string(strs[i]);
//         int strLength = strlen(currentStr);
//         memcpy(currentPos, currentStr, strLength);
//         currentPos += strLength;
//     }

//     *currentPos = '\0';

//     return string_to_yyvalue(resultString);
// }

// EVEN MORE EFFICIENT
yyvalue yyCodePointsConcat(yyvalue str_list_addr) {
    const int length = iso_list_get_length(str_list_addr);
    yyvalue* strs = iso_list_get_elements(str_list_addr);

    int totalLength = 0;
    int* lengths = (int*)malloc(length * sizeof(int));

    for (int i = 0; i < length; i++) {
        lengths[i] = yyvalue_get_strlen(strs[i]);
        totalLength += lengths[i];
    }

    char* resultString = (char*)malloc(totalLength + 1);
    char* currentPos = resultString;

    for (int i = 0; i < length; i++) {
        const char* currentStr = yyvalue_to_string(strs[i]);
        int strLength = lengths[i];
        memcpy(currentPos, currentStr, strLength);
        currentPos += strLength;
    }

    *currentPos = '\0';

    free(lengths);
    char* resultStringRet = (char*)yy_gcAllocateBytes(totalLength + 1);
    memcpy(resultStringRet, resultString, totalLength + 1);
    free(resultString);

    return string_to_yyvalue(resultStringRet);
}
