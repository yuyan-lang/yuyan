
#include "globalInclude.h"


// returns if s1 is a substring of s2
yy_ptr yyIsSubstring(yy_ptr s1, yy_ptr s2) {
    if (strstr(addr_to_string(s2), addr_to_string(s1)) != NULL) {
        return bool_to_addr(true);
    } else {
        return bool_to_addr(false);
    }
}

// returns if s1 is a substring of s2
yy_ptr yyStringEq(yy_ptr s1, yy_ptr s2) {
    char *str2 = addr_to_string(s2);
    char *str1 = addr_to_string(s1);
    if (strcmp(str2, str1) == 0)
    {
        return bool_to_addr(true);
    }
    else
    {
        return bool_to_addr(false);
    }
}

yy_ptr yyStringByteLength(yy_ptr s1){
    char *s = addr_to_string(s1);
    int64_t l = strlen(s);
    return int_to_addr(l);
}


yy_ptr yy_豫言字符串获取字节数组(yy_ptr s1){
    char *s = addr_to_string(s1);
    return string_to_addr(s);
}

yy_ptr yy_豫言字符转整数(yy_ptr s1, yy_ptr idx_ptr){
    char *s = addr_to_string(s1);
    uint64_t index = addr_to_int(idx_ptr);
    return int_to_addr(s[index]);
}

char* yy_豫言子字符串从字节序数开始(char* s, int64_t idx){
    char *result = &s[idx];
    return result;
}

yy_ptr yy_豫言字符串匹配(char* search, int64_t startIdx, char* match) {
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
        return bool_to_addr(true);
    } else {
        return bool_to_addr(false);
    }

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
yy_ptr yyGetCodePoints(yy_ptr str_addr) {
    const char* start = addr_to_string(str_addr);

    const char * end = start;
    while(*end){
        end+=1;
    }

    const char* prevEnd = end;


    yy_ptr resultList = iso_list_nil_to_addr();


    while(end != start) {
        end --;
        if ((*end  &  0xC0) == 0x80) {
            continue;
        }
        // extract current character
        int charLength = prevEnd-end;
        char* newChar = GC_MALLOC(charLength+1);
        // newChar[charLength] = '\0'; // no need due to strlcpy
        strlcpy(newChar, end, charLength+1);
        resultList = iso_list_cons_to_addr(string_to_addr(newChar), resultList);
        prevEnd = end;
    }

    return resultList;

}


yy_ptr yyCodePointsConcat(yy_ptr str_list_addr) {
    const int length = iso_list_get_length(str_list_addr);
    yy_ptr* strs = iso_list_get_elements( str_list_addr);

    int totalLength = 0;

    for (int i = 0; i < length; i ++){
        totalLength += strlen(addr_to_string(strs[i]));
    }

    char * resultString = GC_MALLOC(totalLength + 1);
    resultString[0] = '\0';

    for (int i = 0; i < length; i ++){
        strlcat(resultString, addr_to_string(strs[i]), totalLength+1);
    }

    return string_to_addr(resultString);
}