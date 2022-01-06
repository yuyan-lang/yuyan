
#include "../globalInclude.h"


// returns if s1 is a substring of s2
yy_ptr yyIsSubstring(yy_ptr s1, yy_ptr s2) {
    if (strstr(addr_to_string(s2), addr_to_string(s1)) != NULL) {
        return bool_to_addr(true);
    } else {
        return bool_to_addr(false);
    }
}
