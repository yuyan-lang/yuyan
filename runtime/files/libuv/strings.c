
#include "../globalInclude.h"


// returns if s1 is a substring of s2
yyptr yyIsSubstring(yyptr s1, yyptr s2) {
    if (strstr(addr_to_string(s2), addr_to_string(s1)) != NULL) {
        return bool_to_addr(true);
    } else {
        return bool_to_addr(false);
    }
}
