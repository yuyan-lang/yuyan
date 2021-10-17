structure UTF8Char = struct

    type utf8char = UTF8.wchar
    type t = utf8char 

    exception UTF8fromStringTooLong of string
    (* returns the string reprep, this is DIFFERENT FROM UTF8.tostring which 
    returns "\\uXXXXXXXX"*)
    fun toString(c : utf8char) : string = UTF8.implode ([c])
    fun fromString(c : string) : utf8char = case UTF8.explode c of 
        [c] => c
        | _ => raise UTF8fromStringTooLong (c ^ " has more than 1 utf8 characters")


end