structure UTF8Char = struct

    datatype utf8char = UTF8Char of UTF8.wchar * SourceLocation.t option
    type t = utf8char 

    exception UTF8fromStringTooLong of string
    (* returns the string reprep, this is DIFFERENT FROM UTF8.tostring which 
    returns "\\uXXXXXXXX"*)

    fun fromUTF8Char (c : UTF8.wchar)(sourceLoc : SourceLocation.t option) : utf8char = UTF8Char(c, sourceLoc)
    fun asUTF8WChar (c : utf8char) : UTF8.wchar = case c of UTF8Char (w, _) => w
    fun toString(c : utf8char) : string = UTF8.implode ([asUTF8WChar c])

    fun semanticEqual (c1 : utf8char) (c2: utf8char) : bool = 
    let 
    (* val _ = print ("semanticEqual called on " ^ toString c1 ^ " and " ^ toString c2 ^ "\n") *)
    val result = asUTF8WChar c1 = asUTF8WChar c2
    (* val _ = print ("result is " ^ Bool.toString result ^"\n") *)
    in result end

    fun ~= (c1, c2) = semanticEqual c1 c2


    fun fromString(c : string) (sourceLoc : SourceLocation.t option) : utf8char = case UTF8.explode c of 
        [c] => UTF8Char (c, sourceLoc)
        | _ => raise UTF8fromStringTooLong (c ^ " has more than 1 utf8 characters")

    fun getWidth(c : utf8char) : int = 
        if UTF8.isAscii (asUTF8WChar c ) then 1 else 2

    fun getSourceLocation(c : utf8char) : SourceLocation.t option = 
        case c of (UTF8Char (_, r)) => r
end