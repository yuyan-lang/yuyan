structure UTF8String = struct

    type utf8string = UTF8.wchar list
    type t = utf8string




    fun isSubstring(s1 : utf8string) (s2 : utf8string) = 
        if List.length s1 > List.length s2 then false else
        case (s1, s2) of
            (c1::s1tl, c2::s2tl) => if c1 = c2 andalso isSubstring s1tl s2tl
                                    then true
                                    else isSubstring s2 s2tl
            | ([], []) => true
            | _ => false

    fun fromString(s : string) : utf8string = UTF8.explode(s)
    fun toString(s : utf8string) : string = UTF8.implode(s)
                                    
    fun size(s : utf8string) : int = List.length s

    

end

