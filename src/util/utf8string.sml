structure UTF8String = struct

    type utf8string = UTF8.wchar list
    type t = utf8string



    fun isPrefix(s1 : utf8string) (s2 : utf8string) = 

        if List.length s1 > List.length s2 then false else
        case (s1, s2) of
            (c1::s1tl, c2::s2tl) => if c1 = c2 andalso isPrefix s1tl s2tl
                                    then true
                                    else false
            | ([], _) => true
            | _ => false

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

    fun stripTail (s : utf8string) = List.take (s, List.length(s) -1)
    fun size(s : utf8string) : int = List.length s


    fun containsChar (s : utf8string) (char : UTF8Char.t) = List.exists (fn sc =>sc=char) s

    fun containsAllChar (s : utf8string) (chars : UTF8Char.t list) =
         foldr (fn (b1, b2) => b1 andalso b2) true 
            (map (fn c => containsChar s c) chars)

    fun containsSomeChar (s : utf8string) (chars : UTF8Char.t list) =
         foldr (fn (b1, b2) => b1 orelse b2) false 
            (map (fn c => containsChar s c) chars)

    fun concatWith (sep : utf8string) (elem : utf8string list) = 
    case elem of 
    [] => []
    | [s] => s
    | (x ::xs) => x @ sep @ concatWith sep xs


    (* fun removeAllWhitespace (s : string ) : string =
        toString (List.filter (fn c => not (List.exists (fn x => x = c) [
            SpecialChars.tab, SpecialChars.newline, SpecialChars.space
        ])) (fromString s)) *)
    fun removeAllWhitespace (s : string ) : string =
        String.implode (List.filter (fn c => not (Char.isSpace c)) (String.explode s))


end

