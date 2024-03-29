structure UTF8String = struct

    type utf8string = UTF8Char.t list
    type t = utf8string


    fun toString(s : utf8string) : string = UTF8.implode((map UTF8Char.asUTF8WChar s))

    fun semanticEqual(s1 : utf8string) (s2 : utf8string) = 
        case (s1, s2) of
            (c1::s1tl, c2::s2tl) => if UTF8Char.semanticEqual c1 c2 andalso semanticEqual s1tl s2tl
                                    then true
                                    else false
            | ([], []) => true
            | _ => false

    val ~= = UTF8Char.~=
    infix 4 ~= 

    fun isPrefix(s1 : utf8string) (s2 : utf8string) = 

        if List.length s1 > List.length s2 then false else
        case (s1, s2) of
            (c1::s1tl, c2::s2tl) => if c1 ~= c2 andalso isPrefix s1tl s2tl
                                    then true
                                    else false
            | ([], _) => true
            | _ => false

    (* returns true if s1 is a substring of s2 *)
    fun isSubstring(s1 : utf8string) (s2 : utf8string) = 
    let 
        (* val _ = DebugPrint.p ("isSubstring called on "^ toString s1 ^ " and " ^ toString s2 ^ "\n") *)
        val res = 
        if List.length s1 > List.length s2 then false else
        case (s1, s2) of
             ([], _) => true
            | (s1,  c2::s2tl) => if semanticEqual s1 (List.take(s2, length s1)) 
                                    then true
                                    else isSubstring s1 s2tl
            | _ => false
        (* val _ = DebugPrint.p ("[finished] isSubstring called on "^ toString s1 ^ " and " ^ toString s2 ^ " result is " ^ Bool.toString res ^ "\n" ) *)
    in

        res
    end

    fun isSuffix (s1 : utf8string) (s2 : utf8string) = 
        isPrefix (List.rev s1) (List.rev s2)

(* fromString does not annotate the string with file information *)
    fun fromString(s : string) : utf8string = map (fn c => UTF8Char.fromUTF8Char c NONE) (UTF8.explode(s))


    (* s1 is the suffix *)
    fun stripSuffix (s1 : utf8string) (s2 : utf8string) = 
        if List.length s1 > List.length s2 then 
        raise Fail "Cannot strip prefix" else
        List.rev(List.drop(List.rev s2, length s1))


(* fromStringAndFile annotates the string with file information *)
    fun fromStringAndFile(s : string) (fp: string) : utf8string = 
    let open UTF8Char
    in
    #3 (foldl (fn (char, (line, col, str)) => 
        let val char' = UTF8Char.fromUTF8Char char (SOME (fp, line, col))
        in
            if  semanticEqual char' (SpecialChars.newline)
            then (line+1, 0, str@[char']) (* we choose to keep everything here because mixed string might not want to remove spaces inside string *)
            else if semanticEqual char' SpecialChars.tab orelse semanticEqual char' SpecialChars.space
            then (line, col+1, str@[char'])
            else (line, col+1, str@[char'])
        end
        ) (0,0,[]) (UTF8.explode(s)))
    end

    fun stripTail (s : utf8string) = List.take (s, List.length(s) -1)
    fun size(s : utf8string) : int = List.length s



    fun fields ( f : UTF8Char.t -> bool) (s : utf8string) : utf8string list = 
        let val (res, pending) = 
        (foldl (fn (c, (res, pending)) => 
            if f c  then ((res@[pending], []) 
            ) else (res, pending@[c])
        ) ([], []) s)
        in 
        res@[pending]
        end

    fun tokens ( f : UTF8Char.t -> bool) (s : utf8string) : utf8string list = 
        List.filter (fn l => l <> []) (fields f s)

    fun containsChar (s : utf8string) (char : UTF8Char.t) = List.exists (fn sc =>UTF8Char.semanticEqual sc char) s

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

    fun getSourceRange (s : utf8string ) (callerDebugId : string): SourceRange.t = 
        case s of 
            [] => raise Fail ("attempt to get sourcerange for empty string" ^ toString s ^ callerDebugId)
            | (UTF8Char.UTF8Char(c, loc) :: t) => (
                case loc of 
                    NONE => raise Fail ("attempt to get sourcerange for NONE location: " ^ toString s ^ callerDebugId)
                    | SOME(fname, startLine, startCol) => 
                        let val UTF8Char.UTF8Char(lc, lloc) = List.nth(s, length s -1)
                        in case lloc of 
                            NONE => raise Fail "99: internal : should not happen: either all chars have range info, or not!"
                            | SOME(_, endLine, endCol) => 
                                SourceRange.StartEnd (fname, startLine, startCol, endLine, endCol+1)
                        end
            )
    fun getBytes(s : utf8string) : char list =
        String.explode (toString s)

(* move this functionality to mixed string directly *)
    (* fun removeAllWhitespace (s : utf8string ) : utf8string =
        (List.filter (fn c => not (List.exists (fn x => UTF8Char.semanticEqual x c) [
            SpecialChars.tab, SpecialChars.newline, SpecialChars.space
        ])) s)

    fun removeAllWhitespacePlain (s : string ) : string =
        toString (List.filter (fn c => not (List.exists (fn x => x = c) [
            SpecialChars.tab, SpecialChars.newline, SpecialChars.space
        ])) (fromString s)) *)
    (* fun removeAllWhitespace (s : string ) : string =
        String.implode (List.filter (fn c => not (Char.isSpace c)) (String.explode s)) *)



end

