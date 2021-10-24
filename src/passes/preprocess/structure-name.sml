structure StructureName =
struct
type structureName = UTF8String.t list
type t = structureName
val separatorChar = UTF8Char.fromString "之"
    fun toString x = case x of
        [x] => x
        | (x :: xs) => x@[separatorChar]@toString xs

    fun toStringPlain x = UTF8String.toString (toString x)
    val topLevelName = [UTF8String.fromString "__BUILTIN_STRUCTURE_NAME_TOP_LEVEL"]
    fun localName () = [UTF8String.fromString ("__BUILTIN_STRUCTURE_NAME_LOCAL_" ^ Int.toString(UID.next()))]


    fun isPrefix(s1 : structureName) (s2 : structureName) = 

        if List.length s1 > List.length s2 then false else
        case (s1, s2) of
            (c1::s1tl, c2::s2tl) => if c1 = c2 andalso isPrefix s1tl s2tl
                                    then true
                                    else false
            | ([], _) => true
            | _ => false


    fun stripPrefix(s1 : structureName) (s2 : structureName) = 

        if List.length s1 > List.length s2 then 
        raise Fail "Cannot strip prefix" else
        List.drop(s2, length s1)

end
