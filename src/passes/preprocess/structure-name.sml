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
    fun localName x = x@[UTF8String.fromString "__BUILTIN_STRUCTURE_NAME_LOCAL"]

end
