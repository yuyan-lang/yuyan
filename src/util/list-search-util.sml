
structure ListSearchUtil = struct

    exception NotFound 
    exception NotFoundStr of string
    exception NotFoundSName  of StructureName.t


fun lookupGeneric (l : ('k  * 'a ) list) (key : 'k) (keyeq : 'k -> 'k -> bool) : 'a = 
    case l of
        [] => raise NotFound 
        | ((k, v) :: t) => if keyeq k key then v else lookupGeneric t key keyeq
fun lookupStr (l : (string *'a ) list) (key : string) : 'a = 
    lookupGeneric l key (fn x => fn y => x = y)
    handle NotFound => raise NotFoundStr key

fun findStr (l : (string *'a ) list) (key : string) : 'a option = 
    SOME (lookupGeneric l key (fn x => fn y => x = y))
    handle NotFound => NONE

fun lookupSName (l : (StructureName.t *'a ) list) (key : StructureName.t) : 'a =    
    lookupGeneric l key (StructureName.semanticEqual)
    handle NotFound => raise NotFoundSName key

fun findSName (l : (StructureName.t *'a ) list) (key : StructureName.t) : 'a option =    
    SOME (lookupGeneric l key (StructureName.semanticEqual))
    handle NotFound => NONE

fun lookupUTF8Str (l : (UTF8String.t *'a ) list) (key : UTF8String.t) : 'a =    
    lookupGeneric l key (UTF8String.semanticEqual)
    (* handle NotFound => raise NotFoundUTF8 key *)

fun findUTF8Str (l : (UTF8String.t *'a ) list) (key : UTF8String.t) : 'a option =    
    SOME(lookupGeneric l key UTF8String.semanticEqual)
    handle NotFound => NONE



fun indexOf (l : int list) (elem : int ) =
    let 
        fun g idx = if idx = length l then NONE else 
                    if List.nth(l, idx) = elem then SOME idx else g (idx +1)
    in g 0
    end

end