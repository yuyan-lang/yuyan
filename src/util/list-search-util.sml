
structure ListSearchUtil = struct

    exception NotFound 


fun lookupGeneric (l : ('k  * 'a ) list) (key : 'k) (keyeq : 'k -> 'k -> bool) : 'a = 
    case l of
        [] => raise NotFound 
        | ((k, v) :: t) => if keyeq k key then v else lookupGeneric t key keyeq
fun lookupStr (l : (string *'a ) list) (key : string) : 'a = 
    lookupGeneric l key (fn x => fn y => x = y)

fun lookupSName (l : (StructureName.t *'a ) list) (key : StructureName.t) : 'a =    
    lookupGeneric l key (StructureName.semanticEqual)

end