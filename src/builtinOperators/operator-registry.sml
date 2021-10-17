structure OperatorRegistry =
struct
    open Operators
    structure Store :DICT =RedBlackDict(structure Key =IntOrdered)
    type registry = operator Store.dict
    type t = registry


    fun find(s : registry) (key : int) = Store.lookup s key
    fun build(s : operator list) : registry  = 
        foldr( fn (oper, acc) => Store.insert acc (getUID oper) oper) Store.empty s


end