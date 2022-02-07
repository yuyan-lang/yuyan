
structure CPSHelper = struct
open CPSAst
    fun kcc (cc : cpsvar -> cpscomputation) : cpscontinuation = 
        let val v = CPSVarLocal (UID.next())
        in (v, cc v) end
    fun kcc' (cc : int -> cpscomputation) : (int * cpscomputation) = 
        let val v = (UID.next())
        in (v, cc v) end
    fun kcc2 (cc : cpsvar -> cpsvar -> cpscomputation) : cpsvar * cpsvar * cpscomputation = 
        let val v1 = CPSVarLocal (UID.next())
        val v2 = CPSVarLocal (UID.next())
        in (v1, v2, cc v1 v2) end
    fun kcc2' (cc : int -> int -> cpscomputation) : int * int * cpscomputation = 
        let val v1 = (UID.next())
        val v2 = (UID.next())
        in (v1, v2, cc v1 v2) end
end