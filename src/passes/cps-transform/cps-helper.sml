
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

     fun clams (argCount : int)
        (* compiles function with n consecutive lambda abstractions, 
        the function result is passed into cc *)
        (acc : cpsvar list)
            (body : cpsvar list  * (cpsvar -> cpscomputation) -> cpscomputation)
         (cc : cpsvar -> cpscomputation) : cpscomputation = 
        if argCount = 0
        then body (acc, cc)
        else
                CPSAbs (kcc2' (fn arg => fn ret =>
                    clams (argCount - 1) (acc@[CPSVarLocal arg]) body
                        (fn r => CPSAppSingle(CPSValueVar (CPSVarLocal ret),CPSValueVar r))
                ), NONE, kcc (fn f => 
                    ( 
                        (* registerFunctionNameMapping f originalExpr "Body of"; *)
                        cc f
                    )
                ))
     fun clams1 
            (body : cpsvar  * (cpsvar -> cpscomputation) -> cpscomputation)
         (cc : cpsvar -> cpscomputation) : cpscomputation = 
         clams 1 []
         (fn args => case args of 
                ([a1], k) => body (a1,k)
                | _ => raise Fail "ch44: args count mismatch"
         )
         cc

     fun clams2 
            (body : (cpsvar * cpsvar) * (cpsvar -> cpscomputation) -> cpscomputation)
         (cc : cpsvar -> cpscomputation) : cpscomputation = 
         clams 2 []
         (fn args => 
            case args of 
                ([a1,a2], k) => body ((a1,a2),k)
                | _ => raise Fail "ch44: args count mismatch"
         )
         cc
     fun clams3
            (body : (cpsvar * cpsvar * cpsvar) * (cpsvar -> cpscomputation) -> cpscomputation)
         (cc : cpsvar -> cpscomputation) : cpscomputation = 
         clams 3 []
         (fn args => 
            case args of 
                ([a1,a2, a3], k) => body ((a1,a2, a3),k)
                | _ => raise Fail "ch44: args count mismatch"
         )
         cc
end