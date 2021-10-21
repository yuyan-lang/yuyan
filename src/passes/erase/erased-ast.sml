structure ErasedAST =
struct

    
    (* datatype erasedexpr = EUnit 
                  | ETuple of kvalue list
                  | KInj of Label * kvalue
                  | KFold of kvalue
                  | KAbs of (kvalue -> kcomputation)
                  | KProj of kcomputation * int
                | KCases of kcomputation * (Label * (value -> kcomputation)) list
                | KUnfold of kcomputation 
                | KApp of kcomputation  * kcomputation
                | KAppWithEvaledFun of (kvalue -> kcomputation)  * kcomputation (* only used intermediately *)
                | KRet of kvalue

                  
    type t = erasedexpr *)

end