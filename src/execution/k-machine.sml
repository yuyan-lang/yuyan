structure KMachine =
struct
    open TypeCheckingAST

(* this K machine has efficiency issues due to unevaluated functions as values, 
please use persistent-k-machine for actual evaluation *)
    datatype kvalue = KUnit 
                  (* only used durning persistence, should not be used anywhere else *)
                  | KVar of int (* FOR DEBUG PURPOSES ONLY, do not use for compiling practical programs !!! *)
                  (* We should only run closed programs so KVar shouldn't appear for any practical purposes *)
                  | KTuple of (kvalue) list (* label is only for printing purposes *)
                  | KInj of  Label *int * kvalue (* label is only for printing purposes *)
                  | KFold of kvalue
                  | KAbs of (kvalue -> kcomputation)
                  | KComp of kcomputation
    and kcomputation = 
                  KProj of kcomputation * int
                | KCases of kcomputation * (kvalue -> kcomputation) list
                | KUnfold of kcomputation 
                | KApp of kcomputation  * kcomputation
                (* only used intermediately by the step machine, should not be used outside this module *)
                (* | KAppWithEvaledFun of (kvalue -> kcomputation)  * kcomputation only used intermediately *)
                | KRet of kvalue
                | KFix of kvalue -> kcomputation


    


    

end