(* Adapted from Appel's Compiling with Continuations 1992 *)
structure CPSAst = struct
open TypeCheckingAST

datatype cpsBuiltinValue = 
        CPSBvInt of int
        | CPSBvBool of bool
        | CPSBvString of UTF8String.t
        | CPSBvReal of real

type cpsvar = int

  datatype cpsvalue = CPSVar of cpsvar
    
    datatype cpscomputation = 
                  CPSUnit of (cpsvar * cpscomputation)
                | CPSProj of cpsvalue * int * (cpsvar * cpscomputation)
                | CPSCases of cpsvalue * (cpsvar * cpscomputation) list 
                | CPSUnfold of cpsvalue * (cpsvar * cpscomputation) 
                | CPSApp of cpsvalue  * (cpsvalue * cpsvalue) (* !!! *)
                | CPSAppSingle of cpsvalue  * cpsvalue  (* !!! *)
                | CPSTuple of cpsvalue list * (cpsvar * cpscomputation)
                | CPSInj of Label * int * cpsvalue * (cpsvar * cpscomputation)
                | CPSFold of cpsvalue * (cpsvar * cpscomputation)
                | CPSAbsSingle of (cpsvar (* this is the continuation (return address) *)
                * cpscomputation)  * cpsvar list option (* list of free variables in the function closure (first argument) (to be populated during closure conversion) *)
                * (cpsvar * cpscomputation) (* continuation where cpsvar is bound to the abstraction*)
                | CPSAbs of (cpsvar * cpsvar (* this is the continuation (return address) *)
                * cpscomputation)  * cpsvar list option (* list of free variables in the function closure (first argument) (to be populated during closure conversion) *)
                * (cpsvar * cpscomputation)(* continuation where cpsvar is bound to the abstraction*)
                | CPSDone of cpsvalue (* signals return of the value *)
                | CPSBuiltinValue of cpsBuiltinValue * (cpsvar * cpscomputation) (* actually should only use label when it is 
                  a builtin in fuction for pk, but since we're not doing serialization yet, this is fine *)
    type cpscontinuation = int * cpscomputation

end