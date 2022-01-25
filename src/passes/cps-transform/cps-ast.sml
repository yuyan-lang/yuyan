(* Adapted from Appel's Compiling with Continuations 1992 *)
structure CPSAst = struct
open TypeCheckingAST


datatype cpsvar =  CPSVarLocal of int
                | CPSVarGlobal of int

  datatype cpsvalue = CPSValueVar of cpsvar
  
    datatype cpsBuiltinValue = 
        CPSBvInt of int
        | CPSBvBool of bool
        | CPSBvString of UTF8String.t
        | CPSBvReal of real

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
                | CPSAbsSingle of (int (* this is the continuation (return address) *)
                * cpscomputation)  * int list option (* list of free variables (localvar) in the function closure (first argument) (to be populated during closure conversion) *)
                * (cpsvar * cpscomputation) (* continuation where cpsvar is bound to the abstraction*)
                | CPSAbs of (int * int (* this is the continuation (return address) *)
                * cpscomputation)  * int list option (* list of free variables (localvar) in the function closure (first argument) (to be populated during closure conversion) *)
                * (cpsvar * cpscomputation)(* continuation where cpsvar is bound to the abstraction*)
                | CPSDone of cpsvalue (* signals return of the value *)
                | CPSBuiltinValue of cpsBuiltinValue * (cpsvar * cpscomputation) (* actually should only use label when it is 
                  a builtin in fuction for pk, but since we're not doing serialization yet, this is fine *)
                | CPSFfiCCall of UTF8String.t * cpsvalue list * (cpsvar * cpscomputation)
    type cpscontinuation = cpsvar * cpscomputation

end
