(* Adapted from Appel's Compiling with Continuations 1992 *)
structure CPSAST = struct
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
                | CPSFix of (
                  cpsvar (* this represents the fix itself (for use inside function body *) 
                  * cpsvar (* this represents the continuation for use inside fix body*)
                  * cpscomputation) * 
                  (cpsvar  (* the result of fixed point will be stored here *)
                  * cpscomputation)
                | CPSTuple of cpsvalue list * (cpsvar * cpscomputation)
                | CPSInj of Label * int * cpsvalue * (cpsvar * cpscomputation)
                | CPSFold of cpsvalue * (cpsvar * cpscomputation)
                | CPSAbsSingle of (cpsvar (* this is the continuation (return address) *)
                * cpscomputation) * (cpsvar * cpscomputation)
                | CPSAbs of (cpsvar * cpsvar (* this is the continuation (return address) *)
                * cpscomputation) * (cpsvar * cpscomputation)
                | CPSDone (* signals return *)
                | CPSBuiltinValue of cpsBuiltinValue * (cpsvar * cpscomputation) (* actually should only use label when it is 
                  a builtin in fuction for pk, but since we're not doing serialization yet, this is fine *)
    type cpscontinuation = int * cpscomputation

end
