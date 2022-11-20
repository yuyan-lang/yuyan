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

    datatype cpspattern = CPSPatHeadSpine of ( int (* constructor id *) * cpspattern list (* args *))
                 | CPSPatVar of cpsvar
                 | CPSPatBuiltin of cpsBuiltinValue
                 | CPSPatTuple of cpspattern list

    datatype cpsprimitiveop = 
      CPSPOpIntSub of cpsvalue * cpsvalue * (cpsvar * cpscomputation)
      | CPSPOpIntEq of cpsvalue * cpsvalue * (cpsvar * cpscomputation)
      | CPSPOpIntGt of cpsvalue * cpsvalue * (cpsvar * cpscomputation)


    and cpscomputation = 
                  CPSUnit of (cpsvar * cpscomputation)
                | CPSProj of cpsvalue * int * (cpsvar * cpscomputation)
                (* simple version, use when we have optimized pattern matching *)
                (* | CPSCases of cpsvalue * (int * (* constructor id *) 
                cpsvar list (* bound args *) 
                * cpscomputation) list  *)
                | CPSSimpleCases of cpsvalue * (int * (* constructor id *) cpsvar list (* bound args *) * cpscomputation) list 
                | CPSCases of cpsvalue * (cpspattern * cpscomputation) list  * string (* string is the exception text in case of failure *)
                | CPSIfThenElse of cpsvalue * cpscomputation * cpscomputation (* there is a bug in designing this case and the cases clause, the cc is being translated twice!!, maybe we need to fix it. !!! TODO!!!*)
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
                | CPSStore of cpsvar (* destination (must be global) *) * cpsvalue (*source *)* cpscomputation
                | CPSDone of cpsvalue (* signals return of the value *)
                | CPSBuiltinValue of cpsBuiltinValue * (cpsvar * cpscomputation) (* actually should only use label when it is 
                  a builtin in fuction for pk, but since we're not doing serialization yet, this is fine *)
                | CPSFfiCCall of UTF8String.t * cpsvalue list * (cpsvar * cpscomputation)
                | CPSDynClsfdIn of cpsvalue (* the name *) 
                                * int (* the UNIQUE ID associated with this classification *)
                                * cpsvalue (* the value *)
                                * (cpsvar * cpscomputation) (* the continuation *)
                | CPSDynClsfdMatch of cpsvalue (* the thing to be analized *)
                                * (int * (cpsvar * cpscomputation)) (* the success branch *)
                                * cpscomputation (* the otherwise branch *)
                | CPSPrimitiveOp of cpsprimitiveop
                (* | CPSSequence of cpscomputation list *)
    type cpscontinuation = cpsvar * cpscomputation

    datatype cpscontextvalue = PlainVar of cpsvar
                             | SelfVar of cpsvar (* selfvar represents a pending computation that needs to be applied to itself *)
                             | GlobalVar of cpsvar

    type context = (StructureName.t * cpscontextvalue) list

end
