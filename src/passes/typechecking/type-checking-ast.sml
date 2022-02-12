structure TypeCheckingAST = struct
    open Operators

    type EVar = UTF8String.t
    type TVar = UTF8String.t

    type Label = UTF8String.t

    datatype BuiltinType = BIString
                         | BIInt
                         | BIReal
                         | BIBool
                         | BIDynClsfd (* dynamically classified value *)
                         | BIForeignType of int (* an abstract type denoting some c structure *)
                         
    datatype BuiltinFunc = BFCallCC
                         | BFNewDynClsfdValueWithString
                         | BFRaise
                         | BFHandle
                         | BFIntSub
                         | BFIntEq

    datatype CType =  CTypeVar of StructureName.t
                    | CUnitType
                    | CProd of (Label * CType) list
                    | CLazyProd of (Label * CType) list
                    | CNullType
                    | CSum of (Label * CType) list
                    | CFunc of CType * CType
                    | CTypeInst of CType * CType
                    | CForall of TVar * CType
                    | CExists of TVar * CType
                    | CRho of TVar * CType
                    | CBuiltinType of BuiltinType

    datatype RType =  RTypeVar of StructureName.t
                    | RUnitType
                    | RProd of (Label * RType) list
                    | RLazyProd of (Label * RType) list
                    | RNullType
                    | RSum of (Label * RType) list
                    | RFunc of RType * RType
                    | RTypeInst of RType * RType
                    | RForall of TVar * RType
                    | RExists of TVar * RType
                    | RRho of TVar * RType
                    | RBuiltinType of BuiltinType

    datatype visibility = Public | Private


    (* CExpr for checked expr *)
    datatype CExpr = CExprVar of StructureName.t (* required to be fully qualified name, if not local *)
                    | CUnitExpr
                    | CTuple of CExpr list * CType (* type is Prod *)
                    | CLazyTuple of CExpr list * CType (* type is Prod *)
                    | CProj of CExpr * Label * CType (* type is Prod *)
                    | CLazyProj of CExpr * Label * CType (* type is Prod *)
                    | CInj of Label * CExpr  * CType (* type is  Sum *)
                    | CIfThenElse of CExpr * CExpr * CExpr  (* remove after type inference *)
                    | CCase of (CType (*type is Sum *) * CExpr) * (Label * EVar * CExpr) list * CType (* type is result type *)
                    | CLam of  EVar * CExpr * CType (* type is Func *)
                    | CApp of  CExpr * CExpr * CType (* type is Func *)
                    | CTAbs of TVar * CExpr  * CType(* type is Forall *)
                    | CTApp of CExpr * CType (* instantiation type *) * CType(* type is Forall *)
                    | CPack of CType (* pack type *) * CExpr * CType(* type is Exists *)
                    | COpen of (CType (* type is Exists *) * CExpr) * (TVar * EVar * CExpr) * CType(* type is return type *)
                    | CFold of CExpr  * CType(* type is Rho *)
                    | CUnfold of CExpr  * CType (* type is Rho *)
                    | CFix of EVar * CExpr * CType (* type is the typ of the expression *)
                    | CStringLiteral of UTF8String.t 
                    | CIntConstant of int
                    | CRealConstant of real
                    | CBoolConstant of bool
                    | CLetIn of CDeclaration list * CExpr * CType (* Type is the result of the declaring expression *)
                    | CFfiCCall of UTF8String.t * StructureName.t list
                    | CBuiltinFunc of BuiltinFunc
                    | CSeqComp of CExpr * CExpr * CType * CType (* type is the type of the second expression *)

(* all types are fully normalized *)
    and CDeclaration = 
                        (* Do not need type macro becuase all types for later stages have been expanded 
                        CHANGE: for imports/lsp, still need type macro*)
                        CTypeMacro of StructureName.t * CType 
                        (* Do not need type info as terms have been annotated *)
                        (* CTermTypeJudgment of UTF8String.t * CType *)
                        (* Fold into Term Definition *)
                       (*  CTermMacro of UTF8String.t * CExpr *)
                       | CTermDefinition of StructureName.t * CExpr * CType  
                       | CDirectExpr of CExpr * CType
                       | CImport of (StructureName.t  * FileResourceURI.t)
                       (* | CStructure of bool * UTF8String.t * CDeclaration list *)
                       (* Do not need open : Require all references to open use fully qualified name  *)
                       (* | COpenStructure of StructureName.t *)

    (* stores the source level information that directly correponds to opCompString, that can 
    be used to resconstruct the expression *)
    type sourceOpInfo = Operators.operator (* should be the operator except rapp *)
    (* RExpr for raw expr *)
    datatype RExpr = RExprVar of StructureName.t
                    | RUnitExpr of sourceOpInfo
                    | RTuple of RExpr list * (sourceOpInfo list) (* n-1 op for n tuple *)
                    | RLazyTuple of RExpr list * (sourceOpInfo list) (* n-1 op for n tuple *)
                    | RProj of RExpr * Label * sourceOpInfo
                    | RLazyProj of RExpr * Label * sourceOpInfo
                    | RInj of Label * RExpr * sourceOpInfo
                    | RIfThenElse of RExpr * RExpr * RExpr * sourceOpInfo
                    | RCase of RExpr * (Label * EVar * RExpr) list * (sourceOpInfo  (* top case *)
                            * sourceOpInfo list  (* case separator *)
                            * sourceOpInfo list (* case clause *))
                    | RLam of EVar * RExpr * sourceOpInfo
                    | RLamWithType of RType * EVar * RExpr * sourceOpInfo
                    | RApp of RExpr * RExpr * sourceOpInfo (* if op is not app, then custom operators *)
                    | RTAbs of TVar * RExpr * sourceOpInfo
                    | RTApp of RExpr * RType * (sourceOpInfo* UTF8String.t) (* string represents the type information itself *)
                    | RPack of RType * RExpr * (UTF8String.t * sourceOpInfo)
                    | ROpen of RExpr * (TVar * EVar * RExpr) * sourceOpInfo
                    | RFold of RExpr * sourceOpInfo
                    | RUnfold of RExpr * sourceOpInfo
                    | RFix of EVar * RExpr * sourceOpInfo
                    | RStringLiteral of UTF8String.t  * MixedStr.quoteinfo
                    | RIntConstant of int * UTF8String.t
                    | RRealConstant of real * UTF8String.t
                    | RBoolConstant of bool * UTF8String.t
                    | RLetIn of RDeclaration list * RExpr * sourceOpInfo
                    | RFfiCCall of RExpr * RExpr * sourceOpInfo 
                    | RBuiltinFunc of BuiltinFunc * UTF8String.t (* source info *)
                    | RSeqComp of RExpr * RExpr * sourceOpInfo
                    (* | RUniverse of UTF8String.t a universe is the type of types, stratified by level *)
                    


    and RDeclaration = 
                         RTypeMacro of UTF8String.t * RType
                       | RTermTypeJudgment of UTF8String.t * RType
                       | RTermMacro of UTF8String.t * RExpr
                       | RTermDefinition of UTF8String.t * RExpr
                       | RDirectExpr of RExpr
                       | RStructure of bool * UTF8String.t * RDeclaration list
                       (*  public visible * name * signature *)
                       | ROpenStructure of StructureName.t
                       | RReExportStructure of StructureName.t
                       | RImportStructure of (StructureName.t (* name *) * 
                                              FileResourceURI.t  (* file location *)
                                              )

    type CSignature = CDeclaration list
    type RSignature = RDeclaration list


(* these exist here for pretty printing *)
(* g for generic *)
 datatype 'a gmapping = TermTypeJ of StructureName.t * RType  * 'a
                    | TypeDef of StructureName.t * RType * unit
datatype 'a gcontext = Context of StructureName.t * bool * 
    ('a gmapping) list
    type mapping = (StructureName.t option) gmapping (* original name (for use with open) *)
    type context = (StructureName.t option) gcontext (* original name (for use with open) *)

end