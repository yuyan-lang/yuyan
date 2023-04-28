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
                         | BFRaiseString
                         | BFHandle
                         | BFHandleString
                         | BFIntSub
                         | BFIntEq
                         | BFIntGt

    datatype CBuiltinConstant = 
                     CStringLiteral of UTF8String.t 
                    | CIntConstant of int
                    | CRealConstant of (int * int * int )
                    | CBoolConstant of bool

    datatype visibility = Public | Private

    datatype plicity = Explicit | Implicit
    datatype cvartype = CVTBinder | CVTDefinition of CExpr | CVTConstructor of StructureName.t * cconstructorinfo (* canonical name and cinfo *)
                      | CVTBinderDefinition of StructureName.t (* a definition for binder , will map to metavar name, used in pattern matching *)

    and CPattern = CPatHeadSpine of (CExpr  * cconstructorinfo)  * CPattern list
                 | CPatVar of UTF8String.t 
                 | CPatBuiltinConstant of CBuiltinConstant
                 | CPatTuple of CPattern list
    (* CExpr for checked expr *)
    and CExpr = CVar of (StructureName.t (* required to be fully qualified name, if not local *)* 
                                cvartype (* the referenced expression, if not local *)
                            ) 
                    | CMetaVar of StructureName.t
                    | CUnitExpr
                    | CTuple of CExpr list * CTypeAnn (* type is Prod *)
                    | CLazyTuple of CExpr list * CTypeAnn (* type is Prod *)
                    | CProj of CExpr * int (* index of the label *) * CTypeAnn (* type is Prod *)
                    | CBlockProj of CExpr * Label * int (* index of the label, counting from zero, including cons, :, directexpr, def, excluding import *)
                    | CLazyProj of CExpr * Label * CTypeAnn (* type is Prod *)
                    (* | CInj of Label * CExpr  * CTypeAnn type is  Sum *)
                    | CIfThenElse of CExpr * CExpr * CExpr  (* remove after type inference *)
                    | CCase of (CTypeAnn (*type is to be pattern matched *) * CExpr) * 
                        (CPattern (* pattern *) * CExpr) list * CTypeAnn (* type is result type *)
                    | CLam of  EVar * CExpr * CTypeAnn (* type is Func *)
                    | CApp of  CExpr * CExpr * CTypeAnn (* type is Func *)
                    (* | CTAbs of TVar * CExpr  * CTypeAnn(* type is Forall *)
                    | CTApp of CExpr * CExpr (* instantiation type *) * CTypeAnntype is Forall *)
                    (* | CPack of CExpr (* pack type *) * CExpr * CTypeAnn(* type is Exists *)
                    | COpen of (CTypeAnn (* type is Exists *) * CExpr) * (TVar * EVar * CExpr) * CTypeAnntype is return type *)
                    (* | CFold of CExpr  * CTypeAnn(* type is Rho *)
                    | CUnfold of CExpr  * CTypeAnn type is Rho *)
                    | CFix of EVar * CExpr * CTypeAnn (* type is the typ of the expression *)
                    | CBuiltinConstant of CBuiltinConstant
                    (* | CLetIn of CDeclaration list * CExpr * CTypeAnn Type is the result of the declaring expression *)
                    | CLetInSingle of UTF8String.t * CExpr * CExpr (* Type is the result of the declaring expression *)
                    | CFfiCCall of UTF8String.t * CExpr list
                    | CBuiltinFunc of BuiltinFunc
                    | CSeqComp of CExpr * CExpr * CTypeAnn * CTypeAnn (* type is the type of the second expression *)
                    (* types *)
                    | CUnitType
                    | CProd of (CExpr) list
                    | CLabeledProd of (Label * CExpr) list
                    | CLazyProd of (Label * CExpr) list
                    | CNullType
                    | CSum of (Label * CExpr) list
                    (* | CFunc of CExpr * CExpr *)
                    (* | CTypeInst of CExpr * CExpr *)
                    (* | CForall of TVar * CExpr *)
                    (* | CExists of TVar * CExpr *)
                    (* | CRho of TVar * CExpr *)
                    | CBuiltinType of BuiltinType
                    | CUniverse 
                    | CPiType of CExpr * EVar option * CExpr  * plicity
                    | CSigmaType of CExpr * EVar option * CExpr 
                    | CBlock of CDeclaration list
    

    and cconstructorinfo = CConsInfoTypeConstructor   of int ref (* number of constructors of this type *)
                        | CConsInfoElementConstructor of (
                            (* StructureName.t absolute structure name of the type constructor *)
                                                        int  (* unique identifier of the current constructor, starting with 1 *)
                                                        )

(* all types are fully normalized *)
    and CDeclaration = 
                        (* Do not need type macro becuase all types for later stages have been expanded 
                        CHANGE: for imports/lsp, still need type macro*)
                        (* CTypeMacro of StructureName.t * CExpr  *)
                        (* Do not need type info as terms have been annotated *)
                        (* CTermTypeJudgment of UTF8String.t * CType *)
                        (* Fold into Term Definition *)
                       (*  CTermMacro of UTF8String.t * CExpr *)
                        CTermDefinition of UTF8String.t * CExpr * CExpr  
                       | CDirectExpr of int * CExpr * CExpr
                       | CConstructorDecl of UTF8String.t * CExpr * cconstructorinfo
                       (* Pure Declaration will be things that have not yet defined *)
                       | CPureDeclaration of UTF8String.t * CExpr  (* type only, definition to be provided later *)
                       | CImport of (StructureName.t  * FileResourceURI.t)
                       (* | CStructure of bool * UTF8String.t * CDeclaration list *)
                       (* Do not need open : Require all references to open use fully qualified name  *)
                       (* NEED OPEN IN MODULES *)
                       | COpenStructure of (StructureName.t * CDeclaration list) (* a list of declarations to be imported *)
    and CTypeAnn = CTypeAnn of CExpr
                 | CTypeAnnNotAvailable


    type CType = CExpr
    (* stores the source level information that directly correponds to opCompString, that can 
    be used to resconstruct the expression *)
    type sourceOpInfo = Operators.operator (* should be the operator except rapp *)
    (* RExpr for raw expr *)
    datatype RExpr = RVar of StructureName.t
                    | RPairOfQuotes of MixedStr.quoteinfo
                    | RUnitExpr of UTF8String.t
                    | RTuple of RExpr list * (sourceOpInfo list) (* n-1 op for n tuple *)
                    | RLazyTuple of RExpr list * (sourceOpInfo list) (* n-1 op for n tuple *)
                    | RProj of RExpr * (int * UTF8String.t ) * sourceOpInfo (* the index for rproj should start with 1, for cproj is current zero *)
                    | RLazyProj of RExpr * Label * sourceOpInfo
                    | RInj of Label * RExpr * sourceOpInfo
                    | RIfThenElse of RExpr * RExpr * RExpr * sourceOpInfo
                    | RCase of RExpr * ( RExpr (* pattern *) * RExpr (* body *)) list * (sourceOpInfo  (* top case *)
                            * sourceOpInfo list  (* case separator *)
                            * sourceOpInfo list (* case clause *))
                    | RLam of EVar * RExpr * plicity * sourceOpInfo
                    | RLamWithType of RExpr * EVar * RExpr * sourceOpInfo
                    | RApp of RExpr * RExpr * plicity * sourceOpInfo (* if op is not app, then custom operators *)
                    | RTypeAnnotate of RExpr (*type*) * RExpr * sourceOpInfo (* if op is not app, then custom operators *)
                    (* | RTAbs of TVar * RExpr * sourceOpInfo *)
                    | RTApp of RExpr * RExpr * (sourceOpInfo* UTF8String.t) (* string represents the type information itself *)
                    | RPack of RExpr * RExpr * (UTF8String.t * sourceOpInfo)
                    | ROpen of RExpr * (TVar * EVar * RExpr) * sourceOpInfo
                    | RFold of RExpr * sourceOpInfo
                    | RUnfold of RExpr * sourceOpInfo
                    | RFix of EVar * RExpr * sourceOpInfo
                    | RStringLiteral of UTF8String.t  * MixedStr.quoteinfo
                    | RIntConstant of int * UTF8String.t
                    | RRealConstant of (int * int * int ) * UTF8String.t
                    | RBoolConstant of bool * UTF8String.t
                    | RLetIn of RDeclaration list * (sourceOpInfo * MixedStr.quoteinfo)
                    | RLetInSingle of UTF8String.t * RExpr * RExpr * sourceOpInfo
                    | RFfiCCall of RExpr * RExpr * sourceOpInfo 
                    | RBuiltinFunc of BuiltinFunc * UTF8String.t (* source info *)
                    | RSeqComp of RExpr * RExpr * sourceOpInfo
                    (* types *)
                    | RUniverse of UTF8String.t (* a universe is the type of types, (TODO) stratified by level *)
                    | RPiType of RExpr option * EVar option * RExpr * plicity * sourceOpInfo
                    | RSigmaType of RExpr * EVar option * RExpr * sourceOpInfo
                    | RUnitType of UTF8String.t (* source info *)
                    | RProd of (RExpr) list * sourceOpInfo list (* n-1 source op info *)
                    | RLazyProd of (Label * RExpr * sourceOpInfo) list * sourceOpInfo list (* n-1 source op info *)
                    | RSum of (Label * RExpr * sourceOpInfo) list * sourceOpInfo list (* n-1 source op info *)
                    | RNullType of UTF8String.t (* source info *)
                    (* | RFunc of RExpr * RExpr * sourceOpInfo *)
                    | RTypeInst of RExpr * RExpr * sourceOpInfo
                    | RForall of TVar * RExpr * sourceOpInfo
                    | RExists of TVar * RExpr * sourceOpInfo 
                    | RRho of TVar * RExpr * sourceOpInfo 
                    | RBuiltinType of BuiltinType * UTF8String.t
                    | RBlock of RDeclaration list * MixedStr.quoteinfo


    and RDeclaration = 
                         (* RTypeMacro of UTF8String.t * RExpr *)
                       (* | *)
                        RTermTypeJudgment of UTF8String.t * RExpr
                       | RConstructorDecl of UTF8String.t * RExpr
                       (* | RTermMacro of UTF8String.t * RExpr *)
                       | RTermDefinition of UTF8String.t * RExpr
                       | RDirectExpr of RExpr
                       (* | RStructure of bool * UTF8String.t * RDeclaration list *)
                       (*  public visible * name * signature *)
                       | ROpenStructure of StructureName.t
                       | RReExportStructure of StructureName.t
                       | RImportStructure of (StructureName.t (* name *) * 
                                              FileResourceURI.t  (* file location *)
                                              )
    type RType = RExpr

    type CSignature = CDeclaration list
    type RSignature = RDeclaration list


(* these exist here for pretty printing *)
(* g for generic *)
 (* the term type J may optionally contain the definition *)
 datatype judgmentType = JTConstructor of cconstructorinfo 
                        | JTLocalBinder 
                        | JTLocalBinderWithDef of StructureName.t (* a pattern match binder with a resolved defintion, which is guaranteed to be a metavariable name by the definition *)
                        | JTDefinition of CExpr 
                        | JTPending (* declaration pending definition *)
                        | JTMetaVarPendingResolve of UTF8String.t (* the error reporting string when it cannot be resolved *)
                        | JTMetaVarResolved of CExpr
 datatype 'a gmapping = TermTypeJ of StructureName.t * CType  * judgmentType * 'a
                    (* | TermDefJ of StructureName.t * CType * unit *)
datatype 'a gcontext = Context of StructureName.t * bool * 
    ('a gmapping) list
    type mapping = ((StructureName.t * judgmentType) option ) gmapping (* original name and reference (for use with open) *)
    type context = ((StructureName.t * judgmentType) option ) gcontext (* original name and reference(for use with open) *)


    type constraints = (CExpr * CExpr * CType) list
end