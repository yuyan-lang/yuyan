structure TypeCheckingAST = struct
    open Operators

    type EVar = UTF8String.t
    type TVar = UTF8String.t

    type Label = UTF8String.t

    datatype BuiltinType = BIString
                         | BIInt
                         | BIReal
                         | BIBool

    datatype Type = TypeVar of StructureName.t
                    | UnitType
                    | Prod of (Label * Type) list
                    | NullType
                    | Sum of (Label * Type) list
                    | Func of Type * Type
                    | TypeInst of Type * Type
                    | Forall of TVar * Type
                    | Exists of TVar * Type
                    | Rho of TVar * Type
                    | BuiltinType of BuiltinType

    datatype visibility = Public | Private


    (* CExpr for checked expr *)
    datatype CExpr = CExprVar of StructureName.t (* required to be fully qualified name, if not local *)
                    | CUnitExpr
                    | CTuple of CExpr list * Type (* type is Prod *)
                    | CProj of CExpr * Label * Type (* type is Prod *)
                    | CInj of Label * CExpr  * Type (* type is  Sum *)
                    | CCase of (Type (*type is Sum *) * CExpr) * (Label * EVar * CExpr) list * Type (* type is result type *)
                    | CLam of  EVar * CExpr * Type (* type is Func *)
                    | CApp of  CExpr * CExpr * Type (* type is Func *)
                    | CTAbs of TVar * CExpr  * Type(* type is Forall *)
                    | CTApp of CExpr * Type (* instantiation type *) * Type(* type is Forall *)
                    | CPack of Type (* pack type *) * CExpr * Type(* type is Exists *)
                    | COpen of (Type (* type is Exists *) * CExpr) * (TVar * EVar * CExpr) * Type(* type is return type *)
                    | CFold of CExpr  * Type(* type is Rho *)
                    | CUnfold of CExpr  * Type (* type is Rho *)
                    | CFix of EVar * CExpr * Type (* type is the typ of the expression *)
                    | CStringLiteral of UTF8String.t 
                    | CIntConstant of int
                    | CRealConstant of real
                    | CLetIn of CDeclaration list * CExpr * Type (* Type is the result of the declaring expression *)
                    | CFfiCCall of UTF8String.t * StructureName.t list

    and CDeclaration = 
                        (* Do not need type macro becuase all types for later stages have been expanded *)
                        (* CTypeMacro of UTF8String.t * Type *) 
                        (* Do not need type info as terms have been annotated *)
                        (* CTermTypeJudgment of UTF8String.t * Type *)
                        (* Fold into Term Definition *)
                       (*  CTermMacro of UTF8String.t * CExpr *)
                        CTermDefinition of StructureName.t * CExpr
                       | CDirectExpr of CExpr
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
                    | RProj of RExpr * Label * sourceOpInfo
                    | RInj of Label * RExpr * sourceOpInfo
                    | RCase of RExpr * (Label * EVar * RExpr) list * (sourceOpInfo  (* top case *)
                            * sourceOpInfo list  (* case separator *)
                            * sourceOpInfo list (* case clause *))
                    | RLam of EVar * RExpr * sourceOpInfo
                    | RLamWithType of Type * EVar * RExpr * sourceOpInfo
                    | RApp of RExpr * RExpr * sourceOpInfo (* if op is not app, then custom operators *)
                    | RTAbs of TVar * RExpr * sourceOpInfo
                    | RTApp of RExpr * Type * (sourceOpInfo* UTF8String.t) (* string represents the type information itself *)
                    | RPack of Type * RExpr * (UTF8String.t * sourceOpInfo)
                    | ROpen of RExpr * (TVar * EVar * RExpr) * sourceOpInfo
                    | RFold of RExpr * sourceOpInfo
                    | RUnfold of RExpr * sourceOpInfo
                    | RFix of EVar * RExpr * sourceOpInfo
                    | RStringLiteral of UTF8String.t  * MixedStr.quoteinfo
                    | RIntConstant of int * UTF8String.t
                    | RRealConstant of real * UTF8String.t
                    | RLetIn of RDeclaration list * RExpr * sourceOpInfo
                    | RFfiCCall of RExpr * RExpr * sourceOpInfo
                    


    and RDeclaration = 
                         RTypeMacro of UTF8String.t * Type
                       | RTermTypeJudgment of UTF8String.t * Type
                       | RTermMacro of UTF8String.t * RExpr
                       | RTermDefinition of UTF8String.t * RExpr
                       | RDirectExpr of RExpr
                       | RStructure of bool * UTF8String.t * RDeclaration list
                       (*  public visible * name * signature *)
                       | ROpenStructure of StructureName.t

    type CSignature = CDeclaration list
    type RSignature = RDeclaration list


end