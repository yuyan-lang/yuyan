structure TypeCheckingAST = struct

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
                    | Forall of TVar * Type
                    | Exists of TVar * Type
                    | Rho of TVar * Type
                    | BuiltinType of BuiltinType



    (* CExpr for checked expr *)
    datatype CExpr = CExprVar of StructureName.t
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
                    | CLetIn of CDeclaration list * CExpr * Type (* Type is the result of the declaring expression *)

    and CDeclaration = 
                        CTypeMacro of UTF8String.t * Type
                       | CTermTypeJudgment of UTF8String.t * Type
                       | CTermMacro of UTF8String.t * CExpr
                       | CTermDefinition of UTF8String.t * CExpr
                       | CDirectExpr of CExpr
                       | CStructure of bool * UTF8String.t * CDeclaration list
                       (*  public visible * name * signature *)
                       | COpenStructure of StructureName.t

    (* RExpr for raw expr *)
    datatype RExpr = RExprVar of StructureName.t
                    | RUnitExpr
                    | RTuple of RExpr list
                    | RProj of RExpr * Label
                    | RInj of Label * RExpr
                    | RCase of RExpr * (Label * EVar * RExpr) list
                    | RLam of EVar * RExpr
                    | RLamWithType of Type * EVar * RExpr
                    | RApp of RExpr * RExpr
                    | RTAbs of TVar * RExpr
                    | RTApp of RExpr * Type
                    | RPack of Type * RExpr
                    | ROpen of RExpr * (TVar * EVar * RExpr)
                    | RFold of RExpr
                    | RUnfold of RExpr
                    | RFix of EVar * RExpr
                    | RStringLiteral of UTF8String.t
                    | RLetIn of RDeclaration list * RExpr


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