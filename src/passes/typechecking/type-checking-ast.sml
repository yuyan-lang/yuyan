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
                    | CTuple of (Label * Type * CExpr) list 
                    | CProj of (Label * Type) list * CExpr * Label
                    | CInj of Label * CExpr * (Label * Type) list
                    | CCase of ((Label * Type) list * CExpr) * (Label * EVar * CExpr) list * Type
                    | CLam of (Type * EVar) * (Type * CExpr)
                    | CLamWithType of Type * EVar * (Type * CExpr)
                    | CApp of (Type * CExpr) * (Type * CExpr) (* Capp (return type) (arg type) *)
                    | CTAbs of TVar * (Type * CExpr)
                    | CTApp of (Type * CExpr) * Type
                    | CPack of Type * CExpr
                    | COpen of CExpr * (TVar * EVar * CExpr)
                    | CFold of CExpr
                    | CUnfold of CExpr
                    | CFix of EVar * CExpr
                    | CStringLiteral of UTF8String.t
                    | CLetIn of CDeclaration list * CExpr

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