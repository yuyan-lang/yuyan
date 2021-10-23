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




    datatype Expr = ExprVar of StructureName.t
                    | UnitExpr
                    | Tuple of Expr list
                    | Proj of Expr * Label
                    | Inj of Label * Expr
                    | Case of Expr * (Label * EVar * Expr) list
                    | Lam of EVar * Expr
                    | LamWithType of Type * EVar * Expr
                    | App of Expr * Expr
                    | TAbs of TVar * Expr
                    | TApp of Expr * Type
                    | Pack of Type * Expr
                    | Open of Expr * (TVar * EVar * Expr)
                    | Fold of Expr
                    | Unfold of Expr
                    | Fix of EVar * Expr
                    | StringLiteral of UTF8String.t
                    | LetIn of Declaration list * Expr


    and Declaration = 
                        TypeMacro of UTF8String.t * Type
                       | TermTypeJudgment of UTF8String.t * Type
                       | TermMacro of UTF8String.t * Expr
                       | TermDefinition of UTF8String.t * Expr
                       | DirectExpr of Expr
                       | Structure of bool * UTF8String.t * Declaration list
                       (*  public visible * name * signature *)
                       | OpenStructure of StructureName.t

    type Signature = Declaration list


end