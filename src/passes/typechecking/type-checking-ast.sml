structure TypeCheckingAST = struct

    type EVar = UTF8String.t
    type TVar = UTF8String.t

    type Label = UTF8String.t

    datatype Type = TypeVar of TVar
                    | UnitType
                    | Prod of (Label * Type) list
                    | NullType
                    | Sum of (Label * Type) list
                    | Func of Type * Type
                    | Forall of TVar * Type
                    | Exists of TVar * Type
                    | Rho of TVar * Type

    datatype Expr = ExprVar of EVar
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


    datatype Declaration = 
                        TypeMacro of UTF8String.t * Type
                       | TermTypeJudgment of UTF8String.t * Type
                       | TermMacro of UTF8String.t * Expr
                       | TermDefinition of UTF8String.t * Expr
                       | DirectExpr of Expr

    type Signature = Declaration list
end