structure TypeCheckingAST = struct

    type EVar = UTF8String.t
    type TVar = UTF8String.t

    type Label = UTF8String.t

    datatype Type = TypeVar of TVar
                    | EagerProd of (Label * Type) list
                    | Sum of (Label * Type) list
                    | Func of TVar * Type
                    | Forall of TVar * Type
                    | Exists of TVar * Type
                    | Rho of TVar * Type

    datatype Expr = ExprVar of EVar
                    | EagerTuple of Expr list
                    | Proj of Label * Expr
                    | Inj of Label * Expr
                    | Case of (Label * EVar * Expr) list
                    | Lam of EVar * Expr
                    | App of Expr * Expr
                    | TAbs of TVar * Expr
                    | TApp of Expr * Type
                    | Pack of Type * Expr
                    | Open of Expr * (TVar * EVar * Expr)
                    | Fold of Expr
                    | Unfold of Expr


    datatype Signature = EmptySig
                       | TypeMacro of UTF8String.t * Type
                       | TypeJudgment of UTF8String.t * Type
                       | TermDefinition of UTF8String.t * Expr
end