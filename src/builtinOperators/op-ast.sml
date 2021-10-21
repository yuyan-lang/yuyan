structure OpAST =
struct
open Operators

   datatype OpAST = OpAST of (operator * OpAST list )
                    | UnknownOpName of UTF8String.t
                    | NewOpName of UTF8String.t
                    | OpUnparsedExpr of MixedStr.t (* not used *)
                    | OpUnparsedDecl of MixedStr.t list (* not used *)
    type t= OpAST
end