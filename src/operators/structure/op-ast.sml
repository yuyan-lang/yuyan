structure OpAST =
struct
open Operators

   datatype OpAST = OpAST of (operator * OpAST list )
                    | UnknownOpName of UTF8String.t
                    | NewOpName of UTF8String.t
                    | OpUnparsedExpr of MixedStr.t 
                    | OpUnparsedDecl of MixedStr.t list
                    | OpStrLiteral of (UTF8String.t * MixedStr.quoteinfo)
    type t= OpAST
end