structure OpAST =
struct
open Operators

   datatype OpAST = OpAST of (operator * OpAST list )
                    | UnknownOpName of UTF8String.t
                    | NewOpName of UTF8String.t
                    | OpUnparsedExpr of MixedStr.t 
                    | OpUnparsedDecl of (MixedStr.t * MixedStr.endinginfo) list * MixedStr.quoteinfo
                    | OpStrLiteral of (UTF8String.t * MixedStr.quoteinfo)
    type t= OpAST


    fun reconstructOriginalFromOpAST(opast : OpAST) : UTF8String.t = 
        case opast of
            OpAST(oper, args) => reconstructWithArgs oper (map reconstructOriginalFromOpAST args)
            | UnknownOpName s =>  s
            | NewOpName s =>  s
            | OpUnparsedExpr m => MixedStr.toUTF8String m
            | OpUnparsedDecl ml => MixedStr.toUTF8StringChar (MixedStr.UnparsedDeclaration ml)
            | OpStrLiteral (s, (ql, qr)) => ql :: s @[qr]
end