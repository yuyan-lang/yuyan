structure OpAST =
struct
    open Operators
    (* because of the mixed match nature of 
    the introduction of new operators by signatures, 
    I reckon that single opast cannot represent the full strucutre 
    we've been seen in the files, therefore, 
    I decide to incorporate pJudgement list as 
    part of the definition of OpAST *)
   datatype pJudgment = PEmptyDecl
                       | PTypeMacro of UTF8String.t * OpAST
                       | PTermTypeJudgment of UTF8String.t * OpAST
                       | PTermMacro of UTF8String.t * OpAST
                       | PTermDefinition of UTF8String.t * OpAST
                       | POpDeclaration of UTF8String.t * Operators.associativity * int
                       | PDirectExpr of OpAST
                       | PComment of MixedStr.t
                       | PStructure of bool * UTF8String.t  * pJudgment list(* bool is true if public *)
                       | POpenStructure of OpAST (* bool is true if public *)
                       | PImportStructure of OpAST

    and OpAST = OpAST of (operator * OpAST list )
                    | UnknownOpName of UTF8String.t
                    | NewOpName of UTF8String.t
                    | OpUnparsedExpr of MixedStr.t 
                    | OpUnparsedDecl of (MixedStr.t * MixedStr.endinginfo) list * MixedStr.quoteinfo
                    | OpParsedDecl of pJudgment list
                    | OpStrLiteral of (UTF8String.t * MixedStr.quoteinfo)
    type preprocessAST = pJudgment list
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