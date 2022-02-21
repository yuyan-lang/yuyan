structure OpAST =
struct
    open Operators
    (* because of the mixed match nature of 
    the introduction of new operators by signatures, 
    I reckon that single opast cannot represent the full strucutre 
    we've been seen in the files, therefore, 
    I decide to incorporate pJudgement list as 
    part of the definition of OpAST *)

    type sourceOpInfo = Operators.operator (* should be the operator except rapp *)
   datatype pJudgment = PEmptyDecl 
                       (* | PTypeMacro of UTF8String.t * OpAST * sourceOpInfo *)
                       | PTermTypeJudgment of UTF8String.t * OpAST * sourceOpInfo
                       | PConstructorDecl of UTF8String.t * OpAST * sourceOpInfo
                       (* | PTermMacro of UTF8String.t * OpAST * sourceOpInfo *)
                       | PTermDefinition of UTF8String.t * OpAST * sourceOpInfo
                       | POpDeclaration of UTF8String.t * Operators.associativity * int * 
                        (UTF8String.t (* assoc original text *)
                        * UTF8String.t (*pred original text *)
                        * sourceOpInfo)
                       | PDirectExpr of OpAST 
                       | PComment of MixedStr.t * sourceOpInfo
                       | PStructure of bool * UTF8String.t  * OpAST(* bool is true if public *) 
                            * sourceOpInfo
                       | POpenStructure of OpAST   * sourceOpInfo
                       | PReExportStructure of OpAST   * sourceOpInfo
                       | PImportStructure of OpAST * FileResourceURI.t * sourceOpInfo

    and OpAST = OpAST of (operator * OpAST list )
                    | UnknownOpName of UTF8String.t
                    | NewOpName of UTF8String.t
                    | OpUnparsedExpr of MixedStr.t * MixedStr.quoteinfo
                    | OpUnparsedDecl of (MixedStr.t * MixedStr.endinginfo) list * MixedStr.quoteinfo
                    | OpParsedQuotedExpr of OpAST * MixedStr.quoteinfo
                    | OpParsedDecl of (pJudgment * MixedStr.endinginfo) list * MixedStr.quoteinfo
                    | OpStrLiteral of (UTF8String.t * MixedStr.quoteinfo)
    type preprocessAST = (pJudgment * MixedStr.endinginfo) list
    type t= OpAST


end