

functor MixFixParser  ( structure Options :
     sig
        val enableBracketedExpression : bool
    end
)  =
struct
 

    open RawAST
    open Operators


    exception NoPossibleParse
    exception AmbiguousParse

    structure Parser = PrecedenceParser(structure Options = Options)

    fun parseMixfixExpression (allOps :Operators.allOperators) (exp : UTF8String.t) : OpAST = 
            case Parser.parseExpWithOption allOps true exp of
                                [] => raise NoPossibleParse
                                | [(parseopast, _)] => ElaboratePrecedence.elaborate parseopast
                                | _ => raise AmbiguousParse

end