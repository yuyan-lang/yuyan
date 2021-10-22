

structure MixFixParser  
  =
struct
 

    open Operators


    exception NoPossibleParse of MixedStr.t
    exception AmbiguousParse

    structure Parser = PrecedenceParser(structure Options = struct 
        val enableBracketedExpression = true
        end)

    fun parseMixfixExpression (allOps :Operators.allOperators) (exp : MixedStr.t) : OpAST.t = 
            case Parser.parseExpWithOption allOps true exp of
                                [] => raise NoPossibleParse exp
                                | [(parseopast, _)] => ElaboratePrecedence.elaborate parseopast
                                | _ => raise AmbiguousParse 

end