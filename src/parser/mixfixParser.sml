

functor MixFixParser  ( structure Options :PARSER_OPTIONS)
  =
struct
 

    open RawAST
    open Operators


    exception NoPossibleParse of UTF8String.t
    exception AmbiguousParse

    structure Parser = PrecedenceParser(structure Options = Options)

    fun parseMixfixExpression (allOps :Operators.allOperators) (exp : UTF8String.t) : OpAST = 
            case Parser.parseExpWithOption allOps true exp of
                                [] => raise NoPossibleParse exp
                                | [(parseopast, _)] => ElaboratePrecedence.elaborate parseopast
                                | _ => raise AmbiguousParse 

end