

functor MixFixParser (P : sig 
        val allOps :Operators.allOperators 
        end)  =
struct
 

    open RawAST
    open Operators

    structure Parser = PrecedenceParser(P)
    fun parseMixfixExpression (ops : allOperators) (exp : RawAST.RawAST) : OpAST list = 
        case exp of
            RawList l =>  (case Parser.parseExp() l of
                                [] => raise Fail "noParse 20"
                                | l => map (fn (x, _) => ElaboratePrecedence.elaborate x)
                                     (List.filter (fn (x, s) => List.length s = 0) l))
            | RawID _ => raise Fail "Must be a list 22"

end