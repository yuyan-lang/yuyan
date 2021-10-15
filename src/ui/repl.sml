
structure InteractiveRepl =
struct
    open RawAST
    structure MixFixP = MixFixParser( struct val allOps = BuiltinOperators.allOps end)

    fun replHelper (input : RawAST ) : string =
    let 
        val trees = MixFixP.parseMixfixExpression input
    in 
        String.concatWith "\n" (map PrettyPrint.show_opast trees)
    end


end