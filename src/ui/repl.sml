
structure InteractiveRepl =
struct
    open RawAST
    structure MixFixP = MixFixParser( struct val allOps = BuiltinOperators.allOps end)

    fun addUI (s : string) = "豫☯ " ^ s ^ "\n"

    fun replHelper (input : RawAST ) : string =
    let 
        val trees = MixFixP.parseMixfixExpression input
    in
        ( addUI(String.concatWith "\n" (map PrettyPrint.show_opast trees)))
    end


end