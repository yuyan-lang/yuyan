
structure InteractiveRepl =
struct
    open RawAST
    structure MixFixP = MixFixParser( struct val allOps = BuiltinOperators.allOps end)

    fun addUI (s : string) = "豫☯ " ^ s ^ "\n"

    fun process (input : RawAST list ) : string =
    let 
        val trees = MixFixP.parseMixfixExpression input
    in
        ( addUI("Has "^ Int.toString(List.length trees) ^ " trees: \n" ^
            String.concatWith "\n" (map PrettyPrint.show_opast trees)))
    end

    fun replHelper (input : RawAST list ) : string =
        let 
        val startTime = Time.now()
        val res = process input
        val endTime = Time.now()
        val duration : Time.time = Time.-(endTime,startTime)
        in 
            (res ^ "\n" ^ "Took " ^ (LargeInt.toString(Time.toMilliseconds(duration))) ^ "ms to complete\n")
        end


end