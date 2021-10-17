
structure InteractiveRepl =
struct
    open RawAST

    fun addUIDynamic (s : string) = "豫☯ " ^ s ^ "\n"
    fun addUIStatic (s : string) = "豫䷏ " ^ s ^ "\n"

    fun process (input : string ) : string =
    let 
        val res = TypeCheckAndEval.typeCheckAndEval input
    in
        print (addUIDynamic (""));
        ""
    end

    fun replHelper (input : string ) : string =
        let 
        val startTime = Time.now()
        val res = process input
        val endTime = Time.now()
        val duration : Time.time = Time.-(endTime,startTime)
        in 
            (res ^ "\n" ^ "Took " ^ (LargeInt.toString(Time.toMilliseconds(duration))) ^ "ms to complete\n")
        end


end