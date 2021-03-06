structure CommandLineArgumentsText =
struct

    datatype cmdlineargument = 
        ArgVerbose
        | ArgSuperVerbose
        | ArgCompileOnly
        | ArgOptimize
        | ArgProfile
        | ArgUseLocalLib
        | ArgOutputFp
        | ArgGenDocs
        | ArgShowHelp
        | ArgShowVersion
        | ArgShowAbout
        | ArgTypeCheckOnly

    val allArguments : cmdlineargument list = [ArgVerbose,
        ArgSuperVerbose,
        ArgCompileOnly,
        ArgOptimize,
        ArgProfile,
        ArgUseLocalLib,
        ArgOutputFp,
        ArgGenDocs,
        ArgShowHelp,
        ArgShowVersion,
        ArgShowAbout,
        ArgTypeCheckOnly]

    fun acceptParameter(x : cmdlineargument) : string option = 
    case x of 
        ArgOutputFp => SOME("PATH")
        | _ => NONE
    
    (* short argument is preceded by - *)
    fun getShortOptionStyle (x : cmdlineargument): string option = 
    case x of 
          ArgVerbose => SOME("v")
        | ArgSuperVerbose => NONE
        | ArgCompileOnly => SOME("c")
        | ArgOptimize => SOME("O3")
        | ArgProfile => NONE
        | ArgUseLocalLib => NONE
        | ArgOutputFp => SOME("o")
        | ArgGenDocs => NONE
        | ArgShowHelp => SOME("h")
        | ArgShowVersion => NONE
        | ArgShowAbout => NONE
        | ArgTypeCheckOnly => NONE

    (* long argument is preceded by -- *)
    fun getLongOptionStyle (x : cmdlineargument): string option = 
    case x of 
          ArgVerbose => SOME("verbose")
        | ArgSuperVerbose => SOME("super-verbose")
        | ArgCompileOnly => SOME("compile-only")
        | ArgOptimize => SOME("optimize")
        | ArgProfile => SOME ("profile")
        | ArgUseLocalLib => SOME ("use-local-lib")
        | ArgOutputFp => SOME("output")
        | ArgGenDocs => SOME("gen-docs")
        | ArgShowHelp => SOME("help")
        | ArgShowVersion => SOME("version")
        | ArgShowAbout => SOME("about")
        | ArgTypeCheckOnly => SOME("type-check-only")
    

    fun getDescription(x : cmdlineargument) : string =
        case x of 
          ArgVerbose => "??????????????????(verbose for debugging (unused))"
        | ArgSuperVerbose => "????????????????????????(super verbose for debugging (unused))"
        | ArgCompileOnly => "?????????????????????????????????(make the executable, but do not execute)"
        | ArgOptimize => "????????????(optimize the executable)"
        | ArgProfile => "??????????????????(profile (beta))"
        | ArgUseLocalLib => "????????????????????????????????????(use local libriries)"
        | ArgOutputFp => "??????????????????(write the compiled executable to argument PATH)"
        | ArgGenDocs => "????????????(generate the documentation)"
        | ArgShowHelp => "?????????????????????(show this help)"
        | ArgShowVersion => "????????????(show version)"
        | ArgShowAbout => "??????????????????(show about)"
        | ArgTypeCheckOnly => "?????????????????????(type check only)"

    fun getOptionDescription() : string = 
        let val shortOptions = List.map getShortOptionStyle allArguments
            val longOptions = List.map getLongOptionStyle allArguments
            val optionDescription = ListPair.map (fn (s, l) => case (s, l) of
                (SOME(x), NONE) => "-" ^ x
                | (NONE, SOME(x)) => "--" ^ x
                | (SOME(x), SOME(y)) => "-" ^ x ^ ", --" ^ y
                | _ => raise Fail "clat86"
                ) (shortOptions, longOptions)
            val withParams = ListPair.map(fn (s, a) => case acceptParameter a of 
                SOME name => s ^ " " ^ name
                | NONE => s) (optionDescription, allArguments)
            val maxLength = foldl (fn (x, acc) => Int.max(String.size x, acc)) 0 withParams (* assumes options do not have chinese characters *)
            fun pad n = String.implode(List.tabulate(n, fn _ => #" "))
        in 
            String.concatWith "\n" (ListPair.map (fn (wp, a) => 
                pad 4 ^ wp ^ pad (maxLength - (String.size wp)) ^ pad 4 ^  getDescription a 
            ) (withParams, allArguments))
        end
    val optionDescription : string = getOptionDescription()
        

end
