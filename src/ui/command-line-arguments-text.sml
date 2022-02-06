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
        ArgShowAbout]

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
    

    fun getDescription(x : cmdlineargument) : string =
        case x of 
          ArgVerbose => "显示调试信息(verbose for debugging (unused))"
        | ArgSuperVerbose => "显示更多调试信息(super verbose for debugging (unused))"
        | ArgCompileOnly => "制作可执行文件但不执行(make the executable, but do not execute)"
        | ArgOptimize => "进行优化(optimize the executable)"
        | ArgProfile => "进行速度监测(profile (beta))"
        | ArgUseLocalLib => "使用当前文件夹下的运行库(use local libriries)"
        | ArgOutputFp => "设置输出路径(write the compiled executable to argument PATH)"
        | ArgGenDocs => "生成文档(generate the documentation)"
        | ArgShowHelp => "显示本帮助信息(show this help)"
        | ArgShowVersion => "显示版本(show version)"
        | ArgShowAbout => "显示关于信息(show about)"

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
