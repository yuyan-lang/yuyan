
structure ArgumentParser = struct


    
    open ReplOptions
    open CommandLineArgumentsText

    exception ArgumentParseFailure of string

    fun processOption (s : cmdlineargument) (arg : string option) (current : reploptions) : reploptions = 
        case (s, arg) of
             (ArgVerbose, NONE) => (setVerbose current 1)
            | (ArgSuperVerbose, NONE) => (setVerbose current 2)
            | (ArgCompileOnly, NONE) => (setCompileOnly current true)
            | (ArgOptimize, NONE) => (setOptimize current true)
            | (ArgProfile, NONE) => (setEnableProfiling current true)
            | (ArgUseLocalLib, NONE) => (setUselocallib current true)
            | (ArgOutputFp, SOME fp) => (setOutputFilePath current (SOME fp))
            | (ArgGenDocs, NONE) => (setGenDocs current true)
            | (ArgShowHelp, NONE) => (setShowHelp current true)
            | (ArgShowVersion, NONE) => (setShowVersion current true)
            | (ArgShowAbout, NONE) => (setShowAbout current true)
            | (ArgTypeCheckOnly, NONE) => (setTypeCheckOnly current true)
            | _ => raise Fail "ap24"
    
            
    fun parseArguments (s : string list ) (current : reploptions) : reploptions = 
    let fun processCandidates(f : string) (ss : string list) (candidates : cmdlineargument list) = 
                        if length candidates > 1 
                        then raise Fail "ap42"
                        else if length candidates = 0
                            then raise ArgumentParseFailure ("未知的设置(unrecognized option)："^ f)
                            else 
                            (case acceptParameter (hd candidates) of
                                SOME _ => (case ss of 
                                    [] => raise ArgumentParseFailure ("缺少参数(option needs argument)：" ^ f)
                                    | (hss::tss) => parseArguments tss (processOption (hd candidates) (SOME hss) current)
                                    )
                                | NONE => parseArguments ss (processOption (hd candidates) NONE current)
                            )
    in
        case s of
            [] => current
            | (f :: ss) =>  if String.isPrefix "--" f
                            then let val candidates = List.filter (fn x => 
                                (case getLongOptionStyle x of
                                    SOME t => "--" ^ t = f
                                    | NONE => false
                                )) allArguments
                                in processCandidates f ss candidates 
                                end
                            else if String.isPrefix "-" f
                                then let val candidates = List.filter (fn x => 
                                    (case getShortOptionStyle x of
                                        SOME t => "-" ^ t = f
                                        | NONE => false
                                    )) allArguments
                                    in processCandidates f ss candidates 
                                    end
                                else parseArguments ss (setInputFiles current ((getInputFiles current)@[f]))
    end

                





    fun parseArgumentsTopLevel(sl : string list) : reploptions = 
        parseArguments  sl defaultOptions 
end