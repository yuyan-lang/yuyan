
structure ArgumentParser = struct


    
    open ReplOptions

    fun parseArguments (s : string list ) (current : reploptions) : reploptions = 
        case s of
            [] => current
            | ("-v"::ss) => parseArguments ss (setVerbose current 1)
            | ("--superverbose"::ss) => parseArguments ss (setVerbose current 2)
            | ("--no-exit-on-failure"::ss) => parseArguments ss (setExitOnFailure current false)
            | ("-c"::ss) => parseArguments ss (setCompileOnly current true)
            | ("--compile-only"::ss) => parseArguments ss (setCompileOnly current true)
            | ("--optimize"::ss) => parseArguments ss (setOptimize current true)
            | ("--fast"::ss) => parseArguments ss (setOptimize current true)
            | ("-o"::fp::ss) => parseArguments ss (setOutputFilePath current (SOME fp))
            | ("--gen-docs"::ss) => parseArguments ss (setGenDocs current true)
            | ("-h" :: ss) => parseArguments ss (setShowHelp current true)
            | ("--help" :: ss) => parseArguments ss (setShowHelp current true)
            | ("--version" :: ss) => parseArguments ss (setShowVersion current true)
            | ("--about" :: ss) => parseArguments ss (setShowAbout current true)
            | (f :: ss) => parseArguments ss (setInputFiles current ((getInputFiles current)@[f]))
            
            



    fun parseArgumentsTopLevel(sl : string list) : reploptions = 
        parseArguments  sl defaultOptions 
end