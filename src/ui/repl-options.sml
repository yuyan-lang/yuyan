structure ReplOptions =
struct
    (* COPY reploptions = {...} and RUN: 
          pbpaste | python3 smlutil/gen-record-getter-setter.py | pbcopy
      to create getters and setters
    *)
    type reploptions = 
    {
    verbose : int,
    compileOnly: bool,
    optimize:bool, 
    genDocs: bool,
    showHelp: bool,
    showVersion: bool,
    showAbout: bool,
    outputFilePath : string option,
    inputFiles : string list,
    enableProfiling : bool,
    uselocallib : bool,
    typeCheckOnly : bool,
    clangOptions : string option
    }

    type t = reploptions

    val defaultOptions : reploptions = {
        verbose=0,
        compileOnly=false, 
        optimize=false, 
        genDocs=false, 
        showHelp=false,
        showVersion=false,
        showAbout=false,
        outputFilePath=NONE,
        inputFiles=[],
        enableProfiling=false,
        uselocallib=false,
        typeCheckOnly=false,
        clangOptions=NONE
        }

    fun getVerbose(recd : reploptions) = #verbose recd
    fun setVerbose(recd : reploptions) (newValue : int) = {verbose=newValue,compileOnly= #compileOnly recd,optimize= #optimize recd,genDocs= #genDocs recd,showHelp= #showHelp recd,showVersion= #showVersion recd,showAbout= #showAbout recd,outputFilePath= #outputFilePath recd,inputFiles= #inputFiles recd,enableProfiling= #enableProfiling recd,uselocallib= #uselocallib recd,typeCheckOnly= #typeCheckOnly recd,clangOptions= #clangOptions recd}

    fun getCompileOnly(recd : reploptions) = #compileOnly recd
    fun setCompileOnly(recd : reploptions) (newValue : bool) = {verbose= #verbose recd,compileOnly=newValue,optimize= #optimize recd,genDocs= #genDocs recd,showHelp= #showHelp recd,showVersion= #showVersion recd,showAbout= #showAbout recd,outputFilePath= #outputFilePath recd,inputFiles= #inputFiles recd,enableProfiling= #enableProfiling recd,uselocallib= #uselocallib recd,typeCheckOnly= #typeCheckOnly recd,clangOptions= #clangOptions recd}

    fun getOptimize(recd : reploptions) = #optimize recd
    fun setOptimize(recd : reploptions) (newValue : bool) = {verbose= #verbose recd,compileOnly= #compileOnly recd,optimize=newValue,genDocs= #genDocs recd,showHelp= #showHelp recd,showVersion= #showVersion recd,showAbout= #showAbout recd,outputFilePath= #outputFilePath recd,inputFiles= #inputFiles recd,enableProfiling= #enableProfiling recd,uselocallib= #uselocallib recd,typeCheckOnly= #typeCheckOnly recd,clangOptions= #clangOptions recd}

    fun getGenDocs(recd : reploptions) = #genDocs recd
    fun setGenDocs(recd : reploptions) (newValue : bool) = {verbose= #verbose recd,compileOnly= #compileOnly recd,optimize= #optimize recd,genDocs=newValue,showHelp= #showHelp recd,showVersion= #showVersion recd,showAbout= #showAbout recd,outputFilePath= #outputFilePath recd,inputFiles= #inputFiles recd,enableProfiling= #enableProfiling recd,uselocallib= #uselocallib recd,typeCheckOnly= #typeCheckOnly recd,clangOptions= #clangOptions recd}

    fun getShowHelp(recd : reploptions) = #showHelp recd
    fun setShowHelp(recd : reploptions) (newValue : bool) = {verbose= #verbose recd,compileOnly= #compileOnly recd,optimize= #optimize recd,genDocs= #genDocs recd,showHelp=newValue,showVersion= #showVersion recd,showAbout= #showAbout recd,outputFilePath= #outputFilePath recd,inputFiles= #inputFiles recd,enableProfiling= #enableProfiling recd,uselocallib= #uselocallib recd,typeCheckOnly= #typeCheckOnly recd,clangOptions= #clangOptions recd}

    fun getShowVersion(recd : reploptions) = #showVersion recd
    fun setShowVersion(recd : reploptions) (newValue : bool) = {verbose= #verbose recd,compileOnly= #compileOnly recd,optimize= #optimize recd,genDocs= #genDocs recd,showHelp= #showHelp recd,showVersion=newValue,showAbout= #showAbout recd,outputFilePath= #outputFilePath recd,inputFiles= #inputFiles recd,enableProfiling= #enableProfiling recd,uselocallib= #uselocallib recd,typeCheckOnly= #typeCheckOnly recd,clangOptions= #clangOptions recd}

    fun getShowAbout(recd : reploptions) = #showAbout recd
    fun setShowAbout(recd : reploptions) (newValue : bool) = {verbose= #verbose recd,compileOnly= #compileOnly recd,optimize= #optimize recd,genDocs= #genDocs recd,showHelp= #showHelp recd,showVersion= #showVersion recd,showAbout=newValue,outputFilePath= #outputFilePath recd,inputFiles= #inputFiles recd,enableProfiling= #enableProfiling recd,uselocallib= #uselocallib recd,typeCheckOnly= #typeCheckOnly recd,clangOptions= #clangOptions recd}

    fun getOutputFilePath(recd : reploptions) = #outputFilePath recd
    fun setOutputFilePath(recd : reploptions) (newValue : string option) = {verbose= #verbose recd,compileOnly= #compileOnly recd,optimize= #optimize recd,genDocs= #genDocs recd,showHelp= #showHelp recd,showVersion= #showVersion recd,showAbout= #showAbout recd,outputFilePath=newValue,inputFiles= #inputFiles recd,enableProfiling= #enableProfiling recd,uselocallib= #uselocallib recd,typeCheckOnly= #typeCheckOnly recd,clangOptions= #clangOptions recd}

    fun getInputFiles(recd : reploptions) = #inputFiles recd
    fun setInputFiles(recd : reploptions) (newValue : string list) = {verbose= #verbose recd,compileOnly= #compileOnly recd,optimize= #optimize recd,genDocs= #genDocs recd,showHelp= #showHelp recd,showVersion= #showVersion recd,showAbout= #showAbout recd,outputFilePath= #outputFilePath recd,inputFiles=newValue,enableProfiling= #enableProfiling recd,uselocallib= #uselocallib recd,typeCheckOnly= #typeCheckOnly recd,clangOptions= #clangOptions recd}

    fun getEnableProfiling(recd : reploptions) = #enableProfiling recd
    fun setEnableProfiling(recd : reploptions) (newValue : bool) = {verbose= #verbose recd,compileOnly= #compileOnly recd,optimize= #optimize recd,genDocs= #genDocs recd,showHelp= #showHelp recd,showVersion= #showVersion recd,showAbout= #showAbout recd,outputFilePath= #outputFilePath recd,inputFiles= #inputFiles recd,enableProfiling=newValue,uselocallib= #uselocallib recd,typeCheckOnly= #typeCheckOnly recd,clangOptions= #clangOptions recd}

    fun getUselocallib(recd : reploptions) = #uselocallib recd
    fun setUselocallib(recd : reploptions) (newValue : bool) = {verbose= #verbose recd,compileOnly= #compileOnly recd,optimize= #optimize recd,genDocs= #genDocs recd,showHelp= #showHelp recd,showVersion= #showVersion recd,showAbout= #showAbout recd,outputFilePath= #outputFilePath recd,inputFiles= #inputFiles recd,enableProfiling= #enableProfiling recd,uselocallib=newValue,typeCheckOnly= #typeCheckOnly recd,clangOptions= #clangOptions recd}

    fun getTypeCheckOnly(recd : reploptions) = #typeCheckOnly recd
    fun setTypeCheckOnly(recd : reploptions) (newValue : bool) = {verbose= #verbose recd,compileOnly= #compileOnly recd,optimize= #optimize recd,genDocs= #genDocs recd,showHelp= #showHelp recd,showVersion= #showVersion recd,showAbout= #showAbout recd,outputFilePath= #outputFilePath recd,inputFiles= #inputFiles recd,enableProfiling= #enableProfiling recd,uselocallib= #uselocallib recd,typeCheckOnly=newValue,clangOptions= #clangOptions recd}

    fun getClangOptions(recd : reploptions) = #clangOptions recd
    fun setClangOptions(recd : reploptions) (newValue : string option) = {verbose= #verbose recd,compileOnly= #compileOnly recd,optimize= #optimize recd,genDocs= #genDocs recd,showHelp= #showHelp recd,showVersion= #showVersion recd,showAbout= #showAbout recd,outputFilePath= #outputFilePath recd,inputFiles= #inputFiles recd,enableProfiling= #enableProfiling recd,uselocallib= #uselocallib recd,typeCheckOnly= #typeCheckOnly recd,clangOptions=newValue}




end
