structure ReplOptions =
struct
    type reploptions = 
    {
    verbose : int,
    exitOnFailure: bool,
    compileOnly: bool,
    optimize:bool, 
    genDocs: bool,
    showHelp: bool,
    showVersion: bool,
    showAbout: bool,
    outputFilePath : string option,
    inputFiles : string list,
    enableProfiling : bool
    }

    type t = reploptions

    val defaultOptions : reploptions = {
        verbose=0,
        exitOnFailure=true, 
        compileOnly=false, 
        optimize=false, 
        genDocs=false, 
        showHelp=false,
        showVersion=false,
        showAbout=false,
        outputFilePath=NONE,
        inputFiles=[],
        enableProfiling=false
        }
    fun getVerbose(recd : reploptions) = #verbose recd
    fun setVerbose(recd : reploptions) (newValue : int) = {verbose=newValue,exitOnFailure= #exitOnFailure recd,compileOnly= #compileOnly recd,optimize= #optimize recd,genDocs= #genDocs recd,showHelp= #showHelp recd,showVersion= #showVersion recd,showAbout= #showAbout recd,outputFilePath= #outputFilePath recd,inputFiles= #inputFiles recd,enableProfiling= #enableProfiling recd}

    fun getExitOnFailure(recd : reploptions) = #exitOnFailure recd
    fun setExitOnFailure(recd : reploptions) (newValue : bool) = {verbose= #verbose recd,exitOnFailure=newValue,compileOnly= #compileOnly recd,optimize= #optimize recd,genDocs= #genDocs recd,showHelp= #showHelp recd,showVersion= #showVersion recd,showAbout= #showAbout recd,outputFilePath= #outputFilePath recd,inputFiles= #inputFiles recd,enableProfiling= #enableProfiling recd}

    fun getCompileOnly(recd : reploptions) = #compileOnly recd
    fun setCompileOnly(recd : reploptions) (newValue : bool) = {verbose= #verbose recd,exitOnFailure= #exitOnFailure recd,compileOnly=newValue,optimize= #optimize recd,genDocs= #genDocs recd,showHelp= #showHelp recd,showVersion= #showVersion recd,showAbout= #showAbout recd,outputFilePath= #outputFilePath recd,inputFiles= #inputFiles recd,enableProfiling= #enableProfiling recd}

    fun getOptimize(recd : reploptions) = #optimize recd
    fun setOptimize(recd : reploptions) (newValue : bool) = {verbose= #verbose recd,exitOnFailure= #exitOnFailure recd,compileOnly= #compileOnly recd,optimize=newValue,genDocs= #genDocs recd,showHelp= #showHelp recd,showVersion= #showVersion recd,showAbout= #showAbout recd,outputFilePath= #outputFilePath recd,inputFiles= #inputFiles recd,enableProfiling= #enableProfiling recd}

    fun getGenDocs(recd : reploptions) = #genDocs recd
    fun setGenDocs(recd : reploptions) (newValue : bool) = {verbose= #verbose recd,exitOnFailure= #exitOnFailure recd,compileOnly= #compileOnly recd,optimize= #optimize recd,genDocs=newValue,showHelp= #showHelp recd,showVersion= #showVersion recd,showAbout= #showAbout recd,outputFilePath= #outputFilePath recd,inputFiles= #inputFiles recd,enableProfiling= #enableProfiling recd}

    fun getShowHelp(recd : reploptions) = #showHelp recd
    fun setShowHelp(recd : reploptions) (newValue : bool) = {verbose= #verbose recd,exitOnFailure= #exitOnFailure recd,compileOnly= #compileOnly recd,optimize= #optimize recd,genDocs= #genDocs recd,showHelp=newValue,showVersion= #showVersion recd,showAbout= #showAbout recd,outputFilePath= #outputFilePath recd,inputFiles= #inputFiles recd,enableProfiling= #enableProfiling recd}

    fun getShowVersion(recd : reploptions) = #showVersion recd
    fun setShowVersion(recd : reploptions) (newValue : bool) = {verbose= #verbose recd,exitOnFailure= #exitOnFailure recd,compileOnly= #compileOnly recd,optimize= #optimize recd,genDocs= #genDocs recd,showHelp= #showHelp recd,showVersion=newValue,showAbout= #showAbout recd,outputFilePath= #outputFilePath recd,inputFiles= #inputFiles recd,enableProfiling= #enableProfiling recd}

    fun getShowAbout(recd : reploptions) = #showAbout recd
    fun setShowAbout(recd : reploptions) (newValue : bool) = {verbose= #verbose recd,exitOnFailure= #exitOnFailure recd,compileOnly= #compileOnly recd,optimize= #optimize recd,genDocs= #genDocs recd,showHelp= #showHelp recd,showVersion= #showVersion recd,showAbout=newValue,outputFilePath= #outputFilePath recd,inputFiles= #inputFiles recd,enableProfiling= #enableProfiling recd}

    fun getOutputFilePath(recd : reploptions) = #outputFilePath recd
    fun setOutputFilePath(recd : reploptions) (newValue : string option) = {verbose= #verbose recd,exitOnFailure= #exitOnFailure recd,compileOnly= #compileOnly recd,optimize= #optimize recd,genDocs= #genDocs recd,showHelp= #showHelp recd,showVersion= #showVersion recd,showAbout= #showAbout recd,outputFilePath=newValue,inputFiles= #inputFiles recd,enableProfiling= #enableProfiling recd}

    fun getInputFiles(recd : reploptions) = #inputFiles recd
    fun setInputFiles(recd : reploptions) (newValue : string list) = {verbose= #verbose recd,exitOnFailure= #exitOnFailure recd,compileOnly= #compileOnly recd,optimize= #optimize recd,genDocs= #genDocs recd,showHelp= #showHelp recd,showVersion= #showVersion recd,showAbout= #showAbout recd,outputFilePath= #outputFilePath recd,inputFiles=newValue,enableProfiling= #enableProfiling recd}

    fun getEnableProfiling(recd : reploptions) = #enableProfiling recd
    fun setEnableProfiling(recd : reploptions) (newValue : bool) = {verbose= #verbose recd,exitOnFailure= #exitOnFailure recd,compileOnly= #compileOnly recd,optimize= #optimize recd,genDocs= #genDocs recd,showHelp= #showHelp recd,showVersion= #showVersion recd,showAbout= #showAbout recd,outputFilePath= #outputFilePath recd,inputFiles= #inputFiles recd,enableProfiling=newValue}


    


end
