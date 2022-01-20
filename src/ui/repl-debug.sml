structure ReplDebug =
struct
open CompilationStructure
    fun profileLSPTokens(CompilationFile f : compilationfile) (cm : compilationmanager): unit = 
    let val resJson = SyntaxHighlight.highlightFile (FileResourceURI.make (#fp f)) cm
    in LanguageServerMode.sendJSON(resJson)
    end

end
