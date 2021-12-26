
structure CompilationFileOps =
struct

open CompilationStructure

    fun fromSome (SOME v) = v

    fun updateFileContent ( newContent : string) (file : compilationfile) : compilationfile = 
        CompilationFile(fp, (UTF8String.fromStringAndFile content fp, Time.now()), NONE, NONE, NONE, NONE)   

    fun initWithFilePathAndContent(fp : string) ( content : string) = 
        updateFileContent content (CompilationFile(fp, NONE, NONE, NONE, NONE, NONE))

    fun initWithFilePath(fp : string) = 
        initWithFilePathAndContent fp (TextIO.inputAll (TextIO.openIn filepath))

    fun ensureContent (file : compilationfile) : compilationfile = 
        let val (fp, content, typecheckingast, depends, typecheckedast, cpsconv, llinfo) = file
        in case content of 
            SOME _ => file 
            | NONE => updateFileContent (TextIO.inputAll (TextIO.openIn filepath)) file
        end
    fun getContent (file : compilationfile) : UTF8String.t = 
        let val (fp, content, typecheckingast, depends, typecheckedast, cpsconv, llinfo) = file
        in case content of 
            SOME (s, t) => s
            | NONE => raise Fail "cfops 27 : getContent called on empty content"
        end
    fun getFp (file : compilationfile) : string = 
        let val (fp, content, typecheckingast, depends, typecheckedast, cpsconv, llinfo) = file
        in fp
        end

    fun constructTypeCheckingAST 
        (file : compilationfile) : compilationfile = 
        let 
        open CompilationTokens
        val file = ensureContent file
        val content = getContent file
        val stmtAST = MixedStr.makeDecl content
        val tokensInfo : token list ref = ref []
        val typeCheckingAST = ExpressionConstructionPass.configureAndConstructTypeCheckingASTTopLevel
        (updateUsefulTokensFromOpAST tokensInfo)
        (updateUsefulTokensFromDeclarationParser tokensInfo)
        (fn x => ())
        (stmtAST)
         val sortedTokens = ListMergeSort.sort 
                (* true if gt *)
                (fn (Token(SourceRange.StartEnd(_, l1, c1, _, _),_,_), Token(SourceRange.StartEnd(_, l2, c2, _, _),_, _))
                => if l1 > l2 then true else if l1 < l2 then false else if c1 > c2 then true else false)
                (!tokensInfo)
        in 
            CompilationFile {
                fp=(getFp file)
            , content=SOME(#content file)
            , typeCheckingInfo=SOME((typeCheckingAST, sortedTokens))
            , dependencyInfo=NONE
            , typeCheckedInfo=NONE
            , cpsInfo=NONE
            , llvmInfo=NONE
            }
        end
    

    fun processFileUpTo  (upToLevel: uptolevel)
        (file : compilationfile) : compilationfile = 
        let 
        open CompilationTokens
        let val (fp, content, typecheckingast, depends, typecheckedast, cpsconv, llvminfo) = file
        in if upToLevel = UpToLevelContent
           then
        val file = ensureContent file
        val content = getContent file
        val stmtAST = MixedStr.makeDecl content
        val tokensInfo : token list ref = ref []
        val typeCheckingAST = ExpressionConstructionPass.configureAndConstructTypeCheckingASTTopLevel
        (updateUsefulTokensFromOpAST tokensInfo)
        (updateUsefulTokensFromDeclarationParser tokensInfo)
        (fn x => ())
        (stmtAST)
         val sortedTokens = ListMergeSort.sort 
                (* true if gt *)
                (fn (Token(SourceRange.StartEnd(_, l1, c1, _, _),_,_), Token(SourceRange.StartEnd(_, l2, c2, _, _),_, _))
                => if l1 > l2 then true else if l1 < l2 then false else if c1 > c2 then true else false)
                (!tokensInfo)
        in 
            CompilationFile {
                fp=(getFp file)
            , content=SOME(#content file)
            , typeCheckingInfo=SOME((typeCheckingAST, sortedTokens))
            , dependencyInfo=NONE
            , typeCheckedInfo=NONE
            , cpsInfo=NONE
            , llvmInfo=NONE
            }
        end


    
end