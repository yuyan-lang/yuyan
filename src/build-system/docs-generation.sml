structure DocsGeneration =
struct

    open CompilationStructure
    open FileResourceURI
    open CompilationManager
    open RichTextDocument
    open CompilationFileOps
    open TypeCheckingAST

    val closingStructureSegment = [RichTextSegment(Red, Regular,([SpecialChars.rightSingleQuote]))]
    fun openStructureSegment sname =
    let val opStrings = Operators.getStringComponents PreprocessingOperators.publicStructureOp
    in 
    case opStrings of 
        [op1, op2] => [ 
        RichTextSegment(Red,Regular,op1), 
        RichTextSegment(Black,Regular,(StructureName.toString sname)), 
        RichTextSegment(Red,Regular,op2), 
        RichTextSegment(Red,Regular,([SpecialChars.leftSingleQuote])),
        Newline
        ]
        | _ => raise Fail "dg16"
    end

    fun showType(t) = UTF8String.fromString (PrettyPrint.show_typecheckingType t)
    fun getLastName(sname) =  List.drop(sname, List.length sname -1)

    fun typeMacroSegment sname t =
    let val opStrings = Operators.getStringComponents PreprocessingOperators.typeMacroOp
    in 
    case opStrings of 
        [op1, op2] => [ 
        RichTextSegment(Black,Regular,(StructureName.toString (getLastName sname))), 
        RichTextSegment(Red  ,Regular,op1), 
        RichTextSegment(Black,Regular,((showType t))), 
        RichTextSegment(Red,Regular,op2), 
        RichTextSegment(Red  ,Regular,([SpecialChars.period])),
        Newline
        ]
        | _ => raise Fail "dg33"
    end

    fun termTypeDecl sname t =
    let val opStrings = Operators.getStringComponents PreprocessingOperators.termTypeJudgmentOp
    in 
    case opStrings of 
        [op1, op2] => [ 
        RichTextSegment(Red  ,Regular,op1), 
        RichTextSegment(Black,Regular,(StructureName.toString (getLastName sname))), 
        RichTextSegment(Red,Regular,op2), 
        RichTextSegment(Black,Regular,((showType t))), 
        RichTextSegment(Red  ,Regular,([SpecialChars.period])),
        Newline
        ]
        | _ => raise Fail "dg33"
    end

    fun generateDocForCSigRecursive (currentStructureName : StructureName.t)
    (currentIndentLevel : int) 
    (cSig : TypeCheckingAST.CSignature) : RichTextDocument.t * int * StructureName.t = 
    let fun isInCurrentScope(candidateName : StructureName.t) =
        length candidateName = length currentStructureName + 1 
        andalso StructureName.isPrefix currentStructureName candidateName

        fun scopeUpdate(candidateName : StructureName.t) : (RichTextDocument.t * int) = 
        if isInCurrentScope candidateName then ([], currentIndentLevel) else
        let val agreedParts = StructureName.getAgreedPrefixParts candidateName currentStructureName
                 val indentBackoff = length currentStructureName - length agreedParts
                 val closingMarks = List.concat (List.tabulate(indentBackoff, (fn n => 
                            indentN (currentIndentLevel - n) @ closingStructureSegment @[Newline]
                        )
                    ))
                val indentAfterBackoff = currentIndentLevel - indentBackoff
                val indentProceed = length candidateName - length agreedParts - 1
                val openingMarks = List.concat( List.tabulate(indentProceed, (fn n => 
                            indentN (indentAfterBackoff + n) @  
                                (openStructureSegment ([List.nth (candidateName, length agreedParts + n)])) @[Newline]
                        )
                    ))
                val newIndent = indentAfterBackoff + indentProceed
            in 
                (closingMarks @ openingMarks, newIndent)
            end
        
        fun withScopeUpdate(candidateName : StructureName.t) (after : int -> RichTextDocument.t) : RichTextDocument.t * int  * StructureName.t = 
            case scopeUpdate candidateName  of 
                (c, i) => (c @(after i), i, List.take(candidateName, length candidateName - 1))

        fun showCDecl (x : TypeCheckingAST.CDeclaration) : (RichTextDocument.t  * int* StructureName.t) = 
        case x of 
             CTypeMacro(name, t)  =>  withScopeUpdate name (fn i => 
                                            (indentN i @ typeMacroSegment name t)
                                        )
            | CImport _ => ([], currentIndentLevel, currentStructureName)
            | CTermDefinition(name, _, t) => withScopeUpdate name (fn i => 
                                            (indentN i @ termTypeDecl name t)
                                        )
            | CDirectExpr(_) => ([], currentIndentLevel, currentStructureName)

    in
        case cSig of 
            [] => ([], currentIndentLevel, currentStructureName)
            | (d::ds) => let val (content, nextIdent, nextSName) = showCDecl d
                             val (restContent, finalIndent, finalSName) = generateDocForCSigRecursive nextSName nextIdent ds
                         in (content @ restContent, finalIndent, finalSName)
                         end
    end


    fun generateDocForCompilationFile (relativeStructureName : StructureName.t) (cfile : compilationfile) : RichTextDocument.t = 
        let val cSig = valOfSafe (getTypeCheckedAST cfile) (fn _ => raise Fail "tcast not requested")
        val initialHeader = openStructureSegment relativeStructureName 
        val (middleContent, finalIndent, finalSName) = generateDocForCSigRecursive (relativeStructureName) 1 cSig
         val closingMarks = List.concat (List.tabulate(finalIndent, (fn n => 
                            indentN (finalIndent - n-1) @ closingStructureSegment @[Newline]
                        )
                    ) )
        in 
        initialHeader @ middleContent @ closingMarks
        end


    fun generateDocs (moduleRootPath : filepath) (outputRoot : filepath) (cm : compilationmanager) = 
    if length (List.concat (map (#2) (collectAllDiagnostics cm))) > 0 
    then (DebugPrint.p (PrintDiagnostics.showErrs (List.concat (map (#2) (collectAllDiagnostics cm))) cm)
        ;
        raise Fail "cm has build errors, fix those first before generating docs")
    else
    let val module = lookupModuleForFilePath moduleRootPath cm
        val generatedDocs = map (fn (fp, cfile) => 
            (fp, generateDocForCompilationFile (getRelativeStructureName moduleRootPath (make fp)) cfile)
            ) (StrDict.toList (! (#files module)))
        val docToStr = outputToHTML
        val _ = map (fn (fp, doc) => 
                let val actualFileName = 
                    PathUtil.concat 
                    ((access outputRoot)::(map UTF8String.toString (getRelativeStructureName moduleRootPath (make fp)))) ^ ".html"
                    val fOutput = docToStr doc
                    val _ = IOUtil.writeFile (actualFileName) fOutput
                in () end
            ) generatedDocs
    in 
        ()
    end


end
