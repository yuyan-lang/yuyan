

structure SyntaxHighlight =
struct
    structure TokenType =
    struct
        (* datatype tokentype = TTString  
                           | TTComment
                           | TTKeyword
                           | TTNumber
                           | TTOperator *)
        open CompilationManager
                           
        datatype t = tokentype

        val allTokenTypes = [TkTpStructureKeyword
                       , TkTpTypeKeyword
                       , TkTpExprKeyword
                       , TkTpIdentifierBinder
                       , TkTpIdentifierReference
                       , TkTpCustomOperatorName
                       , TkTpStringLiteral
                       , TkTpLabel
                       ,TkTpComment
                       ]


        fun getString (x : tokenType) = case x of 
                        TkTpStructureKeyword => "TkTpStructureKeyword"
                       | TkTpTypeKeyword =>  "TkTpTypeKeyword"
                       | TkTpExprKeyword => "TkTpExprKeyword"
                       | TkTpIdentifierBinder => "TkTpIdentifierBinder"
                       | TkTpIdentifierReference => "TkTpIdentifierReference"
                       | TkTpCustomOperatorName => "TkTpCustomOperatorName"
                       | TkTpStringLiteral => "TkTpStringLiteral"
                       | TkTpLabel  => "TkTpLabel"
                       | TkTpComment => "TkTpComment"

        fun getIndex (x : tokenType) = case x of 
          TkTpStructureKeyword => 0
        | TkTpTypeKeyword =>  1
        | TkTpExprKeyword => 2
        | TkTpIdentifierBinder => 3
        | TkTpIdentifierReference => 4
        | TkTpCustomOperatorName => 5
        | TkTpStringLiteral => 6
        | TkTpLabel  => 7
        | TkTpComment => 8

    end

    structure TokenModifier =
    struct
        datatype tokenmodifier = 
                              TMDeclaration
                            | TMDefinition
        datatype t = tokenmodifier
        val allTokenModifiers = [TMDeclaration, TMDefinition]
        fun getString (x : tokenmodifier) = case x of 
                              TMDeclaration => "declaration"
                            | TMDefinition => "definition"
        fun getIndex (x : tokenmodifier) = case x of 
              TMDeclaration => 0
            | TMDefinition => 1
        
        fun getCombinedBits (x : tokenmodifier list ) = foldr (fn (tm, acc) => MathUtil.power 2 (getIndex tm) + acc) 0 x
    end


open CompilationManager
    fun getDataFromTokens (tokens : token list) : int list= 
    let val sorted = tokens
    val (_, result) = 
        foldl (fn (Token(s,TokenInfo tktp), 
            ((pls, pcs), data))=> 
            let val SourceRange.StartEnd(_, ls, cs, le, ce) = UTF8String.getSourceRange s
            in
            ((ls, cs),
            data@[ls - pls, 
                if ls = pls then cs - pcs else cs, 
                if ls = le then ce - cs else ce, 
                TokenType.getIndex tktp,
                0
                ])
            end
            ) ((0, 0), []) sorted
        in  result end
    
    fun highlightFile (filePath : FileResourceURI.t) (cm : CompilationStructure.compilationmanager): JSON.value = 
    let 
        open JSON
        open CompilationManager
        (* assume file already added *)
        val tokens = CompilationManager.requestFileProcessing filePath UpToLevelTypeCheckingInfo cm []
        val CompilationFile cfile = CompilationManager.lookupFileByPath filePath cm
        val data = getDataFromTokens ( ((#tokensInfo cfile)))
    in 
        OBJECT[
            ("data", ARRAY (map INT (map (fn (x : int) => IntInf.fromInt x) data)))
        ]

    end
end