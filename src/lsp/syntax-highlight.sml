

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
        foldl (fn (Token(SourceRange.StartEnd(_, ls, cs, le, ce), _,TokenInfo tktp), 
            ((pls, pcs), data))=> 
            ((ls, cs),
            data@[ls - pls, 
                if ls = pls then cs - pcs else cs, 
                if ls = le then ce - cs else ce, 
                TokenType.getIndex tktp,
                0
                ])
            ) ((0, 0), []) sorted
        in  result end

    fun highlightFile (filePath : string) (cm : CompilationManager.compilationmanager): JSON.value = 
    let 
        open JSON
        open CompilationManager
        val _ = CompilationManager.compileFile filePath cm
        val (_, tokens) = StrDict.lookup ((!(#currentModule cm))) filePath
        val data = getDataFromTokens tokens
    in 
        OBJECT[
            ("data", ARRAY (map INT (map (fn (x : int) => IntInf.fromInt x) data)))
        ]

    end
end