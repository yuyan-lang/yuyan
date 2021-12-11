

structure SyntaxHighlight =
struct
    structure TokenType =
    struct
        datatype tokentype = TTString  
                           | TTComment
                           | TTKeyword
                           | TTNumber
                           | TTOperator
        datatype t = tokentype

        val allTokenTypes = [TTString, TTComment, TTKeyword, TTNumber, TTOperator]
        fun getString (x : tokentype) = case x of 
         TTString   => "string"
        | TTComment => "comment"
        | TTKeyword => "keyword"
        | TTNumber => "number"
        | TTOperator => "operator"
        fun getIndex (x : tokentype) = case x of 
         TTString   => 0
        | TTComment => 1
        | TTKeyword => 2
        | TTNumber => 3
        | TTOperator => 4

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

    fun uriToPath (uri : string) : string = 
        if String.isPrefix "file://" uri
        then String.extract (uri, String.size "file://", NONE)
        else uri

    fun highlightFile (fileUri : string) : JSON.value = 
    let 
        open JSON
        val fileContent = TextIO.inputAll (TextIO.openIn (uriToPath fileUri))
    in 
    OBJECT [
        ("f", STRING fileContent)
    ]
    end
end