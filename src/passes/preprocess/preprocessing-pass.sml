
structure PreprocessingPass = struct

    (* PTypeMacro of UTF8String.t * UTF8String.t
                       | PTypeJudgment of UTF8String.t * UTF8String.t
                       | PTermDefinition of UTF8String.t * UTF8String.t
                       | POpDeclaration *)
    open PreprocessingAST
    open Operators
    open StatementAST

    (* type t = T *)
    val typeMacroOp = Operators.parseOperatorStr "〇者〇也" false false 0 []
    (* e : T *)
    val termTypeJudgmentOp = Operators.parseOperatorStr "以〇为〇" false false 0 []
    (* #define e = E *)
    val termMacroOp = Operators.parseOperatorStr "设〇为〇" false false 0 []
    (* e = E *)
    val termDefinitionOp = Operators.parseOperatorStr "施〇乃为〇" false false 0 []
    (* infixl op 232 *)
    val opDeclarationOp = Operators.parseOperatorStr "术〇交〇序〇也" false false 0 []
    (* // *)
    val commentOp = Operators.parseOperatorStr "注〇" false false 0 []
    
    val declOps = [typeMacroOp, termTypeJudgmentOp, termMacroOp, termDefinitionOp, commentOp]

    exception PreprocessMalformedAssoc of UTF8String.t
    exception PreprocessMalformedPrecedence of UTF8String.t

    fun parseAssoc (s : UTF8String.t) : associativity = 
        if s = UTF8String.fromString "左"
        then LeftAssoc
        else 
        if s = UTF8String.fromString "右"
        then RightAssoc
        else 
        if s = UTF8String.fromString "无"
        then NoneAssoc
        else raise PreprocessMalformedAssoc s

    fun parsePrecedence (s : UTF8String.t) : int = 
    foldl (fn (c, acc) => 
        if s = UTF8String.fromString "零"
        then acc * 10 + 0
        else 
        if s = UTF8String.fromString "一"
        then acc * 10 + 1
        else 
        if s = UTF8String.fromString "二"
        then acc * 10 + 2
        else 
        if s = UTF8String.fromString "三"
        then acc * 10 + 3
        else 
        if s = UTF8String.fromString "四"
        then acc * 10 + 4
        else 
        if s = UTF8String.fromString "五"
        then acc * 10 + 5
        else 
        if s = UTF8String.fromString "六"
        then acc * 10 + 6
        else 
        if s = UTF8String.fromString "七"
        then acc * 10 + 7
        else 
        if s = UTF8String.fromString "八"
        then acc * 10 + 8
        else 
        if s = UTF8String.fromString "九"
        then acc * 10 + 9
        else raise PreprocessMalformedPrecedence s
    ) 0 s


    fun parseJudgment (s : MixedStr.t) : pJudgment = 
    (let
       (* val _ = print ("Parsing judgment on" ^ UTF8String.toString s ^ "\n"); *)
       val tp  = MixedStr.toPlainUTF8String
       val res = 
        case DeclarationParser.parseDeclarationSingleOutput declOps s of
            (oper, [l1, l2]) => 
            if oper = typeMacroOp
            then PTypeMacro (tp l1, l2)
            else if oper = termTypeJudgmentOp
            then PTermTypeJudgment (tp l1, l2)
            else if oper = termMacroOp
            then PTermMacro (tp l1, l2)
            else if oper = termDefinitionOp
            then PTermDefinition (tp l1, l2)
            else  raise Fail "pp34"
            | (oper, [l1, l2, l3]) =>  
                if oper = opDeclarationOp
                then POpDeclaration (tp l1, parseAssoc (tp l2), parsePrecedence (tp l3))
                else raise Fail "pp85"
            | (oper, [l1]) =>  
                if oper = commentOp
                then PComment (l1)
                else raise Fail "pp95"
            | _ => raise Fail "pp26: malformed output : not two args or three args"
        (* val _ = print ("returning " ^ PrettyPrint.show_preprocessaastJ res) *)
        in res end
        handle DeclarationParser.DeclNoParse (expr) => PDirectExpr expr 
    )

    fun preprocessAST (s : MixedStr.t list) : PreprocessingAST.t = 
    (
        (* print (PrettyPrint.show_statementast s); *)
    (* case s of  *)
         (* [MixedStr.UnparsedDeclaration l]  =>  *)
         map parseJudgment s
        (* | _ => [PDirectExpr s] *)
    )
        
        

end
