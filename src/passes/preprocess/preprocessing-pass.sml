
structure PreprocessingPass = struct

    (* PTypeMacro of UTF8String.t * UTF8String.t
                       | PTypeJudgment of UTF8String.t * UTF8String.t
                       | PTermDefinition of UTF8String.t * UTF8String.t
                       | POpDeclaration *)
    open PreprocessingAST
    open Operators
    open PreprocessingOperators



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
