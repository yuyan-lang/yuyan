
functor PrecedenceParser (P : sig 
        val allOps :Operators.allOperators 
        end)
        = struct 

        open Operators
        open RawAST
        open ParseAST

          (* DICT defined in cmlib/dict.sig *)
        structure PredDict : DICT =
            RedBlackDict
            (structure Key = IntOrdered)


        val allOps = P.allOps


        fun removeDuplicateInSorted (l : int list) : int list = case l of
            (x :: y :: xs) => if x = y then removeDuplicateInSorted (x :: xs) else x :: removeDuplicateInSorted (y :: xs)
            | l => l
        val allPrecedences : int list= removeDuplicateInSorted (Quicksort.sort 
                    (Int.compare) (map (fn (Operator(i,_,  _, _)) => i) allOps))
        val predDict  = 
        let val emptyDict : operator list PredDict.dict 
            = foldr (fn (p, d) => PredDict.insert d p []) PredDict.empty allPrecedences
        in foldr (fn (oper, d) => PredDict.insert d (getPrecedence oper) 
                (oper :: PredDict.lookup d (getPrecedence oper))) emptyDict allOps
        end
            
        fun findOps (fixity : fixity) (pred : int) (assoc : associativity) : operator list = 
            List.filter (fn (Operator(p, f, a, _)) => p = pred andalso f = fixity andalso a = assoc) allOps

        val opersPresentAtPred = 
        foldr (fn (p, d) => PredDict.insert d p [
            findOps Closed p NoneAssoc = [],
            findOps Prefix p NoneAssoc = [],
            findOps Postfix p NoneAssoc = [],
            findOps Prefix p RightAssoc = [],
            findOps Postfix p LeftAssoc = [],
            findOps Infix p NoneAssoc = [],
            findOps Infix p LeftAssoc = [],
            findOps Infix p RightAssoc = []
        ]) PredDict.empty allPrecedences


        val y = (print ("ALL PRECEDENCES "^ String.concatWith "," (map Int.toString allPrecedences)); 2)

        val debugAlternativeEntryTimes  = ref 0


 (*https://stackoverflow.com/questions/17826034/sml-get-index-of-item-in-list*)
        fun index(item, xs) =
        let
            fun index'(m, nil) = NONE
            | index'(m, x::xr) = if x = item then SOME m else index'(m + 1, xr)
        in
            index'(0, xs)
        end

        fun nextPred (pred : int) : int option = 
            case index(pred,  allPrecedences)of
                SOME(idx) => (
                    (* print ("some next pred for " ^ Int.toString pred  ^ " idx is " ^ Int.toString idx ^ "\n"); *)
                                if idx + 1 < List.length(allPrecedences) 
                              then SOME(List.nth(allPrecedences, (idx + 1))) 
                              else NONE)
                | NONE => (print ("no next pred for " ^ Int.toString pred ^ "\n");
                 raise Fail "not possible 27")

        (* fun up (pred : int) : operator list = 
            case nextPred pred of 
            SOME(np) => List.filter (fn (Operator(p', _, _, _)) => p' = np) allOps
            | NONE => (print ("no up for " ^ Int.toString (pred) ^ "\n"); []) *)


        type parser = RawAST list -> (ParseOpAST* (RawAST list)) list 

        fun combineAST (pr: ParseRule) : ParseOpAST list -> ParseOpAST = fn l => ParseOpAST (pr , l)

        fun combineASTByExtractingFromInternal 
                (pr: operator -> ParseRule) (i : int) : ParseOpAST list -> ParseOpAST = fn l => 
            case List.nth(l, i) of
                ParseOpAST(OperatorInternal oper, _) => ParseOpAST (pr oper, l)
                | _ => raise Fail "52"

        fun parserResToList (l: (ParseOpAST* (RawAST list)) list  ): (ParseOpAST list* (RawAST list)) list 
        = map (fn (x, r) => ([x],r)) l
        fun listToParserResult (combine: ParseOpAST list -> ParseOpAST) (l: (ParseOpAST list* (RawAST list)) list  ): (ParseOpAST* (RawAST list)) list 
        = map (fn (x, r) => (combine x,r)) l

        (* and parseStr s oper = debug "parseStr" (parseStr_ s oper) *)
        and parseStr (s : string) (o' : ParseOpAST) : parser = fn exp =>
            if String.size s = 0 then [(o', exp)] else
            case exp of
                (RawID id :: exps)  => if String.isPrefix id s 
                                then parseStr (String.extract(s, String.size(id), NONE)) o' exps
                                else [] (* no parse failure should be raised as it will disable valid parse from being processed in the capture point *)
                                (* raise ParseFailure ("Cannot match " ^ s ^ " against " ^ id) strip off id from s *)
                (* | (RawList l :: exps) => raise ParseFailure ("Cannot match "^ s ^ " against a rawlist") *)
                | _ =>  [] (* no parse failure should be raised as it will disable valid parse from being processed in the capture point *)
                    (* raise ParseFailure ("Running out of input") *)

                (* TODO : Use a map for lookup, should be much quicker *)

        and parseOpFixityPred (fixity : fixity) (pred : int)(assoc : associativity) : parser = 
            (* debug ("ParseOpFixityPred " ^ Int.toString(pred)) *)
             (parseOpFixityPred_ fixity pred assoc)
        and parseOpFixityPred_ (fixity : fixity) (pred : int)(assoc : associativity) : parser = 
            alternatives (map parseOpOperator (findOps fixity pred assoc))
            
            (* parse the internal *)
        and parseOpOperator (oper : operator) : parser = fn exp => 
        case oper of
                Operator (pred, fixity, assoc, lst)  
            => case lst of
                (hd :: tl) => 
                let 
                  val rawComponents = List.foldl (fn (name : string, acc) => seqL (seqL acc (parseExp())) 
                (parseStr name (ParseOpAST(OperatorNameComponent(name, oper), []))) )
                    (parserResToList (parseStr hd (ParseOpAST(OperatorNameComponent(hd,oper), [])) exp)) tl
                in listToParserResult (fn l => ParseOpAST (OperatorInternal oper,l)) rawComponents
                end
                | _ => raise Fail "59"


        and upP (pred : int) : parser = 
            case nextPred pred of 
            SOME(np) => hat np
            | NONE =>  fn x => []
            (* (print ("no up for " ^ Int.toString (pred) ^ "\n"); fn x => []) *)


        (* and hat oper = (debug ("hat_"^ Int.toString(oper)) (hat_ oper)) *)
        and hat (pred : int) : parser = let
            val masking = PredDict.lookup opersPresentAtPred pred
            in
            alternatives (List.concat [
                (* closed case *)
                if List.nth(masking, 0) then [] else
                    [parseOpFixityPred Closed pred NoneAssoc], 

                (* non assoc prefix and postfix *)
                if List.nth(masking, 1) then [] else
                [sequence (combineASTByExtractingFromInternal PrefixNoneAssoc 0) 
                    [parseOpFixityPred Prefix pred NoneAssoc, upP pred]],
                if List.nth(masking, 2) then [] else
                [sequence (combineASTByExtractingFromInternal PostfixNoneAssoc 1) 
                    [upP pred, parseOpFixityPred Postfix pred NoneAssoc]],

                (* assoc prefix and postfix *)
                if List.nth(masking, 3) then [] else
                [sequence (combineAST (PrefixRightAssoc pred)) 
                    [many1 (parseOpFixityPred Prefix pred RightAssoc), upP pred]],
                if List.nth(masking, 4) then [] else
                [sequence (combineAST (PostfixLeftAssoc pred)) 
                    [upP pred, many1 (parseOpFixityPred Postfix pred LeftAssoc)]],
                
                (* binary *)
                if List.nth(masking, 5) then [] else
                [sequence (combineASTByExtractingFromInternal InfixNoneAssoc 1) 
                    [upP pred, parseOpFixityPred Infix pred NoneAssoc, upP pred]],
                if List.nth(masking, 6) then [] else
                [sequence (combineAST (InfixLeftAssoc pred)) [upP pred, 
                        many1 (sequence (combineASTByExtractingFromInternal InfixLeftAssocLeftArrow 0) 
                            [parseOpFixityPred Infix pred LeftAssoc, upP pred])]],
                if List.nth(masking, 7) then [] else
                [sequence (combineAST (InfixRightAssoc pred)) [
                        many1 (sequence (combineASTByExtractingFromInternal InfixRightAssocRightArrow 1) 
                        [upP pred , parseOpFixityPred Infix pred RightAssoc]), upP pred]],
                
                (* I think the paper made a mistake here, we also directly need to push up the precedence *)
                [upP pred]
            ])
            end

        and show_rawast_list exp = String.concatWith ", " (map PrettyPrint.show_rawast exp)

        and debug (s : string) (p : parser) : parser = fn exp =>
            (print (
            String.concat(List.tabulate(!debugAlternativeEntryTimes, (fn _ => "┃")))^
                " PARSER DEBUG: " ^ s ^ " exp is " ^ show_rawast_list exp ^ "\n"); 
            let val res = p exp
            in (print (s ^" Has " ^ Int.toString(List.length(res)) ^ " parses");
                print (String.concatWith "\n " (map (fn (past, r) => "AST " ^ PrettyPrint.show_parseopast past ^ " REST IS " ^ show_rawast_list r ) res ) ^ "\n");
                res
            )
            end
            )


        and alternatives alt = alternativesTryAll alt

        and alternativesTryAll (alt : parser list) : parser = fn exp =>
            let 
            val _ = debugAlternativeEntryTimes := !debugAlternativeEntryTimes + 1;
            val res = List.concat (List.tabulate
            (List.length alt, (fn i => 
                let 
                (* val _ = print (String.concat(List.tabulate(!debugAlternativeEntryTimes-1, (fn _ => "┃"))) ^ *)
                    (* "┏ Trying " ^ Int.toString(i+1) ^ " of " ^ Int.toString(List.length alt) ^ " alternatives:  \n"); *)
                val res = (try (List.nth(alt, i))) exp
                (* val _ = print (String.concat(List.tabulate(!debugAlternativeEntryTimes-1, (fn _ => "┃"))) ^ *)
                    (* "┗ Completed Trying " ^ Int.toString(i+1) ^ " of " ^ Int.toString(List.length alt) ^ " alternatives: Has " ^  Int.toString(List.length res) ^ " parses. \n"); *)
                in 
                (
                    res
                )
                end
            ) ))
            val _ = debugAlternativeEntryTimes := !debugAlternativeEntryTimes - 1
            in 
            res 
            end

        and alternativesTryOnce (alt : parser list) : parser = fn exp =>
            let 
            val shouldSkip = ref(false)
            val _ = debugAlternativeEntryTimes := !debugAlternativeEntryTimes + 1;
            val res = List.concat (List.tabulate
            (List.length alt, (fn i => 
                if !shouldSkip then [] else 
                let 
                val _ = print (String.concat(List.tabulate(!debugAlternativeEntryTimes-1, (fn _ => "┃"))) ^
                    "┏ Trying " ^ Int.toString(i+1) ^ " of " ^ Int.toString(List.length alt) ^ " alternatives:  \n");
                val res = (try (List.nth(alt, i))) exp
                val _ = print (String.concat(List.tabulate(!debugAlternativeEntryTimes-1, (fn _ => "┃"))) ^
                    "┗ Completed Trying " ^ Int.toString(i+1) ^ " of " ^ Int.toString(List.length alt) ^ " alternatives: Has " ^  Int.toString(List.length res) ^ " parses. \n");
                in 
                (
                    if List.length(res) > 0 then (shouldSkip:=true; res) else res
                )
                end
            ) ))
            val _ = debugAlternativeEntryTimes := !debugAlternativeEntryTimes - 1
            in 
            res 
            end


        and seqL (pending : (ParseOpAST list * (RawAST list)) list) (p : parser) 
        : (ParseOpAST list * (RawAST list)) list =
            List.concat (List.map 
            (fn (asts, exp) => map (fn (ast, rest) => (asts@[ast], rest)) (p exp)) pending)


        (* and many1 p = debug "many1" (many1_ p) *)
        and many1 (p : parser) : parser = fn exp => 
            let val base = parserResToList (p exp)
                fun f b = 
                let
                    val next = seqL b(try p)
                in if List.length(next) > 0
                    then (*try succeeded *) f (next)
                    else b
                end
            in listToParserResult (fn l => ParseOpAST(Many1, l)) (f base)
            end


        (* and sequence c p = debug "sequence" (sequence_ c p) *)
        and sequence (combine : ParseOpAST list -> ParseOpAST) (parserSeq : parser list) : parser = fn exp =>
            (* for early returning *)
                case parserSeq of 
                    [] => raise Fail "Cannot have empty sequence"
                    | (p1 :: ps) => List.map (fn (asts, r) => (combine asts, r)) 
                    (List.foldl (fn (curParser, acc) => 
                    case acc of [] => []
                    | _ => seqL acc curParser) 
        (parserResToList (p1 exp)) ps) (*map parsers over exp usign seq *)


        and try (p : parser) : parser = fn exp =>
            p exp
            handle ParseFailure s => []
            
        and eof () : parser = fn exp => 
            case exp of 
                [] => [(ParseOpAST(EOF,[]), [])]
                | _ => []

        and parseExp (): parser = 
        (*Becuase of inclusion of up P in hat P, this is enough *)
         hat (hd allPrecedences)
        
        and parseExpWithEOF () : parser = 
        sequence (combineAST ExpWithEOF) [
            parseExp(), eof()
        ]


end
     