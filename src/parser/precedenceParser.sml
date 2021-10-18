

functor PrecedenceParser ( structure Options :PARSER_OPTIONS) = struct 
    val DEBUG = true
    (* val DEBUG = false *)
    
            open Operators
            open ParseAST
            (* DICT defined in cmlib/dict.sig *)
            structure PredDict : DICT =
                RedBlackDict
                (structure Key = IntOrdered)
            structure UTF8StringSet : SET = RedBlackSet(structure Elem = UTF8StringOrdered)
            structure UTF8CharSet : SET = RedBlackSet(structure Elem = UTF8CharOrdered)
        type parser = UTF8String.t -> (ParseOpAST* (UTF8String.t)) list 

        (*Remove duplicate https://stackoverflow.com/questions/21077272/remove-duplicates-from-a-list-in-sml *)
        fun isolate [] = []
            | isolate (x::xs) = x::isolate(List.filter (fn y => y <> x) xs)
   val debugAlternativeEntryTimes : string list ref  = ref []
   fun pushDebugIndent (s : string) = if DEBUG 
   then debugAlternativeEntryTimes := (!debugAlternativeEntryTimes @ [s])
   else ()
   fun popDebugIndent () = if DEBUG 
   then debugAlternativeEntryTimes := ((List.take (!debugAlternativeEntryTimes, (List.length(!debugAlternativeEntryTimes)-1))))
   else ()
   fun indentString ()  = String.concat((List.take (!debugAlternativeEntryTimes, (List.length(!debugAlternativeEntryTimes)-1))))
                
        fun debug (s : string) (p : parser) : parser = fn exp =>
        let
        val _ = pushDebugIndent("⋅")
        val _ = if DEBUG then 
                    (print (indentString() ^
                "╔ PARSER DEBUG: " ^ s ^ " exp is " ^ UTF8String.toString exp ^ "\n") )
                        else ()
        val res = p exp
        val _ = if DEBUG then print (indentString() ^
                    ("╚ " ^ s ^" Has " ^ Int.toString(List.length(res)) ^ " parses\n"))
            else ()
        val _ = popDebugIndent()
            in 
            res
            end
        fun combineAST (pr: ParseRule) : ParseOpAST list -> ParseOpAST = fn l => ParseOpAST (pr , l)

        fun combineASTByExtractingFromInternal 
                (pr: operator -> ParseRule) (i : int) : ParseOpAST list -> ParseOpAST = fn l => 
            case List.nth(l, i) of
                ParseOpAST(OperatorInternal oper, _) => ParseOpAST (pr oper, l)
                | _ => raise Fail "52"

        fun parserResToList (l: (ParseOpAST* (UTF8String.t)) list  ): (ParseOpAST list* (UTF8String.t)) list 
            = map (fn (x, r) => ([x],r)) l
        fun listToParserResult (combine: ParseOpAST list -> ParseOpAST) (l: (ParseOpAST list* (UTF8String.t)) list  ): (ParseOpAST* (UTF8String.t)) list 
            = map (fn (x, r) => (combine x,r)) l

        fun seqL (pending : (ParseOpAST list * (UTF8String.t)) list) (p : parser) 
        : (ParseOpAST list * (UTF8String.t)) list =
            List.concat (List.map 
            (fn (asts, exp) => map (fn (ast, rest) => (asts@[ast], rest)) (p exp)) pending)

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


    fun try (p : parser) : parser = fn exp =>
                p exp
                (* handle ParseFailure s => [] *)

         

            fun alternativesTryAll (alt : parser list) : parser = fn exp =>
                let 
                val res = List.concat (List.tabulate
                (List.length alt, (fn i => 
                    let 
                    val _ = pushDebugIndent("┃")
                    val _ = if DEBUG then 
                    (print (indentString() ^
                        "┏ Trying " ^ Int.toString(i+1) ^ " of " ^ Int.toString(List.length alt) ^ " alternatives:  \n"))
                        else ()
                    val res = (try (List.nth(alt, i))) exp
                    val _ = if DEBUG then print (indentString() ^
                        "┗ Completed Trying " ^ Int.toString(i+1) ^ " of " ^ Int.toString(List.length alt) ^ " alternatives: Has " ^  Int.toString(List.length res) ^ " parses. \n")
                        else ()
                    val _ = popDebugIndent()
                    in 
                    (
                        res
                    )
                    end
                ) ))
                in 
                res 
                end

            fun alternativesTryOnce (alt : parser list) : parser = fn exp =>
                let 
                val shouldSkip = ref(false)
                val _ = pushDebugIndent("┃")
                val res = List.concat (List.tabulate
                (List.length alt, (fn i => 
                    if !shouldSkip then [] else  (*! means deref not negation *)
                    let 
                    val _ = print (String.concat(!debugAlternativeEntryTimes) ^
                        "┏ Trying " ^ Int.toString(i+1) ^ " of " ^ Int.toString(List.length alt) ^ " alternatives:  \n");
                    val res = (try (List.nth(alt, i))) exp
                    val _ = print (String.concat(!debugAlternativeEntryTimes) ^
                        "┗ Completed Trying " ^ Int.toString(i+1) ^ " of " ^ Int.toString(List.length alt) ^ " alternatives: Has " ^  Int.toString(List.length res) ^ " parses. \n");
                    in 
                    (
                        if List.length(res) > 0 then (shouldSkip:=true; res) else res
                    )
                    end
                ) ))
                val _ = popDebugIndent()
                in 
                res 
                end

            fun alternatives alt = alternativesTryAll alt

        val defaultSeenCharset =
        if Options.enableBracketedExpression 
        then foldr (fn (elem, acc) => 
            UTF8CharSet.insert acc elem) UTF8CharSet.empty
            [SpecialChars.leftSingleQuote, SpecialChars.rightSingleQuote]
        else UTF8CharSet.empty (* do not put quote as escape when we do not parse quotes *)

        fun parseExpWithOption (allOps :Operators.allOperators) (withEOF : bool) : parser =  fn exp =>
        let 
            fun  scanExpForUnknownId(allOps : operator list) (exp : UTF8String.t) : UTF8String.t list
            = let
                    fun go (seen : UTF8CharSet.set) (remaining : UTF8String.t) (pending : UTF8String.t) : UTF8String.t list
                    = case remaining of
                        [] => (case pending of [] => [] | _ => [pending])
                        | (s:: xs ) => if UTF8CharSet.member seen s
                                            then (case pending of   
                                                        [] => go seen xs pending 
                                                        | _ => pending :: go seen xs [])(*TODO add pending to seen and remove isolate *)
                                            else go seen xs (pending@[ s]) 
                    val allSeen = (foldr (fn (elem, acc) => ( UTF8CharSet.insert acc elem)) 
                                defaultSeenCharset
                                        (List.concat (map getAllOccuringNameChars allOps)))
                in isolate (go allSeen exp [])
                end
            val allUnkownIds  : UTF8String.t list= scanExpForUnknownId allOps exp 

            val _ = if DEBUG
            then print ("All unknown ids (count "^ Int.toString(List.length(allUnkownIds))^") :" ^ (String.concatWith ","
            (map (fn id => UTF8String.toString  id) allUnkownIds)) ^ "|<--END\n")
            else ()



            fun unknownIdComponentParser (s : UTF8Char.t): parser = fn exp => 
                case (exp) of
                    (y :: ys) => if y = s then [(ParseOpAST(UnknownIdComp y, []), ys)] else []
                    | _ => []
            fun unknownIdParser (id : UTF8String.t): parser = 
            case id of
                [] => raise Fail "pp144"
                | _ => sequence (combineAST UnknownId) (map unknownIdComponentParser id)

            fun allUnknownIdsParser() : parser =  if DEBUG
            then debug "UnkownIds" (allUnknownIdsParser_()) else allUnknownIdsParser_()
            and allUnknownIdsParser_() : parser = 
                alternatives (map unknownIdParser allUnkownIds)
                



          




            fun removeDuplicateInSorted (l : int list) : int list = case l of
                (x :: y :: xs) => if x = y then removeDuplicateInSorted (x :: xs) else x :: removeDuplicateInSorted (y :: xs)
                | l => l
            val allPrecedences : int list= removeDuplicateInSorted (Quicksort.sort 
                        (Int.compare) (map (fn (Operator(i,_,  _, _, _)) => i) allOps))
            val predDict  = 
            let val emptyDict : operator list PredDict.dict 
                = foldr (fn (p, d) => PredDict.insert d p []) PredDict.empty allPrecedences
            in foldr (fn (oper, d) => PredDict.insert d (getPrecedence oper) 
                    (oper :: PredDict.lookup d (getPrecedence oper))) emptyDict allOps
            end
                
            fun findOps (fixity : fixity) (pred : int) (assoc : associativity) : operator list = 
                List.filter (fn (Operator(p, f, a, _, _)) => p = pred andalso f = fixity andalso a = assoc) allOps

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


            val y = if DEBUG 
            then (print ("ALL PRECEDENCES "^ String.concatWith "," (map Int.toString allPrecedences) ^ "\n"))
            else ()
            val y = if DEBUG 
            then (print ("ALL OPERATORS "^ String.concatWith "," (map PrettyPrint.show_op allOps) ^ "\n"))
            else ()



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





            and parseStr s oper = 
            if DEBUG then debug ("parseStr trying to match >|" ^ UTF8String.toString s ^ "|<") (parseStr_ s oper)
                else parseStr_ s oper
            and parseStr_ (s : UTF8String.t) (o' : ParseOpAST) : parser = fn exp =>
                if UTF8String.size s = 0 then [(o', exp)] else
                case exp of
                    ( id :: exps)  => if hd s = id 
                                    then parseStr (tl s) o' exps
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
            
            (* and parseBinding (until : string) : parser = debug ("parseBinding until " ^ until) (parseBinding_ until) *)
            (* TODO: until only looks at single char, may want to check all chars of arg *)
            and parseBinding (until : UTF8String.t) : parser = fn exp =>
                let 
                    fun go (remaining : UTF8String.t) (pending : UTF8String.t) : (ParseOpAST* (UTF8String.t)) list= 
                        if List.length remaining < List.length until 
                        then (*add all pending and remaining and return *)
                              [(ParseOpAST(Binding(pending @ remaining), []), [])]
                        else if UTF8String.isPrefix until remaining
                             then [(ParseOpAST(Binding pending, []), remaining)]
                             else go (tl remaining) (pending @ [hd remaining])
                in go exp [] 
                end
                
                (* parse the internal *)
            and parseOpOperator_ (oper : operator) : parser = debug ("parsing " ^ PrettyPrint.show_op oper) (parseOpOperator oper)
            and parseOpOperator (oper : operator) : parser = fn exp => 
            case oper of
                    Operator (_, _, _, lst, _)  
                => let 
                    fun goFoldL (remaining : opComponentType list) (sofar :(ParseOpAST list * (UTF8String.t)) list) = 
                        case remaining of 
                            [] => listToParserResult (fn l => ParseOpAST (OperatorInternal oper,l)) sofar
                            | (OpCompString name::ys) => goFoldL ys (seqL sofar (parseStr name (ParseOpAST(OperatorNameComponent(name, oper), []))))
                            | (OpCompExpr :: ys) => goFoldL ys (seqL sofar (parseExp()))
                            | (OpCompBinding :: OpCompString name::ys) => goFoldL (tl remaining) (seqL sofar (parseBinding name))
                            | _ => raise Fail "59"
                    in 
                        case lst of
                            (OpCompString name :: tail) =>
                                goFoldL tail (parserResToList (parseStr name (ParseOpAST(OperatorNameComponent(name, oper), [])) exp))
                            | _ => raise Fail "pp245"
                    end

  (* Parses the bracket operation smartly 
                in the sense that if a bracket doesn't cotain 
                one of special characters, what's inside automatically 
                becomes a name and no furhter processing is applied *)
            and bracketParser () : parser = if DEBUG
            then debug ("bracketParser") (bracketParser_()) else bracketParser_()
            and bracketParser_ () : parser = fn exp =>
            if List.length exp = 0 then [] else
            if not (hd exp = SpecialChars.leftSingleQuote)
            then []
            else
            case parseBinding [SpecialChars.rightSingleQuote] (tl exp) of
                 [(ParseOpAST(Binding s, []), 
                    (closeQuoteChar::tail))] => 
                    if UTF8String.containsSomeChar  s [SpecialChars.leftSingleQuote, 
                    SpecialChars.leftDoubleQuote, SpecialChars.rightDoubleQuote, SpecialChars.period]
                    then (* parse internal as expression as usual*)
                        sequence (fn (hd ::_) => hd)
                        [parseExp(), (fn exp => 
                                if hd exp = SpecialChars.rightSingleQuote
                                then [(ParseOpAST(PlaceHolder, []), tl exp)]
                                else [] )] (tl exp)
                    else (*no special char so it's a name *)
                    [(ParseOpAST(QuotedName  s, []), tail)]
                | _ => raise Fail "binding is not running correctly 163"

            and upP (pred : int) : parser = if DEBUG 
            then debug ("upP "^ Int.toString(pred) ) (upP_ pred) else upP_ pred
            and upP_ (pred : int) : parser = 
                case nextPred pred of 
                SOME(np) => hat np
                | NONE =>  
                alternatives (List.concat [
                if Options.enableBracketedExpression then [ bracketParser()] else [],
                [allUnknownIdsParser()] (*still parse all possible identifiers *)
                ])
                (* Parse unkown doesn't work as we're not doing CPS*)
                (* fn exp => alternativesTryAll (List.tabulate(List.length(exp), fn l => conservativeId l)) exp *)
                (* (print ("no up for " ^ Int.toString (pred) ^ "\n"); fn x => []) *)


            and hat oper = if DEBUG then (debug ("hat_"^ Int.toString(oper)) (hat_ oper)) 
            else hat_ oper
            and hat_ (pred : int) : parser = let
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







            and many1 p = if DEBUG then debug "many1" (many1_ p) else many1_ p
            and many1_ (p : parser) : parser = fn exp => 
                let val base : (ParseOpAST list* (UTF8String.t)) list  = parserResToList (p exp)
                    fun f trying memory  = 
                    let
                        val next = seqL trying (if DEBUG then debug "many1 lookahead" p else p)
                    in if List.length(next) > 0
                        then (*try succeeded *) f next (next @ memory) (*keep both variants as some might fail during later stages*)
                        else memory
                    end
                in listToParserResult (fn l => ParseOpAST(Many1, l))  (f base base)
                end



      

        
            and eof () : parser = fn exp => 
                case exp of 
                    [] => [(ParseOpAST(EOF,[]), [])]
                    | _ => ([])

            and parseExp (): parser = 
            (*Becuase of inclusion of up P in hat P, this is enough *)
            hat (hd allPrecedences)
            
            and parseExpWithEOF' () : parser = 
            sequence (combineAST ExpWithEOF) [
                parseExp(), eof()
            ]
        in 
        debugAlternativeEntryTimes := [];
        if withEOF then
        parseExpWithEOF'()(exp)
        else parseExp()(exp)
        end


end
     