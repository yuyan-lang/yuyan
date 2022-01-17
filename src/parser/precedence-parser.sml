(* Implements Nils Anders Danielsson, and Ulf Norell "Parsing Mixfix Operators"

with some Modifications
*)

structure PrecedenceParser  = struct 
    (* val DEBUG = true *)
    val DEBUG = false
    (* val DEBUGLIGHT = true *)
    val DEBUGLIGHT = false
    
            open Operators
            open ParseAST
            (* DICT defined in cmlib/dict.sig *)
            structure PredDict : DICT =
                RedBlackDict
                (structure Key = IntOrdered)
            structure IntDict : DICT =
                RedBlackDict
                (structure Key = IntOrdered)

                structure FixityHashable
   :> HASHABLE where type t = fixity
   =
   struct
      type t = fixity

      val eq : fixity * fixity -> bool = op =
      val hash = fn f => Word.fromInt (case f of Infix => 0 | Postfix => 1 | Prefix => 2 | Closed => 3)
   end
                structure AssocHashable
   :> HASHABLE where type t = associativity
   =
   struct
      type t = associativity

      val eq : associativity * associativity -> bool = op =
      val hash = fn f => Word.fromInt (case f of NoneAssoc => 0 | LeftAssoc => 1 | RightAssoc => 2)
   end
            structure AllOpsTable : TABLE =
                HashTable
                (structure Key = ProductHashable(structure X = FixityHashable;
                 structure Y = ProductHashable(structure X = IntHashable;
                  structure Y = AssocHashable)))
            structure UTF8StringSet : SET = RedBlackSet(structure Elem = UTF8StringOrdered)
            structure UTF8CharSet : SET = RedBlackSet(structure Elem = UTF8CharOrdered)
        type parser = MixedStr.t -> (ParseOpAST* (MixedStr.t)) list 

        (*Remove duplicate https://stackoverflow.com/questions/21077272/remove-duplicates-from-a-list-in-sml *)
        fun isolate [] = []
            | isolate (x::xs) = x::isolate(List.filter (fn y => not (UTF8String.semanticEqual y x)) xs)
   val debugAlternativeEntryTimes : string list ref  = ref []
   fun pushDebugIndent (s : string) = if DEBUG  
   then debugAlternativeEntryTimes := (!debugAlternativeEntryTimes @ [s])
   else ()
   fun popDebugIndent () = if DEBUG  andalso (List.length(!debugAlternativeEntryTimes)-1) > 0
   then debugAlternativeEntryTimes := ((List.take (!debugAlternativeEntryTimes, (List.length(!debugAlternativeEntryTimes)-1))))
   else ()
   fun indentString ()  = if DEBUG andalso (List.length(!debugAlternativeEntryTimes)-1) > 0 
   then String.concat((List.take (!debugAlternativeEntryTimes, (List.length(!debugAlternativeEntryTimes)-1)))) 
   else ""
                
        fun debug (s : string) (p : parser) : parser = fn exp =>
        let
        val _ = pushDebugIndent("⋅")
        val _ = if DEBUG then 
                    (print (indentString() ^
                "╔ PARSER DEBUG: " ^ s ^ " exp is " ^ MixedStr.toString exp ^ "\n") )
                        else ()
        val res = p exp
        val _ = if DEBUG then print (indentString() ^
                    ("╚ " ^ s ^" Has " ^ Int.toString(List.length(res)) ^ " parses\n"))
            else ()
        val _ = popDebugIndent()
            in 
            res
            end

        val ~= = UTF8Char.~=
        infix 4 ~= 

        fun combineAST (pr: ParseRule) : ParseOpAST list -> ParseOpAST = fn l => ParseOpAST (pr , l)

        fun combineASTByExtractingFromInternal 
                (pr: operator -> ParseRule) (i : int) : ParseOpAST list -> ParseOpAST = fn l => 
            case List.nth(l, i) of
                ParseOpAST(OperatorInternal oper, _) => ParseOpAST (pr oper, l)
                | _ => raise Fail "52"

        fun parserResToList (l: (ParseOpAST* (MixedStr.t)) list  ): (ParseOpAST list* (MixedStr.t)) list 
            = map (fn (x, r) => ([x],r)) l
        fun listToParserResult (combine: ParseOpAST list -> ParseOpAST) (l: (ParseOpAST list* (MixedStr.t)) list  ): (ParseOpAST* (MixedStr.t)) list 
            = map (fn (x, r) => (combine x,r)) l

        fun seqL (pending : (ParseOpAST list * (MixedStr.t)) list) (p : parser) 
        : (ParseOpAST list * (MixedStr.t)) list =
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
                    val _ = if DEBUG then pushDebugIndent("┃") else ()
                    val _ = if DEBUG then 
                    (print (indentString() ^
                        "┏ Trying " ^ Int.toString(i+1) ^ " of " ^ Int.toString(List.length alt) ^ " alternatives:  \n"))
                        else ()
                    val res = (try (List.nth(alt, i))) exp
                    val _ = if DEBUG then print (indentString() ^
                        "┗ Completed Trying " ^ Int.toString(i+1) ^ " of " ^ Int.toString(List.length alt) ^ " alternatives: Has " ^  Int.toString(List.length res) ^ " parses. \n")
                        else ()
                    val _ = if DEBUG then popDebugIndent() else ()
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
                in 
                res 
                end

            fun alternatives alt = 
                (
                    (* print ("alternatives on list of length " ^ Int.toString (List.length alt) ^ "\n");
                    alternativesTryAll alt;
                    print ("complete alternatives on list of length " ^ Int.toString (List.length alt) ^ "\n"); *)
                    alternativesTryAll alt
                )

(* todo : this can be removed as we are using mixstr *)
        val defaultSeenCharset =
        if true
        then foldr (fn (elem, acc) => 
            UTF8CharSet.insert acc elem) UTF8CharSet.empty
            [SpecialChars.leftSingleQuote, SpecialChars.rightSingleQuote]
        else UTF8CharSet.empty (* do not put quote as escape when we do not parse quotes *)

    type parseExceptionInfo = {
            str : MixedStr.t,
            allUnkownIds : UTF8String.t list,
            allRelevantOps : Operators.operator list,
            allOps : Operators.operator list
        }

    fun showParseExceptionInfo (x : parseExceptionInfo)(errStringInTheMiddle : string) : (string * string option) = 
        ("在理解(parse)`" ^  MixedStr.toString (#str x) ^ "`时出现问题："
        ^ errStringInTheMiddle,  
        SOME( "调试信息：\n"
        ^ "所有可能的名称(all unknown ids)：" ^ String.concatWith "，" (map UTF8String.toString (#allUnkownIds x))
        ^ "\n所有相关的操作符(all relevant ops)：" ^ String.concatWith "，" (map PrettyPrint.show_op (#allRelevantOps x))
        ^ "\n所有（包括不相关的）操作符(all ops)：" ^ String.concatWith "，" (map PrettyPrint.show_op (#allOps x))))

    exception NoPossibleParse of 
        (* debug message *)  parseExceptionInfo
    exception AmbiguousParse of ParseOpAST list * parseExceptionInfo

        fun parseExpWithOption (allOps :Operators.allOperators) : MixedStr.t -> (ParseOpAST* (MixedStr.t))  =  fn exp =>
        let 
            val _ = if DEBUG orelse DEBUGLIGHT then print ("PARSING " ^ MixedStr.toString exp ^ "\n") else ()
            val relevantOps = List.filter (fn oper => MixedStr.containsAllCharsTopLevel exp 
            (Operators.getAllOccuringNameChars oper)) allOps

            fun  scanExpForUnknownId(releavantOps : operator list) (exp : MixedStr.t) : UTF8String.t list
            = let
                    fun go (seen : UTF8CharSet.set) (remaining : MixedStr.t) (pending : UTF8String.t) : UTF8String.t list
                    = case remaining of
                        [] => (case pending of [] => [] | _ => [pending])
                        | (MixedStr.SChar s:: xs ) => if UTF8CharSet.member seen s
                                            then (case pending of   
                                                        [] => go seen xs pending 
                                                        | _ => pending :: go seen xs [])(*TODO add pending to seen and remove isolate *)
                                            else go seen xs (pending@[ s]) 
                        | (_ :: xs) => (case pending of   
                                                        [] => go seen xs pending 
                                                        | _ => pending :: go seen xs []) (* skip nontoplevel constructs *)
                    val allSeen = (foldr (fn (elem, acc) => ( UTF8CharSet.insert acc elem)) 
                                defaultSeenCharset
                                        (List.concat (map getAllOccuringNameChars relevantOps)))
                in isolate (go allSeen exp [])
                end
            val allUnkownIds  : UTF8String.t list= scanExpForUnknownId relevantOps exp 

            val debugAllUnknownId = ("All unknown ids (count "^ Int.toString(List.length(allUnkownIds))^") :" ^ (String.concatWith ","
            (map (fn id => UTF8String.toString  id) allUnkownIds)) ^ "|<--END\n")
            val _ = if DEBUG orelse DEBUGLIGHT
            then print debugAllUnknownId
            else ()



            fun unknownIdComponentParser (s : UTF8Char.t): parser = fn exp => 
                case (exp) of
                    (MixedStr.SChar y :: ys) => if  y ~= s then [(ParseOpAST(UnknownIdComp y, []), ys)] else []
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
                        (Int.compare) (map (fn (Operator(i,_,  _, _, _)) => i) relevantOps))
            val nextPredDict : int IntDict.dict = let 
                fun go remaining d= case remaining of
                    (x1 :: x2 :: l) => go (x2 :: l) (IntDict.insert d x1 x2)
                    | _ => d
                in go allPrecedences IntDict.empty end

            val predDict  = 
            let val emptyDict : operator list PredDict.dict 
                = foldr (fn (p, d) => PredDict.insert d p []) PredDict.empty allPrecedences
            in foldr (fn (oper, d) => PredDict.insert d (getPrecedence oper) 
                    (oper :: PredDict.lookup d (getPrecedence oper))) emptyDict relevantOps
            end

            val allOpsTable : operator list AllOpsTable.table =(AllOpsTable.table (List.length relevantOps + 1))
            val _ = map (fn oper => case oper of
                Operator(p, f, a, _, _) => case AllOpsTable.find allOpsTable (f, (p,a )) of 
                    SOME l => AllOpsTable.insert allOpsTable (f, (p, a)) (oper::l)
                    | NONE =>  AllOpsTable.insert allOpsTable (f, (p, a)) [oper])  relevantOps
                
            fun findOps (fixity : fixity) (pred : int) (assoc : associativity) : operator list = 
               case AllOpsTable.find allOpsTable (fixity, (pred, assoc))
               of SOME l => l | NONE => []


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

            val debugAllPrecedences = 
("ALL PRECEDENCES "^ String.concatWith "," (map Int.toString allPrecedences) ^ "\n")

            val debugAllRelevantOps = ("ALL RELEVANT OPERATORS "^ String.concatWith "," (map PrettyPrint.show_op relevantOps) ^ "\n")
            val debugAllOps = ("ALL OPERATORS "^ String.concatWith "," (map PrettyPrint.show_op allOps) ^ "\n")

            val y = if DEBUG  orelse DEBUGLIGHT
            then (print debugAllPrecedences)
            else ()
            val y = if DEBUG  orelse DEBUGLIGHT
            then (print debugAllRelevantOps
            ;print debugAllOps)
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
                IntDict.find nextPredDict pred 

            (* fun up (pred : int) : operator list = 
                case nextPred pred of 
                SOME(np) => List.filter (fn (Operator(p', _, _, _)) => p' = np) allOps
                | NONE => (print ("no up for " ^ Int.toString (pred) ^ "\n"); []) *)





            and parseStr s oper = 
            if DEBUG then debug ("parseStr trying to match >|" ^ UTF8String.toString s ^ "|<") (parseStr_ s [] oper)
                else parseStr_ s [] oper
            and parseStr_ (s : UTF8String.t) (sofar : UTF8String.t) (o' : UTF8String.t -> ParseOpAST) : parser = fn exp =>
                if UTF8String.size s = 0 then [(o' sofar, exp)] else
                case exp of
                    ( id :: exps)  => if MixedStr.isChar (id) (hd s)
                                    then parseStr_ (tl s) (sofar@[MixedStr.getChar id]) o' exps
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
            (* TODO[DONE]: until only looks at single char, may want to check all chars of arg *)
            (* UNTIL is not used if the head of the char is a named word *)
            and parseBinding (until : UTF8String.t) : parser = fn exp =>

                let 
                    fun go (remaining : MixedStr.t) (pending : UTF8String.t) : (ParseOpAST* (MixedStr.t)) list= 
                    (* parses only SChar's *)
                        if List.length remaining < List.length until 
                        then (* no parse if remaining isn't sufficient to guarantee until *)
                              []
                        else if MixedStr.isPrefix until remaining
                             then [(ParseOpAST(Binding pending, []), remaining)]
                             else case remaining of 
                                    (x :: xs) => case x of 
                                        MixedStr.SChar y =>  go xs (pending @ [y])
                                        | _ => [] (* no parse if not plain characters in binding *)
                in case exp of 
                    [] => [] (* no parse if exp is empty *)
                    | (x :: xs) => if MixedStr.isPlainChar x (* check if plain char *)
                                    then go exp [] 
                                    else case x of
                                          MixedStr.Name(x, qi) => [(ParseOpAST(Binding x, []), xs)]
                                    (* otherwise return the first name whatever that is *)
                                          | _ => [] (*fail if first term is not name, need name for binding *)
                end
                
                (* parse the internal *)
            and parseOpOperator_ (oper : operator) : parser = debug ("parsing " ^ PrettyPrint.show_op oper) (parseOpOperator oper)
            and parseOpOperator (oper : operator) : parser = fn exp => 
            case oper of
                    Operator (_, _, _, lst, _)  
                => let 
                    fun goFoldL (remaining : opComponentType list) (sofar :(ParseOpAST list * (MixedStr.t)) list) = 
                        case remaining of 
                            [] => listToParserResult (fn l => ParseOpAST (OperatorInternal oper,l)) sofar
                            | (OpCompString name::ys) => goFoldL ys (seqL sofar (parseStr name (fn parsedName => ParseOpAST(OperatorNameComponent(parsedName, oper), []))))
                            | (OpCompExpr :: ys) => goFoldL ys (seqL sofar (parseExp()))
                            | (OpCompBinding :: OpCompString name::ys) => goFoldL (tl remaining) (seqL sofar (parseBinding name))
                            | _ => raise Fail "59"
                    in 
                        case lst of
                            (OpCompString name :: tail) =>
                                goFoldL tail (parserResToList (parseStr name (fn parsedName => ParseOpAST(OperatorNameComponent(parsedName, oper), [])) exp))
                            | _ => raise Fail "pp245"
                    end

(* Bracket parser becomes obsolete as we're preprocessing all brackets via mixed str *)
  (* Parses the bracket operation smartly 
                in the sense that if a bracket doesn't cotain 
                one of special characters, what's inside automatically 
                becomes a name and no furhter processing is applied *)
            (* and bracketParser () : parser = if DEBUG
            then debug ("bracketParser") (bracketParser_()) else bracketParser_()
            and bracketParser_ () : parser = fn exp =>
            if List.length exp = 0 then [] else
            if not (hd exp = SpecialChars.leftSingleQuote) (* fail if exp didn't start with openleftbracket *)
            then []
            else
                    (
                        (* print ("inner parsing (quote) on" ^ UTF8String.toString exp ^ "\n"); *)
            case BracketScanner.scanUntilCorrespondingRightQuote 1 0 (tl exp) of
                (contentIncludingPossiblyQuote, rest, includesInnerQuote, includesPeriod) => 
                if List.last contentIncludingPossiblyQuote = SpecialChars.rightSingleQuote
                then (* parse *)
                    (if includesPeriod 
                    then raise Fail "Not Implemented : Cannot handle block inside expression"
                    else 
                        case includesInnerQuote of
                            false => (* no quote, it's just a name *) [(ParseOpAST(QuotedName  (UTF8String.stripTail 
                                        contentIncludingPossiblyQuote), []), rest)]
                            |  true => (* is an expression inside *)
                                    map (fn (opast, [] (* should be empty *)) => (opast, rest))
                                        (
                                            parseExpWithEOF()(UTF8String.stripTail contentIncludingPossiblyQuote))
                    )
                else (* parse failed, maybe we're at the end of the input *) []
                    ) *)
            (* case parseBinding [SpecialChars.rightSingleQuote] (tl exp) of
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
                | _ => raise Fail "binding is not running correctly 163" *)
            
            and structuralParser () : parser = fn exp => 
                case exp of 
                    [] => [] (* fail if exp doesn't have content *)
                    | (MixedStr.UnparsedExpression(s, qi) :: xs) => 
                    [(ParseOpAST(UnparsedExpr s, []), xs)]
                    (* TODO SHOULD NOT PARSE, let the coordinator handle parsing to support better
                    error messages ! *)
                         (* [(fn (opast, [] (* should be empty *)) => (opast, xs))
                            (* backtracking by considering all Ops *)
                         (parseExpWithOption(allOps)(s))]  *)
                    | (MixedStr.UnparsedDeclaration(s,qi) :: xs) => 
                            [(ParseOpAST(UnparsedDecl(map (fn (x, ei) => (x, ei)) s), []), xs)]
                    | (MixedStr.Name(s, qi) :: xs) => 
                            [(ParseOpAST(QuotedName(s), []), xs)]
                    | (MixedStr.Literal (s, qi) :: xs) => 
                            [(ParseOpAST(StringLiteral(s, qi), []), xs)]
                    | _ => [] (* fail for all other cases *)

                

            and topMostParser() : parser = 
                alternatives (List.concat [
                (* if Options.enableBracketedExpression then [ bracketParser()] else [], *)
                [structuralParser()],
                [allUnknownIdsParser()] (*still parse all possible identifiers *)
                ])
                (* Parse unkown doesn't work as we're not doing CPS*)
                (* fn exp => alternativesTryAll (List.tabulate(List.length(exp), fn l => conservativeId l)) exp *)

            and upP (pred : int) : parser = if DEBUG 
            then debug ("upP "^ Int.toString(pred) ) (upP_ pred) else upP_ pred
            and upP_ (pred : int) : parser = 
                case nextPred pred of 
                SOME(np) => hat np
                | NONE => topMostParser()


            and hat oper = if DEBUG then (debug ("hat_"^ Int.toString(oper)) (hat_ oper)) 
            else hat_ oper
            and hat_ (pred : int) : parser = let
                (* val _ = print ("debug" ^ Int.toString pred ^ "\n"); *)
                val masking = PredDict.lookup opersPresentAtPred pred
                val upPpred = upP pred
                in
                alternatives (List.concat [
                    (* closed case *)
                    if List.nth(masking, 0) then [] else
                        [parseOpFixityPred Closed pred NoneAssoc], 

                    (* non assoc prefix and postfix *)
                    if List.nth(masking, 1) then [] else
                    [sequence (combineASTByExtractingFromInternal PrefixNoneAssoc 0) 
                        [parseOpFixityPred Prefix pred NoneAssoc, upPpred]],
                    if List.nth(masking, 2) then [] else
                    [sequence (combineASTByExtractingFromInternal PostfixNoneAssoc 1) 
                        [upPpred, parseOpFixityPred Postfix pred NoneAssoc]],

                    (* assoc prefix and postfix *)
                    if List.nth(masking, 3) then [] else
                    [sequence (combineAST (PrefixRightAssoc pred)) 
                        [many1 (parseOpFixityPred Prefix pred RightAssoc), upPpred]],
                    if List.nth(masking, 4) then [] else
                    [sequence (combineAST (PostfixLeftAssoc pred)) 
                        [upPpred, many1 (parseOpFixityPred Postfix pred LeftAssoc)]],
                    
                    (* binary *)
                    if List.nth(masking, 5) then [] else
                    [sequence (combineASTByExtractingFromInternal InfixNoneAssoc 1) 
                        [upPpred, parseOpFixityPred Infix pred NoneAssoc, upPpred]],
                    if List.nth(masking, 6) then [] else
                    [sequence (combineAST (InfixLeftAssoc pred)) [upPpred, 
                            many1 (sequence (combineASTByExtractingFromInternal InfixLeftAssocLeftArrow 0) 
                                [parseOpFixityPred Infix pred LeftAssoc, upPpred])]],
                    if List.nth(masking, 7) then [] else
                    [sequence (combineAST (InfixRightAssoc pred)) [
                            many1 (sequence (combineASTByExtractingFromInternal InfixRightAssocRightArrow 1) 
                            [upPpred , parseOpFixityPred Infix pred RightAssoc]), upPpred]],
                    
                    (* I think the paper made a mistake here, we also directly need to push up the precedence *)
                    [upPpred]
                ])
                end







            and many1 p = if DEBUG then debug "many1" (many1_ p) else many1_ p
            and many1_ (p : parser) : parser = fn exp => 
                let val base : (ParseOpAST list* (MixedStr.t)) list  = parserResToList (p exp)
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
        (if DEBUG 
        then (print ("parseExp \n"))
        else ();
         (* might have no ops because of extreme filtering *)
            ( case allPrecedences of 
                [] => topMostParser()
                | (p::_) => hat p))

            
            and parseExpWithEOF () : parser = 
            sequence (combineAST ExpWithEOF) [
                parseExp(), eof()
            ]

            val parseExceptionInfo = {
                str = exp,
                allUnkownIds = allUnkownIds,
                allRelevantOps= relevantOps,
                allOps = allOps
            }
        in 
        if DEBUG 
        then (print ("STARTING \n"))
        else ();
        (case parseExpWithEOF()(exp) of
            [] => raise NoPossibleParse parseExceptionInfo
            | [l] => l
            | l => raise AmbiguousParse ((map (fn(x,r) => x) l), parseExceptionInfo)
            (* ("Ambiguous Parse, possibilities : (total "^ Int.toString (length l) ^ ") \n" ^
            String.concatWith "\n" (map (fn (x,y) => PrettyPrint.show_parseopast x ) l)))
            handle NoPossibleParse s => raise NoPossibleParse ((s ^ "\n when parsing " ^ MixedStr.toString exp
            ^ "\n" ^ debugAllUnknownId ^ debugAllPrecedences ^ debugAllRelevantOps ^ debugAllOps),)
            | AmbiguousParse s => raise AmbiguousParse (s ^ "\n when parsing " ^ MixedStr.toString exp
            ^ "\n" ^ debugAllUnknownId ^ debugAllPrecedences ^ debugAllRelevantOps ^ debugAllOps) *)
            
            )
        end

    fun parseMixfixExpression (allOps :Operators.allOperators) (exp : MixedStr.t) : OpAST.t = 
            case parseExpWithOption allOps exp of
                                 (parseopast, _) => ElaboratePrecedence.elaborate parseopast


end
     