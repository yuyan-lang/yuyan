
structure ReplTest =
  struct
    structure CP = YuLangParseFn(YuLangLexer)

    fun topLevelGeneric strm = let
      val sm = AntlrStreamPos.mkSourcemap()
      val lex = YuLangLexer.lex sm;
      val (r, strm', errs) = CP.parse lex "" strm (*the third arg is the env, e.g. AtomMap.empty *)
      in 
      print (String.concatWith "\n"
      (List.map (AntlrRepair.repairToString (YuLangTokens.toString) sm)
      errs)^"\n");
      
      (case r of 
      SOME(s) => (let val s = s in print(PrettyPrint.show_rawasts(s) ^ "\n")
      ;print (InteractiveRepl.replHelper (UTF8String.toString (map RawAST.unId s )))
      ;print "done ...\n"
      ;print "type checking...\n"
      ;print "Not Implemented!"
      (* (SimpleTypeChecking.checkSig s; print "type checking ok\n")
      handle SimpleTypeChecking.TypeCheckingFailure s => print ("type checking failed: "^s^"\n")
      end *)
      end
      )
      | NONE => print "Parsing failed")
      ; r 
      end

    fun testFile x = let
      val strm = YuLangLexer.streamifyInstream (TextIO.openIn "testfiles/test.yuyan")
      in topLevelGeneric strm end
    fun testString (s : string)  = let
      val returned = ref (false) 
      val strm = YuLangLexer.streamify (fn () => if !returned 
                then "" else (returned := true;s))
    in topLevelGeneric strm end

    fun test x = let
      val str =  TextIO.inputAll (TextIO.openIn "testfiles/test.yuyan")
      in testString str end

    fun testT x = 
      (test(); raise Fail "completed")
      handle ExpressionConstructionPass.ElaborateFailure s => print s 
      handle ExpressionConstructionPass.ECPNoPossibleParse s=> print (UTF8String.toString s)


end