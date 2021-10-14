
structure ReplTest =
  struct
    structure CP = YuLangParseFn(YuLangLexer)
    fun test x = let
      val sm = AntlrStreamPos.mkSourcemap()
      val lex = YuLangLexer.lex sm;
      val strm = YuLangLexer.streamifyInstream (TextIO.openIn "src/test.yulang")
      val (r, strm', errs) = CP.parse lex AtomMap.empty strm
      in 
      print (String.concatWith "\n"
      (List.map (AntlrRepair.repairToString (YuLangTokens.toString) sm)
      errs)^"\n");
      
      (case r of 
      SOME(s) => let val s = s in print(PrettyPrint.show_rawast(s) ^ "\n")
      ;print "done ...\n"
      ;print "type checking...\n"
      ;print "Not Implemented!"
      (* (SimpleTypeChecking.checkSig s; print "type checking ok\n")
      handle SimpleTypeChecking.TypeCheckingFailure s => print ("type checking failed: "^s^"\n")
      end *)
      end
      )
      ; r 
      end

end