structure YuLangTokens =
  struct
    datatype token
      = ID of string
      | LEFT_PAREN
      | RIGHT_PAREN
      | EOF
    val allToks = [
            LEFT_PAREN, RIGHT_PAREN, EOF
           ]
    fun toString tok =
(case (tok)
 of (ID(_)) => "ID"
  | (LEFT_PAREN) => "\227\128\140"
  | (RIGHT_PAREN) => "\227\128\141"
  | (EOF) => "EOF"
(* end case *))
    fun isKW tok =
(case (tok)
 of (ID(_)) => false
  | (LEFT_PAREN) => false
  | (RIGHT_PAREN) => false
  | (EOF) => false
(* end case *))
    fun isEOF EOF = true
      | isEOF _ = false
  end (* YuLangTokens *)

functor YuLangParseFn (Lex : ANTLR_LEXER) = struct

  local
    structure Tok =
YuLangTokens
    structure UserCode =
      struct

fun file_PROD_1_ACT (component, env, component_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (component)
fun component_PROD_1_ACT (ID, env, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (RawAST.RawID(ID))
fun ARGS_3 (env) = 
  (env)
      end (* UserCode *)

    structure Err = AntlrErrHandler(
      structure Tok = Tok
      structure Lex = Lex)

(* replace functor with inline structure for better optimization
    structure EBNF = AntlrEBNF(
      struct
	type strm = Err.wstream
	val getSpan = Err.getSpan
      end)
*)
    structure EBNF =
      struct
	fun optional (pred, parse, strm) =
	      if pred strm
		then let
		  val (y, span, strm') = parse strm
		  in
		    (SOME y, span, strm')
		  end
		else (NONE, Err.getSpan strm, strm)

	fun closure (pred, parse, strm) = let
	      fun iter (strm, (left, right), ys) =
		    if pred strm
		      then let
			val (y, (_, right'), strm') = parse strm
			in iter (strm', (left, right'), y::ys)
			end
		      else (List.rev ys, (left, right), strm)
	      in
		iter (strm, Err.getSpan strm, [])
	      end

	fun posclos (pred, parse, strm) = let
	      val (y, (left, _), strm') = parse strm
	      val (ys, (_, right), strm'') = closure (pred, parse, strm')
	      in
		(y::ys, (left, right), strm'')
	      end
      end

    fun mk lexFn = let
fun getS() = {}
fun putS{} = ()
fun unwrap (ret, strm, repairs) = (ret, strm, repairs)
        val (eh, lex) = Err.mkErrHandler {get = getS, put = putS}
	fun fail() = Err.failure eh
	fun tryProds (strm, prods) = let
	  fun try [] = fail()
	    | try (prod :: prods) =
	        (Err.whileDisabled eh (fn() => prod strm))
		handle Err.ParseError => try (prods)
          in try prods end
fun matchID strm = (case (lex(strm))
 of (Tok.ID(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchLEFT_PAREN strm = (case (lex(strm))
 of (Tok.LEFT_PAREN, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchRIGHT_PAREN strm = (case (lex(strm))
 of (Tok.RIGHT_PAREN, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchEOF strm = (case (lex(strm))
 of (Tok.EOF, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))

val (file_NT) = 
let
fun component_NT (env_RES) (strm) = let
      val (ID_RES, ID_SPAN, strm') = matchID(strm)
      val FULL_SPAN = (#1(ID_SPAN), #2(ID_SPAN))
      in
        (UserCode.component_PROD_1_ACT (ID_RES, env_RES, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun file_NT (env_RES) (strm) = let
      fun file_PROD_1_SUBRULE_1_NT (strm) = let
            val (component_RES, component_SPAN, strm') = (component_NT (UserCode.ARGS_3 (env_RES)))(strm)
            val FULL_SPAN = (#1(component_SPAN), #2(component_SPAN))
            in
              ((component_RES), FULL_SPAN, strm')
            end
      fun file_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.ID(_), _, strm') => true
              | _ => false
            (* end case *))
      val (component_RES, component_SPAN, strm') = EBNF.posclos(file_PROD_1_SUBRULE_1_PRED, file_PROD_1_SUBRULE_1_NT, strm)
      val FULL_SPAN = (#1(component_SPAN), #2(component_SPAN))
      in
        (UserCode.file_PROD_1_ACT (component_RES, env_RES, component_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
in
  (file_NT)
end
val file_NT =  fn x => fn s => unwrap (Err.launch (eh, lexFn, file_NT x , true) s)

in (file_NT) end
  in
fun parse lexFn  x s = let val (file_NT) = mk lexFn in file_NT x s end

  end

end
