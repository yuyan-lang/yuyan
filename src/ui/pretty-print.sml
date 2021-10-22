structure PrettyPrint 
 =
struct


  (* fun show_rawast (x : RawAST.RawAST) : string = let 
    open RawAST
    in case x of 
       RawID s => UTF8Char.toString s
      end
  fun show_rawastsPlain (l : RawAST.RawAST list) =  (String.concat (map show_rawast l))
  fun show_rawasts (l : RawAST.RawAST list) = "("^ (String.concatWith ", " (map show_rawast l)) ^")"  *)

  fun show_strlist (x : UTF8String.t list) : string =
  String.concatWith ", " (map UTF8String.toString x)
  fun show_opcomptype (x : Operators.opComponentType) :string = let 
    open Operators
    in
      case x of
        OpCompExpr => UTF8Char.toString underscoreChar
        | OpCompBinding => UTF8Char.toString bindingChar
        | OpCompString s => UTF8String.toString s
    end
      fun show_opcomptypes (x : Operators.opComponentType list) :string = let 
    open Operators
    in
    "[" ^ String.concatWith ", " (map show_opcomptype x) ^ "]"
    end



  fun show_op (x : Operators.operator) = let 
    open Operators
    in case x of
      Operator(p, fix, assoc, comps, uid) =>
      let val baseName = String.concat (map show_opcomptype comps)
      val pstr = Int.toString p
      val assoc = case assoc of LeftAssoc => "l" | RightAssoc => "r" | NoneAssoc => "n"
        val underscoreChar = UTF8Char.toString underscoreChar
        val fullname = case fix of
          Prefix =>  baseName ^ underscoreChar 
          | Infix => underscoreChar ^ baseName ^ underscoreChar 
          | Postfix =>underscoreChar ^ baseName
          | Closed => baseName
      in 
      fullname ^ assoc ^ pstr
        end
    end

 fun show_mixedstrchar(u : MixedStr.mixedchar) : string = 
    let 
    open MixedStr
    in
    case  u of 
    UnparsedExpression s => "(UNPARSED(EXPR):" ^ show_mixedstr s ^ ")"
    | UnparsedDeclaration l => "{UNPARSED(DECL):" ^ String.concatWith ";\n " (map (fn x => show_mixedstr x) l) ^ "}"
    | Name t => "(NAME:" ^ UTF8String.toString t ^ ")"
    | Literal t => "(LITERAL:" ^ UTF8String.toString t ^ ")"
    (* | ParsedExpression e  => "(PARSED(EXPR):" ^ show_opast e ^ ")"
    | ParsedDeclaration d => "(PARSED(DECL):" ^ show_typecheckingSig d ^ ")" *)
    | SChar t => UTF8Char.toString t
    end
    and show_mixedstr(u : MixedStr.t ) : string = String.concat (map show_mixedstrchar u)
    and show_mixedstrs(u : MixedStr.t list ) : string = String.concatWith ";\n" (map show_mixedstr u)

fun show_parseopast x = let 
open ParseAST in 
case x of
    ParseOpAST (r, l) => show_parseRule r ^ "[" ^ (String.concatWith ", " (map show_parseopast l))  ^"]" 
    end

and show_parseRule x = let 
open Operators 
open ParseAST
in
case x of
    OperatorNameComponent (f,oper) => "OperatorNameComponent " ^ UTF8String.toString f ^  " in " ^ show_op oper
    | OperatorInternal oper => "OperatorInternal " ^ show_op oper
    | PrefixNoneAssoc oper=> "PrefixNoneAssoc "^ show_op oper
    | PrefixRightAssoc oper=> "PrefixRightAssoc "^ Int.toString(oper)
    | PostfixNoneAssoc oper=> "PostfixNoneAssoc "^ show_op oper
    | PostfixLeftAssoc oper=> "PostfixLeftAssoc "^ Int.toString(oper)
    | InfixNoneAssoc oper=> "InfixNoneAssoc "^ show_op oper
    | InfixLeftAssoc oper=> "InfixLeftAssoc "^ Int.toString(oper)
    | InfixLeftAssocLeftArrow oper=> "InfixLeftAssocLeftArrow "^ show_op oper
    | InfixRightAssoc oper=> "InfixRightAssoc "^ Int.toString(oper)
    | InfixRightAssocRightArrow oper=> "InfixRightAssocRightArrow "^ show_op oper
    | Many1  => "Many1"
    | EOF  => "EOF"
    | ExpWithEOF  => "ExpWithEOF"
    | UnknownId  => "UnknownId"
    | UnknownIdComp s => "UnknownIdComp "^ UTF8Char.toString s
    | Binding id => "Binding "^ UTF8String.toString id
    | QuotedName s => "QuotedName "^UTF8String.toString s
    | UnparsedDecl l => "UnparsedDecl "^ String.concatWith ", " (map show_mixedstr l)
    | UnparsedExpr l => "UnparsedExpr "^ show_mixedstr l
    | PlaceHolder => "PlaceHolder "
end

  fun show_opast (x : OpAST.OpAST) = let 
    open Operators
    open OpAST
    in case x of 
      OpAST (oper, l) => (show_op oper) ^ "[" ^ String.concatWith ", " (map show_opast l) ^ "]"
      | UnknownOpName s => "?[" ^ UTF8String.toString s ^ "]"
      | NewOpName s => "![" ^ UTF8String.toString s ^ "]"
      | OpUnparsedDecl s => "[DECL:" ^ (String.concatWith "。" (map show_mixedstr s)) ^ "]"
      | OpUnparsedExpr s => "[EXPR:" ^ show_mixedstr s ^ "]"
    end


fun show_preprocessaastJ x = let
open PreprocessingAST
in 
case x of 
   PTypeMacro(tname, tbody) => "type "^ UTF8String.toString tname ^ " = " ^ MixedStr.toString tbody
  | PTermTypeJudgment(ename, tbody) => UTF8String.toString ename ^ " : " ^ MixedStr.toString tbody
  | PTermMacro(ename, ebody) => "#define " ^ UTF8String.toString ename ^ " = " ^ MixedStr.toString ebody
  | PTermDefinition(ename, ebody) => UTF8String.toString  ename ^ " = " ^ MixedStr.toString  ebody
  | POpDeclaration(opName, assoc, pred) => "infix" ^ (case assoc of Operators.NoneAssoc => "n" | Operators.RightAssoc => "r" | Operators.LeftAssoc => "l"
  ) ^ " " ^ UTF8String.toString opName ^ Int.toString pred
  | PDirectExpr(ebody) => "/* eval */ " ^ MixedStr.toString ebody ^ "/* end eval */ " 
  | PComment(ebody) => "/* comment : -- */ "
  end
fun show_preprocessaast x = let
open PreprocessingAST
in 
String.concatWith "\n" (map show_preprocessaastJ x) ^ "\n"
  end

(* fun show_statementast x = let 
open StatementAST
in case x of 
    Leaf => ". /* END */\n"
    | StatementNode (stmt, next) => UTF8String.toString stmt ^ "\n -> "^ show_statementast next
    end *)
fun show_typecheckingType x = let
open TypeCheckingAST
val st = show_typecheckingType
val se = show_typecheckingExpr
val ss = UTF8String.toString
in case x of
 TypeVar t => ss t
                    | UnitType => "1"
                    | Prod l => "(" ^ String.concatWith "* " (map (fn (lbl, t) => ss lbl ^ ": " ^ st t) l) ^ ")"
                    | NullType => "0"
                    | Sum l =>  "(" ^ String.concatWith "+ " (map (fn (lbl, t) => ss lbl ^ ": " ^ st t) l) ^ ")"
                    | Func (t1, t2) => "(" ^ st t1 ^ " -> " ^ st t2 ^ ")"
                    | Forall(t1, t2) => "(∀" ^ ss t1 ^ " . " ^ st t2 ^")" 
                    | Exists (t1, t2) => "(∃" ^ ss t1 ^ " . " ^ st t2 ^")" 
                    | Rho (t1, t2) => "(ρ" ^ ss t1 ^ " . " ^ st t2 ^")" 
end


and show_typecheckingExpr x = let
open TypeCheckingAST
val st = show_typecheckingType
val se = show_typecheckingExpr
val ss = UTF8String.toString
in case x of
ExprVar v => ss v
                    | UnitExpr => "⟨⟩"
                    | Tuple (l) => "⟨"^ String.concatWith ", " (map se l) ^ "⟩"
                    | Proj (e, lbl) => "(" ^ se e ^ "." ^ ss lbl ^ ")"
                    | Inj  ( lbl,e) => "(" ^ ss lbl ^ "." ^ se e ^ ")"
                    | Case (e, l)=>"(case "^ se e ^ " of {"^ String.concatWith "; " (map (fn (lbl, x, e) => ss lbl ^ ". " ^ ss x ^ " => " ^ se e) l) ^ "})"
                    | Lam (x, e) => "(λ" ^ ss x ^ "." ^ se e ^ ")"
                    | LamWithType (t, x, e) => "(λ" ^ ss x ^ ":" ^ st t ^ "." ^ se e ^ ")"
                    | App (e1, e2)=> "ap("^ se e1 ^ ", "^ se e2 ^")"
                    | TAbs (x, e) => "(Λ" ^ ss x ^ "." ^ se e ^ ")"
                    | TApp (e1, e2)=> "("^ se e1 ^ " ["^ st e2 ^"])"
                    | Pack (t, e)=> "pack("^ st t ^ ", "^ se e ^")"
                    | Open (e, (t, x, e2))=> "open(" ^se e ^ "; "^ ss t ^ ". "^ ss x ^ ". " ^ se e2 ^"])"
                    | Fold (e) => "fold(" ^ se e ^")"
                    | Unfold (e) => "unfold("^  se e ^")"
                    | Fix (x, e) => "(fix " ^ ss x ^ "." ^   se e ^")"
                end

fun show_typecheckingDecl x = let
open TypeCheckingAST
in case x of 
   TypeMacro(tname, tbody) => "type "^UTF8String.toString tname ^ " = " ^show_typecheckingType tbody
  | TermTypeJudgment(ename, tbody) => UTF8String.toString ename ^ " : " ^ show_typecheckingType tbody
  | TermMacro(ename, ebody) => "#define " ^ UTF8String.toString ename ^ " = " ^ show_typecheckingExpr ebody
  | TermDefinition(ename, ebody) => UTF8String.toString  ename ^ " = " ^ show_typecheckingExpr  ebody
  | DirectExpr(ebody) => "/* eval */ " ^ show_typecheckingExpr ebody ^ "/* end eval */ " 
  end

fun show_typecheckingSig x = let
in
          String.concatWith "。\n " (map show_typecheckingDecl x) ^ "\n"
end

fun show_pkvalue x =let
open PersistentKMachine
in
      case x of
        PKUnit => "()"
        | PKVar i => Int.toString i
        | PKTuple l => "[" ^ String.concatWith ", " (map show_pkvalue l) ^ "]"
        | PKInj (l, i, kv) => "(" ^ UTF8String.toString l ^ ")" ^ Int.toString i ^ "⋅" ^ show_pkvalue kv
        | PKFold e => "fold (" ^ show_pkvalue e ^ ")"
        | PKAbs (i, c) => "(λ" ^ Int.toString i ^ "." ^ show_pkcomputation c ^ ")"
        | PKComp (c) => "comp(" ^  show_pkcomputation c ^ ")"

end
and show_pkcomputation x = let
open PersistentKMachine
in
    case x of
      PKProj(k, i) => show_pkcomputation k ^ " . " ^ Int.toString i
      | PKCases(e, l) => "(case "  ^ show_pkcomputation e ^ " of {" ^ 
      String.concatWith "; " (map (fn (i, c) => Int.toString i ^ " => " ^ show_pkcomputation c) l)
      ^ "}"
      | PKUnfold(e) => "unfold (" ^ show_pkcomputation e ^ ")"
      | PKApp(c1, c2) => "ap (" ^ show_pkcomputation c1 ^ ", " ^ show_pkcomputation c2 ^ ")"
      | PKRet(v) => "ret (" ^ show_pkvalue v ^ ")"
      | PKFix(id, c) => "(fix " ^ Int.toString id ^ "." ^ show_pkcomputation c ^ ")"
      end


   fun show_kmachine x = let
   open KMachine
in 
  case x of 
    Run (l, c) => "RUN : "^ Int.toString (length l) ^ " : " ^ show_pkcomputation (PersistentKMachine.fromKComp c)
    | NormalRet (l, c) => "RET : "^ Int.toString (length l) ^ " : " ^ show_pkvalue (PersistentKMachine.fromKValue c)
    end
 
  
end