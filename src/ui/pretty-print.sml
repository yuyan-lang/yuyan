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

  fun show_intlist (x : int list) : string =
  String.concatWith "," (map Int.toString x)
  fun show_strlist (x : UTF8String.t list) : string =
  String.concatWith ", " (map UTF8String.toString x)
  fun show_sttrlist (x : StructureName.t list) : string =
  String.concatWith "->" (map StructureName.toStringPlain x)
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
  fun show_ecpops (x) = String.concatWith ", \n" (map (fn (sname, _, opers) => 
  StructureName.toStringPlain sname  ^ ": "^ String.concatWith ", "  (map show_op opers)) x)

 fun show_mixedstrchar(u : MixedStr.mixedchar) : string = 
    let 
    open MixedStr
    in
    case  u of 
    UnparsedExpression s => "(UNPARSED(EXPR):" ^ show_mixedstr s ^ ")"
    | UnparsedDeclaration l => "{UNPARSED(DECL):" ^ String.concatWith ";\n " (map (fn x => show_mixedstr x) l) ^ "}\n"
    | Name t => "(NAME:" ^ UTF8String.toString t ^ ")"
    | Literal t => "[LITERAL(size="^ Int.toString (length t) ^"):" ^ UTF8String.toString t ^ ")"
    (* | ParsedExpression e  => "(PARSED(EXPR):" ^ show_opast e ^ ")"
    | ParsedDeclaration d => "(PARSED(DECL):" ^ show_typecheckingSig d ^ ")" *)
    | SChar t => UTF8Char.toString t
    end
    and show_mixedstr(u : MixedStr.t ) : string = String.concat (map show_mixedstrchar u)
    and show_mixedstrs(u : MixedStr.t list ) : string = "[" ^ String.concatWith ";\n" (map show_mixedstr u) ^ "]"

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
    | StringLiteral s => "StringLiteral " ^ UTF8String.toString s
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
      | OpStrLiteral s => "[LITERAL(size="^ Int.toString (length s) ^"):" ^ UTF8String.toString s ^ "]"
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
  | PStructure(v, name, ebody) => (if v then "public" else "private") ^
    " structure " ^ UTF8String.toString name ^ " = " ^ show_mixedstrs ebody
  | POpenStructure(name) => "open " ^ StructureName.toStringPlain name ^ "" 
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
val se = show_typecheckingRExpr
val ss = UTF8String.toString
val sst =StructureName.toStringPlain
in case x of
 TypeVar t => sst t
                    | UnitType => "1"
                    | Prod l => "(" ^ String.concatWith "* " (map (fn (lbl, t) => ss lbl ^ ": " ^ st t) l) ^ ")"
                    | NullType => "0"
                    | Sum l =>  "(" ^ String.concatWith "+ " (map (fn (lbl, t) => ss lbl ^ ": " ^ st t) l) ^ ")"
                    | Func (t1, t2) => "(" ^ st t1 ^ " -> " ^ st t2 ^ ")"
                    | Forall(t1, t2) => "(∀" ^ ss t1 ^ " . " ^ st t2 ^")" 
                    | Exists (t1, t2) => "(∃" ^ ss t1 ^ " . " ^ st t2 ^")" 
                    | Rho (t1, t2) => "(ρ" ^ ss t1 ^ " . " ^ st t2 ^")" 
                    | BuiltinType (BIString) => "(string)" 
                    | BuiltinType (BIBool) => "(bool)" 
                    | BuiltinType (BIInt) => "(int)" 
                    | BuiltinType (BIReal) => "(real)" 
end


and show_typecheckingRExpr x = let
open TypeCheckingAST
val st = show_typecheckingType
val se = show_typecheckingRExpr
val ss = UTF8String.toString
val sst =StructureName.toStringPlain
in case x of
RExprVar v => sst v
                    | RUnitExpr => "⟨⟩"
                    | RTuple (l) => "⟨"^ String.concatWith ", " (map se l) ^ "⟩"
                    | RProj (e, lbl) => "(" ^ se e ^ "." ^ ss lbl ^ ")"
                    | RInj  ( lbl,e) => "(" ^ ss lbl ^ "." ^ se e ^ ")"
                    | RCase (e, l)=>"(case "^ se e ^ " of {"^ String.concatWith "; " (map (fn (lbl, x, e) => ss lbl ^ ". " ^ ss x ^ " => " ^ se e) l) ^ "})"
                    | RLam (x, e) => "(λ" ^ ss x ^ "." ^ se e ^ ")"
                    | RLamWithType (t, x, e) => "(λ" ^ ss x ^ ":" ^ st t ^ "." ^ se e ^ ")"
                    | RApp (e1, e2)=> "ap("^ se e1 ^ ", "^ se e2 ^")"
                    | RTAbs (x, e) => "(Λ" ^ ss x ^ "." ^ se e ^ ")"
                    | RTApp (e1, e2)=> "("^ se e1 ^ " ["^ st e2 ^"])"
                    | RPack (t, e)=> "pack("^ st t ^ ", "^ se e ^")"
                    | ROpen (e, (t, x, e2))=> "open(" ^se e ^ "; "^ ss t ^ ". "^ ss x ^ ". " ^ se e2 ^"])"
                    | RFold (e) => "fold(" ^ se e ^")"
                    | RUnfold (e) => "unfold("^  se e ^")"
                    | RFix (x, e) => "(fix " ^ ss x ^ "." ^   se e ^")"
                    | RStringLiteral l => "\"" ^ ss l ^"\""
                    | RLetIn (s, e) => "(let " ^ show_typecheckingRSig s ^ " in "^  se e  ^" end"
                end

and show_typecheckingRDecl x = let
open TypeCheckingAST
in case x of 
    RTypeMacro(tname, tbody) => "type "^UTF8String.toString tname ^ " = " ^show_typecheckingType tbody
  | RTermTypeJudgment(ename, tbody) => UTF8String.toString ename ^ " : " ^ show_typecheckingType tbody
  | RTermMacro(ename, ebody) => "#define " ^ UTF8String.toString ename ^ " = " ^ show_typecheckingRExpr ebody
  | RTermDefinition(ename, ebody) => UTF8String.toString  ename ^ " = " ^ show_typecheckingRExpr  ebody
  | RDirectExpr(ebody) => "/* eval */ " ^ show_typecheckingRExpr ebody ^ "/* end eval */ " 
  | RStructure(v, name, ebody) => (if v then "public" else "private") ^
    " structure " ^ UTF8String.toString name ^ " = {" ^ show_typecheckingRSig ebody ^ "}"
  | ROpenStructure(name) => "open " ^ StructureName.toStringPlain name ^ "" 
  end

and show_typecheckingRSig x = let
in
          String.concatWith "。\n " (map show_typecheckingRDecl x) ^ "\n"
end
fun show_typecheckingCExpr x =  
let
open TypeCheckingAST
val st = show_typecheckingType
val se = show_typecheckingCExpr
val ss = UTF8String.toString
val sst =StructureName.toStringPlain
fun cst t = "⟦" ^ st t ^ "⟧"
in case x of
                      CExprVar v => sst v
                    | CUnitExpr => "⟨⟩"
                    | CTuple (l,t) => "⟨"^ String.concatWith ", " (map se l) ^ "⟩" ^ cst t
                    | CProj (e, lbl, t) => "(" ^ se e ^ cst t ^ "." ^ ss lbl ^ ")"
                    | CInj  ( lbl,e, t) => "(" ^ ss lbl ^ "." ^ se e ^ ")" ^ cst t
                    | CCase ((ts, e), l, t)=>"(case "^ se e ^ cst ts ^ " of {"^ String.concatWith "; " (map (fn (lbl, x, e) => ss lbl ^ ". " ^ ss x ^ " => " ^ se e) l) ^ "})" ^ cst t
                    | CLam (x, e, t) => "(λ" ^ ss x ^ "." ^ se e ^ ")" ^ cst t
                    | CApp (e1, e2, t)=> "ap("^ se e1 ^ cst t^ ", "^ se e2 ^")"
                    | CTAbs (x, e, t) => "(Λ" ^ ss x ^ "." ^ se e ^ ")" ^ cst t 
                    | CTApp (e1, e2, ft)=> "("^ se e1 ^ cst ft^ " ["^ st e2 ^"])"
                    | CPack (t, e, et)=> "pack("^ st t ^ ", "^ se e ^")" ^ cst et
                    | COpen ((et, e), (t, x, e2), rt)=> "open(" ^se e ^ cst et ^ "; "^ ss t ^ ". "^ ss x ^ ". " ^ se e2 ^"])" ^ cst rt
                    | CFold (e, t) => "fold(" ^ se e ^")" ^ cst t
                    | CUnfold (e, rhot) => "unfold("^  se e ^ cst rhot ^")"
                    | CFix (x, e, t) => "(fix " ^ ss x ^ "." ^   se e ^")" ^ cst t
                    | CStringLiteral l => "\"" ^ ss l ^"\""
                    | CLetIn (s, e, t) => "(let " ^ show_typecheckingCSig s ^ " in "^  se e  ^ cst t ^" end" 
                end
and show_typecheckingCDecl x = let
open TypeCheckingAST
in case x of 
    CTermDefinition(ename, ebody) => StructureName.toStringPlain ename ^ " = " ^ show_typecheckingCExpr  ebody
  | CDirectExpr(ebody) => "/* eval */ " ^ show_typecheckingCExpr ebody ^ "/* end eval */ " 
  end


and show_typecheckingCSig x = let
in
          String.concatWith "。\n " (map show_typecheckingCDecl x) ^ "\n"
end
fun show_source_location ((fname, line, col) : SourceLocation.t) = "[" ^ Int.toString (line + 1) ^ ", "^ Int.toString (col + 1) ^ "]"
fun show_source_range (SourceRange.StartEnd(fname, ls, cs,le,ce ) : SourceRange.t) = 
  "[" ^ Int.toString (ls+1) ^ ", "^ Int.toString (cs+1) ^ "," ^ Int.toString (le+1) ^ ", "^Int.toString (ce+1) ^"]"
fun show_utf8char (UTF8Char.UTF8Char(c, loc)) = UTF8.implode [c] ^ (case loc of SOME loc => show_source_location(loc) | NONE => "[NOLOC]")
fun show_utf8string x = String.concatWith "+" (map show_utf8char x)
fun show_utf8strings x = String.concatWith ", " (map UTF8String.toString x)
fun show_token (CompilationTokens.Token(range, str,  _)) = UTF8String.toString str ^ " : " ^ show_source_range range
fun show_tokens x = String.concatWith ", " (map show_token x) ^ "\n"
fun show_typecheckingpassmappping x = let
open TypeCheckingASTOps
in
  case x of
    TermTypeJ(e, t,_) => StructureName.toStringPlain e ^ " : " ^ show_typecheckingType t
    | TypeDef(s, t, _) => StructureName.toStringPlain s ^ " = " ^ show_typecheckingType t
end
fun show_typecheckingpassctx x = let
open TypeCheckingASTOps
in
case x of 
  Context(curSName, curVis, m) => (if curVis then "public " else "private ") ^
  "structure " ^ StructureName.toStringPlain curSName ^ " {"  ^
          String.concatWith ",\n " (map show_typecheckingpassmappping m) ^ "}\n"
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
        | PKBuiltinValue (KbvBool t) => "builtin:bool(" ^  Bool.toString t  ^ ")"
        | PKBuiltinValue (KbvReal t) => "builtin:real(" ^  Real.toString t  ^ ")"
        | PKBuiltinValue (KbvInt t) => "builtin:int(" ^  Int.toString t  ^ ")"
        | PKBuiltinValue (KbvString t) => "builtin:string(" ^  UTF8String.toString t  ^ ")"
        | PKBuiltinValue (KbvFunc (l, f)) => "builtin:func(" ^  UTF8String.toString l  ^ ", ---)"

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
      | PKAppWithEvaledFun((x,f), c2) => "apfun (" ^ show_pkvalue (PKAbs(x,f))  ^ ", " ^ show_pkcomputation c2 ^ ")"
      | PKRet(v) => "ret (" ^ show_pkvalue v ^ ")"
      | PKFix(id, c) => "(fix " ^ Int.toString id ^ "." ^ show_pkcomputation c ^ ")"
      end


   (* fun show_pkmachine x = let
   open PersistentKMachine
in 
  case x of 
    Run (l, c) => "RUN : "^ Int.toString (length l) ^ " : " ^ show_pkcomputation c
    | NormalRet (l, c) => "RET : "^ Int.toString (length l) ^ " : " ^ show_pkvalue c
    end
 
   *)


fun show_cpsvalue  (CPSAst.CPSVar(i)) = Int.toString i
fun show_cpsbuiltin (e : CPSAst.cpsBuiltinValue) = 
let open CPSAst
in
case  e of
        CPSBvInt i => Int.toString i
        | CPSBvBool  b=> Bool.toString b
        | CPSBvString l=> UTF8String.toString l
        | CPSBvReal r => Real.toString r
      end
    
fun show_cpscomputation  (c : CPSAst.cpscomputation) : string = 
let 
open CPSAst
fun show_cpskont (v, cpscomp ) = " ===> \\k:"^ Int.toString v ^ "⟦" ^ show_cpscomputation cpscomp ^"⟧"
val sv = show_cpsvalue
val sk = show_cpskont
val si = Int.toString
val sc = show_cpscomputation
in
case c of
              CPSUnit(k) => "()" ^ sk k
            | CPSProj(v, i, k) => "(" ^ sv v ^ " . " ^ si i ^ ")" ^ sk k
            | CPSCases(v, l) => "(case "  ^ sv v ^ " of {" ^ 
    String.concatWith "; " (map (fn (i, c) => Int.toString i ^ " => " ^ sc c) l)
    ^ "}"
            | CPSUnfold(v, k) => "unfold (" ^ sv v ^ ")" ^ sk k
            | CPSApp(a, (b, c)) => "ap("^ sv a ^ ",("^ sv b ^ ", " ^ sv c^"))"
            | CPSAppSingle (a,b)=> "ap1("^ sv a ^ ","^ sv b ^")"
            (* | CPSFix((f, a, c1), k) => "(fix " ^ si f ^ ", " ^ si a ^ " . " ^ sc c1 ^ ")" ^ sk k *)
            | CPSTuple(l, k) => "[" ^ String.concatWith ", " (map sv l) ^ "]" ^ sk k
            | CPSInj(l, i, kv, k) => "(" ^ UTF8String.toString l ^ ")" ^ Int.toString i ^ "⋅" ^ sv kv ^ sk k
            | CPSFold(v, k) => "fold (" ^ sv v ^ ")" ^ sk k
            | CPSAbsSingle((i, c), k) => "(λ1" ^ Int.toString i ^ "." ^ sc c ^ ")" ^ sk k
            | CPSAbs((i,ak, c), k) => "(λ" ^ Int.toString i ^ ", "^ si ak ^ "." ^ sc c ^ ")" ^ sk k
            | CPSDone (CPSVar i)(* signals return *) => "DONE[RESULT IS STORED IN "^ Int.toString i ^ "]"
            | CPSBuiltinValue(bv, k) => show_cpsbuiltin bv ^ sk k


end
end