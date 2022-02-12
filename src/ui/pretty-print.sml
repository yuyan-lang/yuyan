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
  fun show_plainstrlist (x : string list) : string =
  String.concatWith ", " x
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
  StructureName.toStringPlain sname  ^ ": ["^ String.concatWith ", "  (map show_op opers) ^ "]" ) x)

 fun show_mixedstrchar(u : MixedStr.mixedchar) : string = 
    let 
    open MixedStr
    in
    case  u of 
    UnparsedExpression(s,qi) => "(UNPARSED(EXPR):" ^ show_mixedstr s ^ ")"
    | UnparsedDeclaration(l, qi) => "{UNPARSED(DECL):" ^ String.concatWith ";\n " (map (fn (x,e) => show_mixedstr x) l) ^ "}\n"
    | Name(t, qi) => "(NAME:" ^ UTF8String.toString t ^ ")"
    | Literal(t, qi) => "[LITERAL(size="^ Int.toString (length t) ^"):" ^ UTF8String.toString t ^ ")"
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
    | QuotedBinding (id, qi) => "Binding "^ UTF8String.toString id
    | QuotedName (s, qi) => "QuotedName "^UTF8String.toString s
    | UnparsedDecl (l, qi) => "UnparsedDecl "^ String.concatWith ", " (map show_mixedstr (map (#1)l))
    | UnparsedExpr (l, qi) => "UnparsedExpr "^ show_mixedstr l
    | PlaceHolder => "PlaceHolder "
    | StringLiteral (s, qi) => "StringLiteral " ^ UTF8String.toString s
end

  fun show_opast (x : OpAST.OpAST) = let 
    open Operators
    open OpAST
    in case x of 
      OpAST (oper, l) => (show_op oper) ^ "[" ^ String.concatWith ", " (map show_opast l) ^ "]"
      | UnknownOpName s => "?[" ^ UTF8String.toString s ^ "]"
      | NewOpName s => "![" ^ UTF8String.toString s ^ "]"
      | OpUnparsedDecl(s, qi) => "[DECL:" ^ (String.concatWith "。" (map show_mixedstr (map (#1) s))) ^ "]"
      | OpUnparsedExpr (s, qi) => "[EXPR:" ^ show_mixedstr s ^ "]"
      | OpStrLiteral (s, qi) => "[LITERAL(size="^ Int.toString (length s) ^"):" ^ UTF8String.toString s ^ "]"
      | _ => "<other opast>"
    end


fun show_preprocessaastJ x = let
open OpAST
open PreprocessingAST
in 
case x of 
   PTypeMacro(tname, tbody, soi) => "type "^ UTF8String.toString tname ^ " = " ^ show_opast tbody
  | PTermTypeJudgment(ename, tbody, soi) => UTF8String.toString ename ^ " : " ^ show_opast tbody
  | PTermMacro(ename, ebody, soi) => "#define " ^ UTF8String.toString ename ^ " = " ^ show_opast ebody
  | PTermDefinition(ename, ebody, soi) => UTF8String.toString  ename ^ " = " ^ show_opast  ebody
  | POpDeclaration(opName, assoc, pred, soi) => "infix" ^ (case assoc of Operators.NoneAssoc => "n" | Operators.RightAssoc => "r" | Operators.LeftAssoc => "l"
  ) ^ " " ^ UTF8String.toString opName ^ Int.toString pred
  | PDirectExpr(ebody) => "/* eval */ " ^ show_opast ebody ^ "/* end eval */ " 
  | PStructure(v, name, ebody, soi) => (if v then "public" else "private") ^
    " structure " ^ UTF8String.toString name ^ " = " ^ show_opast ebody
  | POpenStructure(name, soi) => "open " ^ show_opast name ^ "" 
  | PImportStructure(name, path, soi) => "import " ^ show_opast name ^ "" 
  | PReExportStructure(name, soi) => "reexport " ^ show_opast name ^ "" 
  | PComment(ebody, soi) => "/* comment : -- */ "
  | PEmptyDecl => "/* empty */"
  end
and show_preprocessaast x = let
open PreprocessingAST
in 
"{" ^ String.concatWith "\n" (map show_preprocessaastJ x) ^ "\n" ^ "}"
  end

fun show_typecheckingbuiltinfunc x = 
let 
open TypeCheckingAST
in
case x of 
      BFCallCC  => "bf_callcc"
      | BFNewDynClsfdValueWithString => "bf_newclsfd"
      | BFRaise => "bf_raise"
      | BFHandle => "bf_handle"
      | BFIntEq => "bf_int_eq"
      | BFIntSub => "bf_int_sub"
end

fun show_builtintype x = let 
open TypeCheckingAST
in
case x of
                     (BIString) => "(string)" 
                    | (BIBool) => "(bool)" 
                    | (BIInt) => "(int)" 
                    | (BIReal) => "(real)" 
                    | (BIDynClsfd) => "(clsfd)" 
                    | BIForeignType i => "(foreign_type:" ^ Int.toString i ^ ")"
                    end


(* fun show_statementast x = let 
open StatementAST
in case x of 
    Leaf => ". /* END */\n"
    | StatementNode (stmt, next) => UTF8String.toString stmt ^ "\n -> "^ show_statementast next
    end *)
fun show_typecheckingRType x = let
open TypeCheckingAST
val st = show_typecheckingRType
val se = show_typecheckingRExpr
val ss = UTF8String.toString
val sst =StructureName.toStringPlain
in case x of
                      RTypeVar t => sst t
                    | RUnitType => "1"
                    | RProd l => "(" ^ String.concatWith "* " (map (fn (lbl, t) => ss lbl ^ ": " ^ st t) l) ^ ")"
                    | RLazyProd l => "(" ^ String.concatWith "*(lazy) " (map (fn (lbl, t) => ss lbl ^ ": " ^ st t) l) ^ ")"
                    | RNullType => "0"
                    | RSum l =>  "(" ^ String.concatWith "+ " (map (fn (lbl, t) => ss lbl ^ ": " ^ st t) l) ^ ")"
                    | RFunc (t1, t2) => "(" ^ st t1 ^ " -> " ^ st t2 ^ ")"
                    | RTypeInst (t1, t2) => "(INST[" ^ st t1 ^ ", " ^ st t2 ^ "])"
                    | RForall(t1, t2) => "(∀" ^ ss t1 ^ " . " ^ st t2 ^")" 
                    | RExists (t1, t2) => "(∃" ^ ss t1 ^ " . " ^ st t2 ^")" 
                    | RRho (t1, t2) => "(ρ" ^ ss t1 ^ " . " ^ st t2 ^")" 
                    | RBuiltinType (bi) => show_builtintype bi
  
end


and show_typecheckingRExpr x = let
open TypeCheckingAST
val st = show_typecheckingRType
val se = show_typecheckingRExpr
val ss = UTF8String.toString
val sst =StructureName.toStringPlain
in case x of
RExprVar v => sst v
                    | RUnitExpr(soi) => "⟨⟩"
                    | RTuple (l, soi) => "⟨"^ String.concatWith ", " (map se l) ^ "⟩"
                    | RLazyTuple (l, soi) => "⟨(lazy>)"^ String.concatWith ", " (map se l) ^ "(<lazy)⟩"
                    | RProj (e, lbl, soi) => "(" ^ se e ^ "." ^ ss lbl ^ ")"
                    | RLazyProj (e, lbl, soi) => "(" ^ se e ^ ".(lazy) " ^ ss lbl ^ ")"
                    | RIfThenElse (e, tcase, fcase, soi) => "(if " ^ se e ^ " then " ^ se tcase ^ " else " ^ se fcase ^ ")"
                    | RInj  ( lbl,e, soi) => "(" ^ ss lbl ^ "." ^ se e ^ ")"
                    | RCase (e, l, soi)=>"(case "^ se e ^ " of {"^ String.concatWith "; " (map (fn (lbl, x, e) => ss lbl ^ ". " ^ ss x ^ " => " ^ se e) l) ^ "})"
                    | RLam (x, e, soi) => "(λ" ^ ss x ^ "." ^ se e ^ ")"
                    | RLamWithType (t, x, e, soi) => "(λ" ^ ss x ^ ":" ^ st t ^ "." ^ se e ^ ")"
                    | RApp (e1, e2, soi)=> "ap("^ se e1 ^ ", "^ se e2 ^")"
                    | RTAbs (x, e, soi) => "(Λ" ^ ss x ^ "." ^ se e ^ ")"
                    | RTApp (e1, e2, soi)=> "("^ se e1 ^ " ["^ st e2 ^"])"
                    | RPack (t, e, soi)=> "pack("^ st t ^ ", "^ se e ^")"
                    | ROpen (e, (t, x, e2), soi)=> "open(" ^se e ^ "; "^ ss t ^ ". "^ ss x ^ ". " ^ se e2 ^"])"
                    | RFold (e, soi) => "fold(" ^ se e ^")"
                    | RUnfold (e, soi) => "unfold("^  se e ^")"
                    | RFix (x, e, soi) => "(fix " ^ ss x ^ "." ^   se e ^")"
                    | RStringLiteral (l, soi) => "\"" ^ ss l ^"\""
                    | RIntConstant (l, soi) => "(" ^ Int.toString l ^")"
                    | RRealConstant (l, soi) => "(" ^ Real.toString l ^")"
                    | RBoolConstant (b, soi) => "(" ^ Bool.toString b ^")"
                    | RLetIn (s, e, soi) => "(let " ^ show_typecheckingRSig s ^ " in "^  se e  ^" end"
                    | RFfiCCall (s, e, soi) => "(ccall \"" ^ se e ^ "\" args "^  se e  ^")"
                    | RBuiltinFunc(f, s) => show_typecheckingbuiltinfunc f
                    | RSeqComp(e1, e2, soi) => "(" ^ se e1 ^ "; " ^ se e2 ^ ")"
                end

and show_typecheckingRDecl x = let
open TypeCheckingAST
in case x of 
    RTypeMacro(tname, tbody) => "type "^UTF8String.toString tname ^ " = " ^show_typecheckingRType tbody
  | RTermTypeJudgment(ename, tbody) => UTF8String.toString ename ^ " : " ^ show_typecheckingRType tbody
  | RTermMacro(ename, ebody) => "#define " ^ UTF8String.toString ename ^ " = " ^ show_typecheckingRExpr ebody
  | RTermDefinition(ename, ebody) => UTF8String.toString  ename ^ " = " ^ show_typecheckingRExpr  ebody
  | RDirectExpr(ebody) => "/* eval */ " ^ show_typecheckingRExpr ebody ^ "/* end eval */ " 
  | RStructure(v, name, ebody) => (if v then "public" else "private") ^
    " structure " ^ UTF8String.toString name ^ " = {" ^ show_typecheckingRSig ebody ^ "}"
  | ROpenStructure(name) => "open " ^ StructureName.toStringPlain name ^ "" 
  | RReExportStructure(name) => "reexport " ^ StructureName.toStringPlain name ^ "" 
  | RImportStructure(name, fp) => "import " ^ StructureName.toStringPlain name ^ "" 
  end

and show_typecheckingRSig x = let
in
          String.concatWith "。\n " (map show_typecheckingRDecl x) ^ "\n"
end
fun show_typecheckingCType x = let
open TypeCheckingAST
val st = show_typecheckingCType
val se = show_typecheckingRExpr
val ss = UTF8String.toString
val sst =StructureName.toStringPlain
in case x of
                      CTypeVar t => sst t
                    | CUnitType => "1"
                    | CProd l => "(" ^ String.concatWith "* " (map (fn (lbl, t) => ss lbl ^ ": " ^ st t) l) ^ ")"
                    | CLazyProd l => "(" ^ String.concatWith "*(lazy) " (map (fn (lbl, t) => ss lbl ^ ": " ^ st t) l) ^ ")"
                    | CNullType => "0"
                    | CSum l =>  "(" ^ String.concatWith "+ " (map (fn (lbl, t) => ss lbl ^ ": " ^ st t) l) ^ ")"
                    | CFunc (t1, t2) => "(" ^ st t1 ^ " -> " ^ st t2 ^ ")"
                    | CTypeInst (t1, t2) => "(INST[" ^ st t1 ^ ", " ^ st t2 ^ "])"
                    | CForall(t1, t2) => "(∀" ^ ss t1 ^ " . " ^ st t2 ^")" 
                    | CExists (t1, t2) => "(∃" ^ ss t1 ^ " . " ^ st t2 ^")" 
                    | CRho (t1, t2) => "(ρ" ^ ss t1 ^ " . " ^ st t2 ^")" 
                    | CBuiltinType (bi) => show_builtintype bi
  
end

and show_typecheckingCExpr x =  
let
open TypeCheckingAST
val st = show_typecheckingCType
val se = show_typecheckingCExpr
val ss = UTF8String.toString
val sst =StructureName.toStringPlain
fun cst t = "⟦" ^ st t ^ "⟧"
in case x of
                      CExprVar v => sst v
                    | CUnitExpr => "⟨⟩"
                    | CTuple (l,t) => "⟨"^ String.concatWith ", " (map se l) ^ "⟩" ^ cst t
                    | CLazyTuple (l,t) => "⟨"^ String.concatWith ",(lazy) " (map se l) ^ "⟩" ^ cst t
                    | CProj (e, lbl, t) => "(" ^ se e ^ cst t ^ "." ^ ss lbl ^ ")"
                    | CLazyProj (e, lbl, t) => "(" ^ se e ^ cst t ^ ".(lazy) " ^ ss lbl ^ ")"
                    | CIfThenElse (e, tcase, fcase ) => "(if " ^ se e ^ " then " ^ se tcase ^ " else " ^ se fcase ^ ")"
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
                    | CIntConstant l => "(" ^ Int.toString l ^")"
                    | CRealConstant l => "(" ^ Real.toString l ^")"
                    | CBoolConstant b => "(" ^ Bool.toString b ^")"
                    | CLetIn (s, e, t) => "(let " ^ show_typecheckingCSig s ^ " in "^  se e  ^ cst t ^" end" 
                    | CFfiCCall(fname, args) => 
                    "(ccall \"" ^ ss fname ^ "\" args ⟨"^  String.concatWith ", " (map sst args) ^"⟩)"
                    | CBuiltinFunc(f) => show_typecheckingbuiltinfunc f
                    | CSeqComp(e1, e2, t1, t2) => "(" ^ se e1 ^ "; " ^ se e2 ^ ")"
                end
and show_typecheckingCDecl x = let
open TypeCheckingAST
in case x of 
    CTypeMacro(tname, tbody) => "type " ^ StructureName.toStringPlain tname ^ " = " ^ show_typecheckingCType  tbody
  | CTermDefinition(ename, ebody, tp) => StructureName.toStringPlain ename ^ " = " ^ show_typecheckingCExpr  ebody
  | CDirectExpr(ebody, tp) => "/* eval */ " ^ show_typecheckingCExpr ebody ^ "/* end eval */ " 
  | CImport(name, fp) => "import " ^ StructureName.toStringPlain name  ^ ""
  end


and show_typecheckingCSig x = let
in
          "[" ^ String.concatWith "。\n " (map show_typecheckingCDecl x) ^ "]\n" 
end
fun show_source_location ((fname, line, col) : SourceLocation.t) = "[" ^ Int.toString (line + 1) ^ ", "^ Int.toString (col + 1) ^ "]"
fun show_source_range (SourceRange.StartEnd(fname, ls, cs,le,ce ) : SourceRange.t) = 
  "[" ^ Int.toString (ls+1) ^ ", "^ Int.toString (cs+1) ^ "," ^ Int.toString (le+1) ^ ", "^Int.toString (ce+1) ^"]"
fun show_utf8char (UTF8Char.UTF8Char(c, loc)) = UTF8.implode [c] ^ (case loc of SOME loc => show_source_location(loc) | NONE => "[NOLOC]")
fun show_utf8string x = String.concatWith "+" (map show_utf8char x)
fun show_utf8strings x = String.concatWith ", " (map show_utf8string x)
fun show_token (CompilationStructure.Token(str,  _)) = UTF8String.toString str ^ " : " ^ show_source_range (UTF8String.getSourceRange str)
fun show_tokens x = String.concatWith ", " (map show_token x) ^ "\n"
fun show_typecheckingpassmappping x = let
open TypeCheckingAST
in
  case x of
    TermTypeJ(e, t,_) => StructureName.toStringPlain e ^ " : " ^ show_typecheckingRType t
    | TypeDef(s, t, _) => StructureName.toStringPlain s ^ " = " ^ show_typecheckingRType t
end
fun show_typecheckingpassctx x = let
open TypeCheckingAST
in
case x of 
  Context(curSName, curVis, m) => (if curVis then "public " else "private ") ^
  "structure " ^ StructureName.toStringPlain curSName ^ " {"  ^
          String.concatWith ",\n " (map show_typecheckingpassmappping m) ^ "}\n"
end



fun show_cpsvar  v = 
    let open CPSAst 
    in case v of 
      CPSVarLocal i =>  "l" ^ Int.toString i
      | CPSVarGlobal i =>  "g" ^ Int.toString i
    end
fun show_cpsvalue  (CPSAst.CPSValueVar(v)) = show_cpsvar v
fun show_cpsbuiltin (e : CPSAst.cpsBuiltinValue) = 
let open CPSAst
val realStr = case  e of
        CPSBvInt i => "(Int)"^Int.toString i
        | CPSBvBool  b=> "(Bool)"^ Bool.toString b
        | CPSBvString l=> "(String)"^UTF8String.toString l
        | CPSBvReal r => "(Real)"^Real.toString r
in 
  "[CPSBuiltin: " ^ realStr ^ "]"
      end
    
    
fun show_cpscomputation  (c : CPSAst.cpscomputation) : string = 
let 
open CPSAst
fun show_cpskont (v, cpscomp ) = " ===> \\k:"^ show_cpsvar v ^ "⟦" ^ show_cpscomputation cpscomp ^"⟧"
val sv = show_cpsvalue
val sk = show_cpskont
val si = Int.toString
val sc = show_cpscomputation
fun sfvs (fvs: int list option) : string = case 
  fvs of SOME fvs => "[FreeVars(" ^ Int.toString (length fvs) ^ "): " ^ String.concatWith ", " (map Int.toString fvs) ^ "]" | NONE => ""
in
case c of
              CPSUnit(k) => "()" ^ sk k
            | CPSProj(v, i, k) => "(" ^ sv v ^ " . " ^ si i ^ ")" ^ sk k
            | CPSCases(v, l) => "(case "  ^ sv v ^ " of {" ^ 
    String.concatWith "; " (map (fn (i, c) => show_cpsvar i ^ " => " ^ sc c) l)
    ^ "}"
            | CPSUnfold(v, k) => "unfold (" ^ sv v ^ ")" ^ sk k
            | CPSApp(a, (b, c)) => "ap("^ sv a ^ ",("^ sv b ^ ", " ^ sv c^"))"
            | CPSAppSingle (a,b)=> "ap1("^ sv a ^ ","^ sv b ^")"
            (* | CPSFix((f, a, c1), k) => "(fix " ^ si f ^ ", " ^ si a ^ " . " ^ sc c1 ^ ")" ^ sk k *)
            | CPSTuple(l, k) => "[" ^ String.concatWith ", " (map sv l) ^ "]" ^ sk k
            | CPSInj(l, i, kv, k) => "(" ^ UTF8String.toString l ^ ")" ^ Int.toString i ^ "⋅" ^ sv kv ^ sk k
            | CPSIfThenElse(v, ct, cf) => "(if " ^ sv v ^ " then " ^ sc ct ^  " else " ^ sc cf ^ ")"
            | CPSFold(v, k) => "fold (" ^ sv v ^ ")" ^ sk k
            | CPSAbsSingle((i, c), fvs, k) => "(λS" ^ si i ^ "." ^ sc c ^ ")"  ^ 
            sfvs fvs ^ sk k
            | CPSAbs((i,ak, c),fvs,  k) => "(λ" ^ si i ^ ", "^ si ak ^ "." ^ sc c ^ ")" 
            ^ sfvs fvs^ sk k
            | CPSDone (CPSValueVar i)(* signals return *) => "DONE[RESULT IS STORED IN "^ show_cpsvar i ^ "]"
            | CPSBuiltinValue(bv, k) => show_cpsbuiltin bv ^ sk k
            | CPSFfiCCall(fname, args, k) => "(ccall \"" ^ UTF8String.toString fname ^
            "\" args [" ^ String.concatWith ", " (map sv args) ^ "])" ^ sk k
            (* | CPSSequence l => "[" ^ String.concatWith ", " (map show_cpscomputation l) ^ "]" *)
            | CPSStore (dst, src, cc) => "store " ^ show_cpsvar dst ^  " = " ^ sv src ^ "] ==>" ^ show_cpscomputation cc
            | CPSDynClsfdIn(n, uid, v, k) => "(clsfdin (name=" 
              ^ sv n ^ ") id=" ^ Int.toString uid ^ " v=" ^ sv v ^ ")" ^ sk k
            | CPSDynClsfdMatch(v, (uid, (a, c1)), c2) => "(caseclsfd (id=" ^ Int.toString uid ^ ")"  
            ^ sv v ^ " of {" ^ show_cpsvar a ^ " => " ^ sc c1 ^ " | otherwise => " ^ sc c2 ^ "}"
            | CPSPrimitiveOp (x) => "TODO: unimplemented cpsprimitiveop"
end

fun show_cpscontextvalue (cv : CPSAst.cpscontextvalue) = 
let open CPSAst
in
case cv of 
  PlainVar v => "PlainVar "^ show_cpsvar v
  | SelfVar v => "SelfVar "^ show_cpsvar v
  | GlobalVar v => "GlobalVar "^ show_cpsvar v
end

fun show_cpscontext (ctx : CPSAst.context) = 
String.concatWith ", \n" (map (fn (s, cv) => StructureName.toStringPlain s ^ " ==> " ^ show_cpscontextvalue cv) ctx)


fun show_static_error (err : 'a StaticErrorStructure.witherrsoption) (showa : 'a -> string) : string= 
let 
open StaticErrorStructure
in
  case err of
    Success a => showa a
    | NotAvailable => "<Not Available>"
    | DErrors l => "<error occurred>"
end



fun show_compilationfile file = 
let 
open CompilationStructure
in case file of
    CompilationFile  f => 
    "<CompilationFile: \n" 
    ^ "\nfp : " ^ (#fp f)
    ^ "\ncontent: " ^ show_static_error (#content f) (fn (utf8s, time) => show_utf8string utf8s) 
    ^ "\ntypeCheckingInfo: " ^ show_static_error (#typeCheckingInfo f)( fn (rsig) => show_typecheckingRSig rsig)
    ^ "\ndependencyInfo: " ^ show_static_error (#dependencyInfo f)(fn (sl) => "[Dependencies]")
    ^ "\ntypeCheckedInfo: " ^ show_static_error (#typeCheckedInfo f)(fn (csig) => show_typecheckingCSig csig)
    ^ "\ncpsInfo: " ^ show_static_error (#cpsInfo f)(fn (cps, cloconv, llvm) => 
      show_cpscomputation (#3 cps) ^ ";\n\t\t ClosureConvert = "
      ^ show_cpscomputation cloconv ^ "\n\t\t LLVM = ?"
      )
    ^ "\nllvmInfo: " ^ show_static_error (#llvmInfo f)(fn ({llfilepath=s}) =>  s)  (* the actual generated ll file *)

end
end