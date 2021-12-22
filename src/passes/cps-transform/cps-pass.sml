structure CPSPass =
struct

open TypeCheckingAST
open CPSAst

exception CPSInternalError

    datatype cpscontextvalue = PlainVar of int
                             | SelfVar of int (* selfvar represents a pending computation that needs to be applied to itself *)

    type context = (StructureName.t * cpscontextvalue) list

    fun kcc (cc : cpsvar -> cpscomputation) : cpscontinuation = 
        let val v = UID.next()
        in (v, cc v) end
    fun kcc2 (cc : cpsvar -> cpsvar -> cpscomputation) : cpsvar * cpsvar * cpscomputation = 
        let val v1 = UID.next()
        val v2 = UID.next()
        in (v1, v2, cc v1 v2) end

    fun klookupLabel ( ctx : (Label * Type) list) (l : Label) : int = 
        case ctx of 
             (n1, t1)::cs => if UTF8String.semanticEqual n1 l then 0 else klookupLabel cs l+1
      fun klookupLabel3 ( ctx : (Label * EVar *'a ) list) (l : Label) : int = 
        case ctx of 
             (n1, _, t1)::cs => if UTF8String.semanticEqual n1 l then 0 else klookupLabel3 cs l+1

    fun cpsTransformSig  (ctx : context) (s : CSignature) 
    (cc : context * cpsvar option (* possible computation result *) -> cpscomputation) : cpscomputation =

            (* print ("eraseSigLazy DEBUG " ^ PrettyPrint.show_typecheckingSig s )
            ; *)
            case s of
            [] => cc (ctx, NONE)
            (* (case kont of SOME f => f(ctx) | NONE => PKRet(PKUnit)) *)
            (* optimize if tail of the block is an expression, it is the value of the expression *)
        | [CDirectExpr e]  => 
             cpsTransformExpr ctx e (fn resvar => 
             cc (ctx, SOME resvar))

        | CTermDefinition(name, def):: ss =>  
             cpsTransformExpr ctx def (fn resvar => 
            cpsTransformSig ((name, PlainVar resvar)::ctx) ss cc)
        | CDirectExpr e :: ss => 
             cpsTransformExpr ctx e (fn resvar => 
            cpsTransformSig (ctx) ss cc)

    and cpsTransformExpr   
        (ctx : context) (e : CExpr) (cc : cpsvar -> cpscomputation) (* cc is current continutaion *)
        : cpscomputation =
    (
        let 
        (* val _ = print ("cpsTransformSig on " ^ PrettyPrint.show_typecheckingExpr e ^ " in context " ^ PrettyPrint.show_typecheckingpassctx (eraseCtx ctx) ^ "\n"); *)
         val res = case e of
            CExprVar sn => (case ListSearchUtil.lookupSName ctx sn of 
                PlainVar v => cc v
                | SelfVar v => CPSAbsSingle(kcc (fn arg => 
                        cc arg
                    ), NONE, kcc (fn kont => 
                        CPSApp(CPSVar v, (CPSVar v, CPSVar kont)) (* apply the recursive value to itself *)
                     )))
            | CUnitExpr => CPSUnit (kcc cc)
            | CTuple (cs, Prod ls) => (
                    let fun go i values = 
                        if i = List.length cs
                        then CPSTuple(values, kcc cc)
                        else cpsTransformExpr ctx (List.nth(cs, i)) 
                                    (fn v => go (i+1) (values@[CPSVar v]))
                    in go 0 [] end
                )
            | CProj(e, l, Prod ls) => cpsTransformExpr ctx e (fn v 
                        => CPSProj(CPSVar v, (klookupLabel ls l),(kcc cc)))
            | CInj (l, e, Sum ls ) => 
                cpsTransformExpr ctx e 
                    (fn v => CPSInj(l, klookupLabel ls l, CPSVar v, kcc cc)
            )
            | CCase((Sum ls, e),cases, resType) => 
            (cpsTransformExpr ctx e) (fn v => 
                    CPSCases (CPSVar v, (List.tabulate(List.length ls, 
                    fn index => let val  (label,t) = List.nth(ls, index)
                                    val caseIndex = klookupLabel3 cases label
                                    val (_, ev, e) = List.nth(cases, caseIndex)
                                in kcc (fn boundId => cpsTransformExpr (
                                     ([ev], PlainVar boundId)::ctx) e cc
                                ) end)))
            )
            | CLam(ev, eb, Func(t1, t2)) => 
                CPSAbs (kcc2 (fn arg => fn ret =>
                    cpsTransformExpr ((([ev], PlainVar arg))::ctx) eb 
                        (fn r => CPSAppSingle(CPSVar ret,CPSVar r))
                ), NONE, kcc cc)
            | CApp (e1, e2, t) => 
                cpsTransformExpr ctx e1 (fn v1 => 
                 cpsTransformExpr ctx e2 (fn v2 => 
                    CPSAbsSingle(kcc (fn arg => 
                        cc arg
                    ), NONE, kcc (fn kont => 
                        CPSApp(CPSVar v1, (CPSVar v2, CPSVar kont))
                     ))
                 ))
            | CTAbs (tv, e2, _) => cpsTransformExpr ctx e2 cc
            | CTApp (e2, t, _) => cpsTransformExpr ctx e2 cc
            | CPack (t, e2, et) => cpsTransformExpr ctx e2 cc
            | COpen ((et, e1), (tv, ev, e2), rt) => 
                (cpsTransformExpr ctx e1) 
                    (fn v => 
                cpsTransformExpr (([ev],PlainVar v)::ctx) e2 cc
                    )
            | CFold (e2, t) => 
                cpsTransformExpr ctx e2 (fn v => CPSFold(CPSVar v, kcc cc))
            | CUnfold (e2, _) => cpsTransformExpr ctx e2 (fn v => CPSUnfold(CPSVar v , kcc cc))
            | CFix (ev, e, _)=>  
            let val fixedPointBoundId = UID.next()
            in
              CPSAbs (kcc2 (fn self => fn ret =>
                    cpsTransformExpr (([ev], SelfVar self) :: ctx) e 
                     (fn r => CPSAppSingle(CPSVar ret,CPSVar r))
                ), NONE, (fixedPointBoundId , 
                    CPSAbsSingle(kcc (fn arg => 
                        cc arg
                        ), NONE, kcc (fn kont => 
                        CPSApp(CPSVar fixedPointBoundId, (CPSVar fixedPointBoundId, CPSVar kont))
                        ))
                    )
                )
                end
                (* Is this really the case ??? *)
            (* let
             fix F = ((\y. f (y y)) (\y. f (y y)))
             (this one causes infinite look)
                    val compiledF = KAbs (fn v => cpsTransformSig ((ev, v)::kctx) ((ev, tt)::ctx) e tt)
                    fun appVV (v1 : kvalue) (v2:kvalue) : kcomputation = KApp(KRet(v1), KRet(v2))
                    val lamydotfyy = KAbs(fn y => KApp(KRet(compiledF),(appVV y y)))
                in appVV lamydotfyy lamydotfyy
            end *)
            | CStringLiteral l => 
                CPSBuiltinValue(CPSBvString l, kcc cc)
            | CLetIn(csig, e,t) => 
                cpsTransformSig ctx csig (fn (newCtx, _) (* ignore the final value of the decls*) => 
                    cpsTransformExpr newCtx e cc)
            | _ => raise Fail "cpsp116"

        (* val _ = print ("cpsTransformSig result is " ^ PrettyPrint.show_pkcomputation res ^ "cpsTransformSig on " ^ PrettyPrint.show_typecheckingExpr e ^ 
        " against type" ^ PrettyPrint.show_typecheckingType tt ^
        " in context " ^ PrettyPrint.show_typecheckingpassctx (eraseCtx ctx) ^ "\n"); *)
        in res end
        )

        handle ListSearchUtil.NotFoundSName sname => 
            (DebugPrint.p ("Internal error: " ^ StructureName.toStringPlain sname  ^ " not found \n");
            raise CPSInternalError)
        handle CPSInternalError =>
            (DebugPrint.p ("When transforming expression " ^ PrettyPrint.show_typecheckingCExpr e ^ " \n");
            raise CPSInternalError)


 fun cpsTransformSigTopLevel  (s : CSignature) 
     : cpscomputation =
    cpsTransformSig [] s (fn (ctx, resvar) => 
        case resvar of SOME resvar => CPSDone (CPSVar resvar)
                     | NONE => raise Fail "TODO")


(* this assumes the signature has been fully checked, so no more checking !*)
(* THIS IS VERY IMPORTANT, as this file just copies from type checking but disregards all 
untyped cases *)
(* the lazy comes from the fact that we used lambdas during kcomputation construction *)
(* we can force evaluation by persisting it back and forth *)
    


end