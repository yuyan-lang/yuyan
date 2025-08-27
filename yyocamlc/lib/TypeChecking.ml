open EngineData
open! ProcCombinators
module Env = Environment

type unification_ctx = (int * A.t option) list

let global_unification_ctx : unification_ctx ref = ref []

let add_to_global_unification_ctx (uid : int) (tp : A.t option) : unit =
  global_unification_ctx := (uid, tp) :: !global_unification_ctx
;;

let get_from_global_unification_ctx (uid : int) : A.t option = List.assoc_opt uid !global_unification_ctx |> Option.join

let set_global_unification_ctx (uid : int) (tp : A.t) : unit =
  if List.mem_assoc uid !global_unification_ctx
  then
    global_unification_ctx
    := List.map
         (fun (cur_uid, cur_tp) -> if cur_uid = uid then uid, Some tp else cur_uid, cur_tp)
         !global_unification_ctx
  else failwith ("uid " ^ string_of_int uid ^ " not found in global unification context")
;;

(* Bidirectional type checking skeleton *)
type local_tm_env = (string * A.t) list
type local_tp_env = string list

type local_env =
  { tp : local_tp_env
  ; tm : local_tm_env
  }

let empty_local_env : local_env = { tp = []; tm = [] }
let extend_local_env_tp (env : local_env) (name : string) : local_env = { env with tp = name :: env.tp }

let extend_local_env_tm (env : local_env) (name : string) (tp : A.t) : local_env =
  { env with tm = (name, tp) :: env.tm }
;;

let check_is_type (tp : A.t) : A.t proc_state_m =
  match A.view tp with
  | A.N (N.Builtin N.Type, _) -> return tp
  | _ -> pfail_with_ext "Expecting type but got " (A.get_extent_some tp)
;;

let check_kind_valid (tp : A.t) : A.t proc_state_m =
  match A.view tp with
  | A.N (N.Arrow, [ ([], dom); ([], cod) ]) ->
    let* checked_dom = check_is_type dom in
    let* checked_cod = check_is_type cod in
    return (A.fold_with_extent (A.N (N.Arrow, [ [], checked_dom; [], checked_cod ])) (A.get_extent_some tp))
  | _ -> check_is_type tp
;;

let rec normalize_type (tp : A.t) : A.t proc_state_m =
  match A.view tp with
  | A.FreeVar name ->
    let* id = Environment.find_binding name in
    (match id with
     | None -> return tp
     | Some id ->
       let* tp_constant = Environment.lookup_constant id in
       (match tp_constant with
        | TypeExpression tp -> normalize_type tp
        | TypeConstructor _ -> return (A.fold_with_extent (A.N (N.Constant id, [])) (A.get_extent_some tp))
        | DataExpression _ -> pfail_with_ext ("TC27: Expecting type but got " ^ A.show_view tp) (A.get_extent_some tp)
        | DataConstructor _ -> pfail_with_ext ("TC28: Expecting type but got " ^ A.show_view tp) (A.get_extent_some tp)))
  | A.N (N.UnifiableTp uid, []) ->
    (match get_from_global_unification_ctx uid with
     | None -> return tp
     | Some tp -> normalize_type tp)
  | _ -> return tp
;;

(* unify tp1 with tp2, where only tp1 may contain free var names, returns a list of substitutions 
that will cause tp1 to be equal to tp2 *)
let rec type_unify (free_var_names : (string * A.t option) list) (tp1 : A.t) (tp2 : A.t)
  : (string * A.t option) list proc_state_m
  =
  let* normalized_tp1 = normalize_type tp1 in
  let* normalized_tp2 = normalize_type tp2 in
  with_type_checking_history
    ("unifying "
     ^ A.show_view tp1
     ^ " and "
     ^ A.show_view tp2
     ^ " i.e. "
     ^ A.show_view normalized_tp1
     ^ " and "
     ^ A.show_view normalized_tp2)
  @@
  match A.view normalized_tp1, A.view normalized_tp2 with
  | A.N (N.UnifiableTp uid, []), _ ->
    set_global_unification_ctx uid normalized_tp2;
    return free_var_names
  | _, A.N (N.UnifiableTp uid, []) ->
    set_global_unification_ctx uid normalized_tp1;
    return free_var_names
  | A.N (N.Builtin N.StringType, []), A.N (N.Builtin N.StringType, []) -> return free_var_names
  | A.N (N.Builtin N.IntType, []), A.N (N.Builtin N.IntType, []) -> return free_var_names
  | A.N (N.Builtin N.BoolType, []), A.N (N.Builtin N.BoolType, []) -> return free_var_names
  | A.N (N.Builtin N.UnitType, []), A.N (N.Builtin N.UnitType, []) -> return free_var_names
  | A.N (N.Builtin N.FloatType, []), A.N (N.Builtin N.FloatType, []) -> return free_var_names
  | A.N (N.Arrow, [ ([], dom1); ([], cod1) ]), A.N (N.Arrow, [ ([], dom2); ([], cod2) ]) ->
    let* new_free_var_names = type_unify free_var_names dom1 dom2 in
    type_unify new_free_var_names cod1 cod2
  | _ ->
    pfail_with_ext
      (__LOC__ ^ " TC51: Failed to unify " ^ A.show_view tp1 ^ " and " ^ A.show_view tp2)
      (A.get_extent_some tp1)
;;

(* Check that a type is well-formed *)
let rec check_type_valid (env : local_env) (tp : A.t) : A.t proc_state_m =
  let* normalized_tp = normalize_type tp in
  with_type_checking_history
    ("checking type is valid: " ^ A.show_view tp ^ " (normalized to " ^ A.show_view normalized_tp)
  @@
  match A.view normalized_tp with
  | A.FreeVar name ->
    if List.mem name env.tp
    then return normalized_tp
    else pfail_with_ext ("TC98: Free variable not found in the environment: " ^ name) (A.get_extent_some normalized_tp)
  | A.N (N.Builtin N.StringType, []) -> return normalized_tp
  | A.N (N.Builtin N.IntType, []) -> return normalized_tp
  | A.N (N.Builtin N.BoolType, []) -> return normalized_tp
  | A.N (N.Builtin N.UnitType, []) -> return normalized_tp
  | A.N (N.Builtin N.FloatType, []) -> return normalized_tp
  | A.N (N.ImplicitPi, [ ([ bnd ], cod) ]) ->
    let env' = extend_local_env_tp env bnd in
    let* checked_coid = check_type_valid env' cod in
    return (A.fold_with_extent (A.N (N.ImplicitPi, [ [ bnd ], checked_coid ])) (A.get_extent_some tp))
  | A.N (N.Arrow, [ ([], dom); ([], cod) ]) ->
    let* dom = check_type_valid env dom in
    let* cod = check_type_valid env cod in
    return (A.fold_with_extent (A.N (N.Arrow, [ [], dom; [], cod ])) (A.get_extent_some normalized_tp))
  | A.N (N.Ap, [ ([], f); ([], arg) ]) ->
    (match A.view f with
     | A.FreeVar name ->
       let* id = Environment.lookup_binding name in
       let* tp_constant = Environment.lookup_constant id in
       (match tp_constant with
        | TypeConstructor { tp = tcons_tp; _ } ->
          (match A.view tcons_tp with
           | A.N (N.Arrow, [ ([], _); ([], _) ]) ->
             let* arg = check_type_valid env arg in
             return
               (A.fold_with_extent
                  (A.N (N.Ap, [ [], A.fold_with_extent (A.N (N.Constant id, [])) (A.get_extent_some f); [], arg ]))
                  (A.get_extent_some tp))
           | _ -> pfail_with_ext (__LOC__ ^ "TC141: Cannot be applied to " ^ A.show_view arg) (A.get_extent_some tp))
        | _ ->
          pfail_with_ext
            (__LOC__ ^ "TC141: Expecting some data but got " ^ show_t_constant tp_constant)
            (A.get_extent_some tp))
     | _ -> pfail_with_ext (__LOC__ ^ "TC140: Expecting free variable but got " ^ A.show_view f) (A.get_extent_some tp))
  | A.N (N.Constant id, []) ->
    let* tp_constant = Environment.lookup_constant id in
    (match tp_constant with
     | TypeExpression _ | TypeConstructor _ -> return normalized_tp
     | DataExpression _ | DataConstructor _ ->
       pfail_with_ext ("TC28: Expecting type but got " ^ A.show_view tp) (A.get_extent_some tp))
  | _ ->
    pfail_with_ext
      (__LOC__ ^ "TC26: Expecting type but got " ^ A.show_view normalized_tp)
      (A.get_extent_some normalized_tp)
;;

let rec apply_implicit_args (expr : A.t) (expr_tp : A.t) : (A.t * A.t) proc_state_m =
  with_type_checking_history ("applying implicit args to " ^ A.show_view expr ^ " with type " ^ A.show_view expr_tp)
  @@
  let* normalized_expr_tp = normalize_type expr_tp in
  (* TODO! *)
  match A.view normalized_expr_tp with
  | A.N (N.ImplicitPi, [ ([ bnd ], cod) ]) ->
    let id = Uid.next () in
    let _ = add_to_global_unification_ctx id None in
    let new_var = A.fold_with_extent (A.N (N.UnifiableTp id, [])) (A.get_extent_some expr) in
    let cod = A.subst new_var bnd cod in
    let new_expr = A.fold_with_extent (A.N (N.ImplicitAp, [ [], expr; [], new_var ])) (A.get_extent_some expr) in
    apply_implicit_args new_expr cod
  | _ -> return (expr, normalized_expr_tp)
;;

(* Synthesize/infer type from an expression *)
let rec synth (env : local_env) (expr : A.t) : (A.t * A.t) proc_state_m =
  with_type_checking_history ("synthesizing " ^ A.show_view expr)
  @@
  match A.view expr with
  | A.FreeVar name ->
    (match List.assoc_opt name env.tm with
     | Some tp -> return (expr, tp)
     | None ->
       let* id = Environment.find_binding name in
       (match id with
        | None -> pfail_with_ext ("TC83: Free variable not found in environment: " ^ name) (A.get_extent_some expr)
        | Some id ->
          let* tp_constant = Environment.lookup_constant id in
          (match tp_constant with
           | DataExpression { tp; _ } ->
             return (A.fold_with_extent (A.N (N.Constant id, [])) (A.get_extent_some expr), tp)
           | _ ->
             pfail_with_ext
               ("TC84: Expecting some data but got " ^ show_t_constant tp_constant)
               (A.get_extent_some expr))))
  | A.N (N.Builtin (N.Bool _), []) ->
    return (expr, A.fold_with_extent (A.N (N.Builtin N.BoolType, [])) (A.get_extent_some expr))
  | A.N (N.Builtin (N.Int _), []) ->
    return (expr, A.fold_with_extent (A.N (N.Builtin N.IntType, [])) (A.get_extent_some expr))
  | A.N (N.Builtin (N.String _), []) ->
    return (expr, A.fold_with_extent (A.N (N.Builtin N.StringType, [])) (A.get_extent_some expr))
  | A.N (N.Builtin N.Unit, []) ->
    return (expr, A.fold_with_extent (A.N (N.Builtin N.UnitType, [])) (A.get_extent_some expr))
  | A.N (N.Builtin (N.Float _), []) ->
    return (expr, A.fold_with_extent (A.N (N.Builtin N.FloatType, [])) (A.get_extent_some expr))
  | A.N (N.Ap, [ ([], f); ([], arg) ]) ->
    let* f, f_tp = synth env f in
    let* f, f_tp = apply_implicit_args f f_tp in
    (match A.view f_tp with
     | A.N (N.Arrow, [ ([], dom); ([], cod) ]) ->
       let* arg = check env arg dom in
       return (A.fold_with_extent (A.N (N.Ap, [ [], f; [], arg ])) (A.get_extent_some expr), cod)
     | _ ->
       pfail_with_ext ("TC134: Expecting its type to be an arrow but got " ^ A.show_view f_tp) (A.get_extent_some f))
  | _ ->
    pfail_with_ext
      (__LOC__ ^ " TC82: Expression does not support type synthesis, please specify the type " ^ A.show_view expr)
      (A.get_extent_some expr)

and check (env : local_env) (expr : A.t) (tp : A.t) : A.t proc_state_m = fill_implicit_lam_then_check env expr tp

and fill_implicit_lam_then_check (env : local_env) (expr : A.t) (expr_tp : A.t) : A.t proc_state_m =
  let* normalized_expr_tp = normalize_type expr_tp in
  match A.view expr, A.view normalized_expr_tp with
  | A.N (N.ImplicitLam, _), _ -> check_after_filling_implicit_lam env expr expr_tp
  | _, A.N (N.ImplicitPi, [ ([ bnd_name ], cod) ]) ->
    with_type_checking_history ("filling implicit lam " ^ A.show_view expr ^ " with type " ^ A.show_view expr_tp)
    @@
    let env' = extend_local_env_tp env bnd_name in
    let* checked_body = check env' expr cod in
    let checked_expr =
      A.fold_with_extent (A.N (N.ImplicitLam, [ [ bnd_name ], checked_body ])) (A.get_extent_some expr)
    in
    return checked_expr
  | _, _ -> check_after_filling_implicit_lam env expr expr_tp

(* Check expression against a type *)
and check_after_filling_implicit_lam (env : local_env) (expr : A.t) (tp : A.t) : A.t proc_state_m =
  with_type_checking_history ("checking " ^ A.show_view expr ^ " against " ^ A.show_view tp)
  @@
  let* tp_normalized = normalize_type tp in
  match A.view expr with
  | A.N (N.ExternalCall _, []) -> return expr
  | A.N (N.IfThenElse, [ ([], cond); ([], then_branch); ([], else_branch) ]) ->
    let* cond = check env cond (A.fold_with_extent (A.N (N.Builtin N.BoolType, [])) (A.get_extent_some cond)) in
    let* then_branch = check env then_branch tp in
    let* else_branch = check env else_branch tp in
    return
      (A.fold_with_extent (A.N (N.IfThenElse, [ [], cond; [], then_branch; [], else_branch ])) (A.get_extent_some expr))
  | A.N (N.ImplicitLam, [ ([ bnd ], body) ]) ->
    (match A.view tp_normalized with
     | A.N (N.ImplicitPi, [ ([ tp_bnd ], cod) ]) ->
       let env' = extend_local_env_tp env tp_bnd in
       let body = A.subst (A.annotate_with_extent (A.free_var bnd) (A.get_extent_some expr)) tp_bnd body in
       let* checked_body = check env' body cod in
       return (A.fold_with_extent (A.N (N.ImplicitLam, [ [ bnd ], checked_body ])) (A.get_extent_some expr))
     | _ ->
       pfail_with_ext
         ("TC132: Expecting its type to be an implicit pi but got " ^ A.show_view tp_normalized)
         (A.get_extent_some expr))
  | A.N (N.Lam, [ ([ bnd ], body) ]) ->
    (match A.view tp_normalized with
     | A.N (N.Arrow, [ ([], dom); ([], cod) ]) ->
       let env' = extend_local_env_tm env bnd dom in
       let* checked_body = check env' body cod in
       return (A.fold_with_extent (A.N (N.Lam, [ [ bnd ], checked_body ])) (A.get_extent_some expr))
     | _ ->
       pfail_with_ext
         ("TC133: Expecting its type to be an arrow but got " ^ A.show_view tp_normalized)
         (A.get_extent_some expr))
  | _ ->
    let* synth_expr, synth_tp = synth env expr in
    let* _ = type_unify [] synth_tp tp_normalized in
    return synth_expr
;;

let assert_no_free_vars (tp : A.t) : unit proc_state_m =
  with_type_checking_history ("asserting no free vars in " ^ A.show_view tp)
  @@
  match A.get_free_vars tp with
  | [] -> return ()
  | free_vars ->
    pfail
      ("TC_assert_no_free_vars: free variables found in type: "
       ^ String.concat ", " free_vars
       ^ " in type: "
       ^ A.show_view tp)
;;

let check_type_valid_top (tp : A.t) : A.t proc_state_m =
  with_type_checking_history ("checking type is valid: " ^ A.show_view tp)
  @@
  let* checked_tp = check_type_valid empty_local_env tp in
  let* () = assert_no_free_vars checked_tp in
  return checked_tp
;;

let check_top (expr : A.t) (tp : A.t) : A.t proc_state_m =
  with_type_checking_history ("checking " ^ A.show_view expr ^ " against " ^ A.show_view tp)
  @@ check empty_local_env expr tp
;;

let synth_top (expr : A.t) : (A.t * A.t) proc_state_m =
  with_type_checking_history ("synthesizing " ^ A.show_view expr) @@ synth empty_local_env expr
;;

let group_type_constructor_declarations (decls : A.t list) : (A.t * A.t list) * A.t list =
  (* print_endline ("group_type_constructor_declarations: " ^ String.concat ", " (List.map A.show_view decls)); *)
  match decls with
  | [] -> failwith "TC11: Unexpected end of declaration list"
  | hd :: tl ->
    (match A.view hd with
     | A.N (N.Declaration N.TypeConstructorDecl, [ ([], _name); _ ]) ->
       let rec aux (sofar : A.t list) (decls : A.t list) : (A.t * A.t list) * A.t list =
         (* print_endline
           ("aux sofar: "
            ^ String.concat ", " (List.map A.show_view sofar)
            ^ " remaining: "
            ^ String.concat ", " (List.map A.show_view decls)); *)
         match decls with
         | [] -> (hd, sofar), []
         | tl_hd :: tl_tl ->
           (match A.view tl_hd with
            | A.N (N.Declaration N.ConstructorDecl, _) -> aux (sofar @ [ tl_hd ]) tl_tl
            | _ -> (hd, sofar), decls)
       in
       aux [] tl
     | _ -> failwith ("TC12: Expecting type constructor declaration but got " ^ A.show_view hd))
;;

let rec group_declarations (decls : A.t list) : (A.t * A.t list) list =
  (* print_endline ("group_declarations: " ^ String.concat ", " (List.map A.show_view decls)); *)
  match decls with
  | [] -> []
  | hd :: tl ->
    (match A.view hd with
     | A.N (N.Declaration N.ConstantDecl, _) ->
       (match tl with
        | [] -> failwith "OO245: Unexpected end of declaration list"
        | tl_hd :: tl_tl ->
          (match A.view tl_hd with
           | A.N (N.Declaration N.ConstantDefn, _) -> (hd, [ tl_hd ]) :: group_declarations tl_tl
           | _ -> Fail.failwith ("OO246: Expecting constant definition, got: " ^ A.show_view tl_hd)))
     | A.N (N.Declaration N.DirectExpr, _) -> (hd, []) :: group_declarations tl
     | A.N (N.Declaration N.CustomOperatorDecl, _) -> group_declarations tl
     | A.N (N.Declaration N.TypeConstructorDecl, _) ->
       let cons_decls, remaining = group_type_constructor_declarations decls in
       cons_decls :: group_declarations remaining
     | A.N (N.Declaration N.TypeDefn, _) -> (hd, []) :: group_declarations tl
     | A.N (N.Declaration N.ModuleAliasDefn, _) -> (hd, []) :: group_declarations tl
     | _ -> Fail.failwith ("TC278: Expecting group leading constructor declaration, got: " ^ A.show_view hd))
;;

let rec get_constructor_tp_head (tp : A.t) : string =
  match A.view tp with
  | A.FreeVar name -> name
  | A.N (N.Ap, [ ([], head); _ ]) -> get_constructor_tp_head head
  | A.N ((N.ExplicitPi | N.ImplicitPi), [ ([], _); ([ _ ], cod) ]) -> get_constructor_tp_head cod
  | A.N (N.Arrow, [ ([], _); ([], cod) ]) -> get_constructor_tp_head cod
  | _ -> Fail.failwith ("TC41: Expecting constructor type but got " ^ A.show_view tp)
;;
