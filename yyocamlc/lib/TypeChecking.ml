open EngineData
open! ProcCombinators
module Env = Environment

let add_to_global_unification_ctx (uid : int) (tp : A.t option) : unit proc_state_m =
  let* s = get_proc_state () in
  let new_ctx = (uid, tp) :: s.unification_ctx in
  write_proc_state { s with unification_ctx = new_ctx }
;;

let get_from_global_unification_ctx (uid : int) : A.t option proc_state_m =
  let* s = get_proc_state () in
  return (List.assoc_opt uid s.unification_ctx |> Option.join)
;;

let set_global_unification_ctx (uid : int) (tp : A.t) : unit proc_state_m =
  let* s = get_proc_state () in
  if List.mem_assoc uid s.unification_ctx
  then
    write_proc_state
      { s with
        unification_ctx =
          List.map (fun (cur_uid, cur_tp) -> if cur_uid = uid then uid, Some tp else cur_uid, cur_tp) s.unification_ctx
      }
  else pfail ("uid " ^ string_of_int uid ^ " not found in global unification context")
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
        | DataExpression _ | PatternVar _ | DataConstructor _ | ModuleAlias _ ->
          pfail_with_ext ("TC28: Expecting type but got " ^ A.show_view tp) (A.get_extent_some tp)))
  | A.N (N.UnifiableTp uid, []) ->
    let* tp_deref = get_from_global_unification_ctx uid in
    (match tp_deref with
     | None -> return tp
     | Some tp -> normalize_type tp)
  | _ -> return tp
;;

(* unify tp1 with tp2, where only tp1 may contain free var names, returns a list of substitutions 
that will cause tp1 to be equal to tp2 *)
let rec type_unify
          (expr : A.t) (* for error reporting *)
          (free_var_names : (string * A.t option) list)
          (tp1 : A.t)
          (tp2 : A.t)
  : (string * A.t option) list proc_state_m
  =
  let type_unify = type_unify expr in
  let* normalized_tp1 = normalize_type tp1 in
  let* normalized_tp2 = normalize_type tp2 in
  with_type_checking_history (HistTwo ("unifying ", tp1, " and ", tp2))
  @@
  let fail_to_unify msg =
    pfail_with_ext
      (__LOC__
       ^ " "
       ^ msg
       ^ " "
       ^ " TC51: Failed to unify "
       ^ A.show_view tp1
       ^ " and "
       ^ A.show_view tp2
       ^ " i.e. "
       ^ A.show_view normalized_tp1
       ^ " and "
       ^ A.show_view normalized_tp2)
      (A.get_extent_some expr)
  in
  match A.view normalized_tp1, A.view normalized_tp2 with
  | A.N (N.UnifiableTp uid1, []), A.N (N.UnifiableTp uid2, []) when uid1 = uid2 -> return free_var_names
  | A.N (N.UnifiableTp uid, []), _ ->
    let* () = set_global_unification_ctx uid normalized_tp2 in
    return free_var_names
  | _, A.N (N.UnifiableTp uid, []) ->
    let* () = set_global_unification_ctx uid normalized_tp1 in
    return free_var_names
  | A.N (N.Builtin N.StringType, []), A.N (N.Builtin N.StringType, []) -> return free_var_names
  | A.N (N.Builtin N.IntType, []), A.N (N.Builtin N.IntType, []) -> return free_var_names
  | A.N (N.Builtin N.BoolType, []), A.N (N.Builtin N.BoolType, []) -> return free_var_names
  | A.N (N.Builtin N.UnitType, []), A.N (N.Builtin N.UnitType, []) -> return free_var_names
  | A.N (N.Builtin N.FloatType, []), A.N (N.Builtin N.FloatType, []) -> return free_var_names
  | A.N (N.Constant id1, []), A.N (N.Constant id2, []) when id1 = id2 -> return free_var_names
  | A.FreeVar name1, A.FreeVar name2 when name1 = name2 -> return free_var_names
  | A.N (N.Arrow, [ ([], dom1); ([], cod1) ]), A.N (N.Arrow, [ ([], dom2); ([], cod2) ]) ->
    let* new_free_var_names = type_unify free_var_names dom1 dom2 in
    type_unify new_free_var_names cod1 cod2
  | A.N (N.Ap, [ ([], f1); ([], arg1) ]), A.N (N.Ap, [ ([], f2); ([], arg2) ]) ->
    let* new_free_var_names = type_unify free_var_names f1 f2 in
    type_unify new_free_var_names arg1 arg2
  | A.N (N.Sequence Comma, args1), A.N (N.Sequence Comma, args2) ->
    if List.length args1 <> List.length args2
    then fail_to_unify "Length Mismatch"
    else
      let* new_free_var_names =
        pfold_left
          (fun free_var_names ((_, arg1), (_, arg2)) ->
             let* new_free_var_names = type_unify free_var_names arg1 arg2 in
             return new_free_var_names)
          free_var_names
          (ListUtil.zip args1 args2)
      in
      return new_free_var_names
  | _ -> fail_to_unify "Head Mismatch "
;;

(* Check that a type is well-formed *)
let rec check_type_valid (env : local_env) (tp : A.t) : A.t proc_state_m =
  let* normalized_tp = normalize_type tp in
  with_type_checking_history (HistOne ("checking type is valid ", tp))
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
  | A.N (N.Builtin N.RefType, []) -> return normalized_tp
  | A.N (N.Builtin N.ArrayRefType, []) -> return normalized_tp
  | A.N (N.ImplicitPi, [ ([ bnd ], cod) ]) ->
    let env' = extend_local_env_tp env bnd in
    let* checked_coid = check_type_valid env' cod in
    return (A.fold_with_extent (A.N (N.ImplicitPi, [ [ bnd ], checked_coid ])) (A.get_extent_some tp))
  | A.N (N.Arrow, [ ([], dom); ([], cod) ]) ->
    let* dom = check_type_valid env dom in
    let* cod = check_type_valid env cod in
    return (A.fold_with_extent (A.N (N.Arrow, [ [], dom; [], cod ])) (A.get_extent_some normalized_tp))
  | A.N (N.Ap, [ ([], f); ([], arg) ]) ->
    let* id =
      match A.view f with
      | A.FreeVar name -> Environment.lookup_binding (Ext.str_with_extent name (A.get_extent_some f))
      | A.N (N.Constant id, []) -> return id
      | _ -> pfail_with_ext (__LOC__ ^ "TC140: Expecting free variable but got " ^ A.show_view f) (A.get_extent_some tp)
    in
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
     | TypeExpression tp ->
       (match A.view tp with
        | A.N (N.Builtin N.RefType, []) | A.N (N.Builtin N.ArrayRefType, []) ->
          let* arg = check_type_valid env arg in
          return
            (A.fold_with_extent
               (A.N (N.Ap, [ [], A.fold_with_extent (A.N (N.Constant id, [])) (A.get_extent_some f); [], arg ]))
               (A.get_extent_some tp))
        | _ -> pfail_with_ext (__LOC__ ^ "TC141: Cannot be applied to " ^ A.show_view arg) (A.get_extent_some tp))
     | _ ->
       pfail_with_ext
         (__LOC__ ^ "TC141: Expecting some data but got " ^ EngineDataPrint.show_t_constant tp_constant)
         (A.get_extent_some tp))
  | A.N (N.Constant id, []) ->
    let* tp_constant = Environment.lookup_constant id in
    (match tp_constant with
     | TypeExpression _ | TypeConstructor _ -> return normalized_tp
     | DataExpression _ | DataConstructor _ | PatternVar _ | ModuleAlias _ ->
       pfail_with_ext ("TC28: Expecting type but got " ^ A.show_view tp) (A.get_extent_some tp))
  | A.N (N.Sequence Comma, args) ->
    let* args =
      psequence
        (List.map
           (fun (_, arg) ->
              let* arg = check_type_valid env arg in
              return ([], arg))
           args)
    in
    return (A.fold_with_extent (A.N (N.Sequence Comma, args)) (A.get_extent_some tp))
  | _ ->
    pfail_with_ext
      (__LOC__ ^ "TC26: Expecting type but got " ^ A.show_view normalized_tp)
      (A.get_extent_some normalized_tp)
;;

let check_data_constructor_final_type_valid (env : local_env) (tp : A.t) : (A.t * int) proc_state_m =
  with_type_checking_history (HistOne ("checking data constructor is a direct type: ", tp))
  @@
  let* checked_tp = check_type_valid env tp in
  match A.view checked_tp with
  | A.N (N.Constant id, []) -> return (checked_tp, id)
  | A.N (N.Ap, [ ([], f); ([], _) ]) ->
    (match A.view f with
     | A.N (N.Constant id, []) -> return (checked_tp, id)
     | _ ->
       pfail_with_ext
         (__LOC__ ^ "TC144: Ill-formed data constructor type, head is not a type constructor " ^ A.show_view checked_tp)
         (A.get_extent_some tp))
  | _ ->
    pfail_with_ext
      (__LOC__
       ^ "TC145: Ill-formed data constructor type, expecting type constructor or applied to a single argument"
       ^ A.show_view tp)
      (A.get_extent_some tp)
;;

let rec check_data_constructor_type_valid (env : local_env) (tp : A.t)
  : (A.t * int (* id of the final type*)) proc_state_m
  =
  with_type_checking_history (HistOne ("checking data constructor type is valid: ", tp))
  @@
  match A.view tp with
  | A.N (N.ImplicitPi, [ ([ bnd ], cod) ]) ->
    let env' = extend_local_env_tp env bnd in
    let* checked_cod, id = check_data_constructor_type_valid env' cod in
    return (A.fold_with_extent (A.N (N.ImplicitPi, [ [ bnd ], checked_cod ])) (A.get_extent_some tp), id)
  | A.N (N.Arrow, [ ([], dom); ([], cod) ]) ->
    let* dom = check_type_valid env dom in
    let* cod, id = check_data_constructor_final_type_valid env cod in
    return (A.fold_with_extent (A.N (N.Arrow, [ [], dom; [], cod ])) (A.get_extent_some tp), id)
  | _ -> check_data_constructor_final_type_valid env tp
;;

let rec apply_implicit_args (expr : A.t) (expr_tp : A.t) : (A.t * A.t) proc_state_m =
  with_type_checking_history (HistTwo ("applying implicit args to ", expr, " with type ", expr_tp))
  @@
  let* normalized_expr_tp = normalize_type expr_tp in
  (* TODO! *)
  match A.view normalized_expr_tp with
  | A.N (N.ImplicitPi, [ ([ bnd ], cod) ]) ->
    let id = Uid.next () in
    let* () = add_to_global_unification_ctx id None in
    let new_var = A.fold_with_extent (A.N (N.UnifiableTp id, [])) (A.get_extent_some expr) in
    let cod = A.subst new_var bnd cod in
    let new_expr = A.fold_with_extent (A.N (N.ImplicitAp, [ [], expr; [], new_var ])) (A.get_extent_some expr) in
    apply_implicit_args new_expr cod
  | _ -> return (expr, normalized_expr_tp)
;;

(* expand component fold right *)
let rec desugar_top_level (expr : A.t) : A.t option proc_state_m =
  match A.view expr with
  | A.N (N.ComponentFoldRight, [ ([], f); ([], l); ([], acc) ]) ->
    (match A.view l with
     | A.N (N.Sequence Dot, args) ->
       let expr =
         List.fold_right
           (fun (_, arg) acc ->
              let f_arg = A.fold_with_extent (A.N (N.Sequence Dot, ([], arg) :: [ [], acc ])) (A.get_extent_some arg) in
              A.fold_with_extent (A.N (N.Ap, [ [], f; [], f_arg ])) (A.get_extent_some arg))
           args
           acc
       in
       return (Some expr)
     | _ ->
       let l = A.fold_with_extent (A.N (N.Sequence Dot, ([], l) :: [])) (A.get_extent_some l) in
       let expr = A.fold_with_extent (A.N (N.ComponentFoldRight, [ [], f; [], l; [], acc ])) (A.get_extent_some expr) in
       desugar_top_level expr)
  | _ -> return None
;;

let get_tp_for_expr_id (id : int) : A.t proc_state_m =
  let* const = Environment.lookup_constant id in
  match const with
  | DataExpression { tp; _ } | DataConstructor { tp; _ } -> return tp
  | _ -> pfail (__LOC__ ^ " Expecting some data but got " ^ EngineDataPrint.show_t_constant const)
;;

let partial_resolve_structure_deref (_env : local_env) (expr : A.t) : int proc_state_m =
  match A.view expr with
  | A.FreeVar name ->
    let* id = Environment.lookup_binding (Ext.str_with_extent name (A.get_extent_some expr)) in
    return id
  | _ -> pfail_with_ext (__LOC__ ^ " Only dereference from a free variable is supported") (A.get_extent_some expr)
;;

let resolve_structure_deref (env : local_env) (expr : A.t)
  : (A.t (* resolved constant *) * A.t (* resolved type *)) proc_state_m
  =
  match A.view expr with
  | A.N (N.StructureDeref deref_name, [ ([], arg) ]) ->
    let* id = partial_resolve_structure_deref env arg in
    let* const = Environment.lookup_constant id in
    (match const with
     | ModuleAlias { name = _; filepath } ->
       (match !compilation_manager_get_file_hook filepath with
        | None -> failwith "Impossible"
        | Some (mexpr, _) ->
          (match A.view mexpr with
           | A.N (N.ModuleDef, args) ->
             (match
                List.filter_map
                  (fun (_, arg) ->
                     match A.view arg with
                     | A.N (N.Declaration (N.CheckedConstantDefn (name, id)), _)
                       when Ext.get_str_content name = deref_name -> Some id
                     | A.N (N.Declaration (N.ModuleAliasDefn (name, _)), _) when Ext.get_str_content name = deref_name
                       -> failwith "TODO double module alias deref"
                     | _ -> None)
                  (List.rev args)
              with
              | id :: _ ->
                let ret_expr = A.fold_with_extent (A.N (N.Constant id, [])) (A.get_extent_some expr) in
                let* ret_tp = get_tp_for_expr_id id in
                return (ret_expr, ret_tp)
              | _ ->
                pfail_with_ext
                  (__LOC__ ^ " Name not " ^ deref_name ^ " not found in " ^ filepath)
                  (A.get_extent_some expr))
           | _ -> failwith (__LOC__ ^ " Expecting a module expression, got " ^ A.show_view mexpr)))
     | _ ->
       pfail_with_ext (__LOC__ ^ " Expecting a module alias as subject of structure deref") (A.get_extent_some expr))
  | _ -> Fail.failwith (__LOC__ ^ " Expecting a structure deref on the top level, got " ^ A.show_view expr)
;;

(* Synthesize/infer type from an expression *)
let rec synth (env : local_env) (expr : A.t) : (A.t * A.t) proc_state_m =
  with_type_checking_history (HistOne ("synthesizing ", expr))
  @@
  let* expr_desugared = desugar_top_level expr in
  match expr_desugared with
  | Some expr_desugared -> synth env expr_desugared
  | None ->
    (match A.view expr with
     | A.FreeVar name ->
       (match List.assoc_opt name env.tm with
        | Some tp -> return (expr, tp)
        | None ->
          let* id = Environment.find_binding name in
          (match id with
           | None -> pfail_with_ext ("TC83: Free variable not found in environment: " ^ name) (A.get_extent_some expr)
           | Some id ->
             let* tp_constant = Environment.lookup_constant id in
             let expr = A.fold_with_extent (A.N (N.Constant id, [])) (A.get_extent_some expr) in
             (match tp_constant with
              | DataExpression { tp; _ } | DataConstructor { tp; _ } -> return (expr, tp)
              | _ ->
                pfail_with_ext
                  (__LOC__ ^ "TC84: Expecting some data but got " ^ EngineDataPrint.show_t_constant tp_constant)
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
     | A.N (N.TypingAnnotation, [ ([], tp); ([], tm) ]) ->
       let* tp = check_type_valid env tp in
       let* tm = check env tm tp in
       let expr = A.fold_with_extent (A.N (N.TypingAnnotation, [ [], tp; [], tm ])) (A.get_extent_some expr) in
       return (expr, tp)
     | A.N (N.TypedLam, [ ([], dom); ([ bnd ], body) ]) ->
       let* dom = check_type_valid env dom in
       let env' = extend_local_env_tm env bnd dom in
       let* body, cod_tp = synth env' body in
       let tp = A.fold_with_extent (A.N (N.Arrow, [ [], dom; [], cod_tp ])) (A.get_extent_some expr) in
       let expr = A.fold_with_extent (A.N (N.TypedLam, [ [], dom; [ bnd ], body ])) (A.get_extent_some expr) in
       return (expr, tp)
     | A.N (N.TupleDeref idx, [ ([], f) ]) ->
       let* f, f_tp = synth env f in
       (match A.view f_tp with
        | A.N (N.Sequence Comma, args) ->
          if idx < 0 || idx >= List.length args
          then pfail_with_ext ("TC134: Index out of bounds: " ^ string_of_int idx) (A.get_extent_some expr)
          else (
            let _, tp = List.nth args idx in
            return
              ( A.fold_with_extent
                  (A.N (N.CheckedTupleDeref { idx; len = List.length args }, [ [], f ]))
                  (A.get_extent_some expr)
              , tp ))
        | _ ->
          pfail_with_ext
            ("TC134: Expecting its type to be a sequence but got " ^ A.show_view f_tp)
            (A.get_extent_some f))
     | A.N (N.ImplicitAp, [ ([], f); ([], targ) ]) ->
       let* f, f_tp = synth env f in
       (match A.view f_tp with
        | A.N (N.ImplicitPi, [ ([ name ], cod) ]) ->
          let* targ = check_type_valid env targ in
          let cod = A.subst targ name cod in
          return (A.fold_with_extent (A.N (N.ImplicitAp, [ [], f; [], targ ])) (A.get_extent_some expr), cod)
        | _ ->
          pfail_with_ext
            ("TC134: Expecting its type to be an implicit pi but got " ^ A.show_view f_tp)
            (A.get_extent_some f))
     | A.N (N.Ap, [ ([], f); ([], arg) ]) ->
       let* f, f_tp = synth env f in
       let* f, f_tp = apply_implicit_args f f_tp in
       (match A.view f_tp with
        | A.N (N.Arrow, [ ([], dom); ([], cod) ]) ->
          let* arg = check env arg dom in
          return (A.fold_with_extent (A.N (N.Ap, [ [], f; [], arg ])) (A.get_extent_some expr), cod)
        | _ ->
          pfail_with_ext ("TC134: Expecting its type to be an arrow but got " ^ A.show_view f_tp) (A.get_extent_some f))
     | A.N (N.Constant id, []) ->
       let* tp_constant = Environment.lookup_constant id in
       (match tp_constant with
        | PatternVar { tp; _ } -> return (expr, tp)
        | DataConstructor { tp; _ } -> return (expr, tp)
        | DataExpression { tp; _ } -> return (expr, tp)
        | _ ->
          pfail_with_ext
            (__LOC__ ^ "TC331: Expecting some data but got " ^ EngineDataPrint.show_t_constant tp_constant)
            (A.get_extent_some expr))
     | A.N (N.Sequence Dot, args) ->
       let* args_checked =
         psequence
           (List.map
              (fun (_, arg) ->
                 let* arg, arg_tp = synth env arg in
                 return (([], arg), ([], arg_tp)))
              args)
       in
       let new_arg = A.fold_with_extent (A.N (N.Sequence Dot, List.map fst args_checked)) (A.get_extent_some expr) in
       let new_tp = A.fold_with_extent (A.N (N.Sequence Comma, List.map snd args_checked)) (A.get_extent_some expr) in
       return (new_arg, new_tp)
     | A.N (N.StructureDeref _, _) -> resolve_structure_deref env expr
     | _ ->
       pfail_with_ext
         (__LOC__ ^ " TC82: Expression does not support type synthesis, please specify the type " ^ A.show_view expr)
         (A.get_extent_some expr))

and check (env : local_env) (expr : A.t) (tp : A.t) : A.t proc_state_m =
  let* expr_desugared = desugar_top_level expr in
  match expr_desugared with
  | Some expr_desugared -> check env expr_desugared tp
  | None -> fill_implicit_lam_then_check env expr tp

and fill_implicit_lam_then_check (env : local_env) (expr : A.t) (expr_tp : A.t) : A.t proc_state_m =
  let* normalized_expr_tp = normalize_type expr_tp in
  match A.view expr, A.view normalized_expr_tp with
  | A.N (N.ImplicitLam, _), _ -> check_after_filling_implicit_lam env expr expr_tp
  | _, A.N (N.ImplicitPi, [ ([ bnd_name ], cod) ]) ->
    with_type_checking_history (HistTwo ("filling implicit lam ", expr, " with type ", expr_tp))
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
  with_type_checking_history (HistTwo ("checking ", expr, " against ", tp))
  @@
  let* tp_normalized = normalize_type tp in
  let default_synth_then_unify () =
    let* synth_expr, synth_tp = synth env expr in
    (* we can assume tp is not implicit pi as it has been filled, and expr is not implicit lam as this was a previosu case *)
    let* synth_expr, synth_tp = apply_implicit_args synth_expr synth_tp in
    let* _ = type_unify synth_expr [] synth_tp tp_normalized in
    return synth_expr
  in
  match A.view expr with
  | A.N (N.ExternalCall fname, args) ->
    let* args =
      psequence
        (List.map
           (fun (_, arg) ->
              let* arg, _arg_tp = synth env arg in
              return ([], arg))
           args)
    in
    return (A.fold_with_extent (A.N (N.ExternalCall fname, args)) (A.get_extent_some expr))
  | A.N (N.IfThenElse, [ ([], cond); ([], then_branch); ([], else_branch) ]) ->
    let* cond = check env cond (A.fold_with_extent (A.N (N.Builtin N.BoolType, [])) (A.get_extent_some cond)) in
    let* then_branch = check env then_branch tp in
    let* else_branch = check env else_branch tp in
    return
      (A.fold_with_extent (A.N (N.IfThenElse, [ [], cond; [], then_branch; [], else_branch ])) (A.get_extent_some expr))
  | A.N (N.LetIn, [ ([], sub); ([ bnd ], body) ]) ->
    let* sub, sub_tp = synth env sub in
    let env' = extend_local_env_tm env bnd sub_tp in
    let* body = check env' body tp in
    return (A.fold_with_extent (A.N (N.LetIn, [ [], sub; [ bnd ], body ])) (A.get_extent_some expr))
  | A.N (N.RecLetIn, [ ([], sub_tp); ([ bnd_sub ], sub); ([ bnd_body ], body) ]) ->
    let* sub_tp = check_type_valid env sub_tp in
    let env_sub = extend_local_env_tm env bnd_sub sub_tp in
    let* sub = check env_sub sub sub_tp in
    let env_body = extend_local_env_tm env bnd_body sub_tp in
    let* body = check env_body body tp in
    return
      (A.fold_with_extent
         (A.N (N.RecLetIn, [ [], sub_tp; [ bnd_sub ], sub; [ bnd_body ], body ]))
         (A.get_extent_some expr))
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
  | A.N (N.Sequence Dot, args) ->
    (match A.view tp_normalized with
     | A.N (N.Sequence Comma, args_tps) ->
       if List.length args <> List.length args_tps
       then
         pfail_with_ext
           ("TC142: Expecting the same number of arguments and types but got " ^ A.show_view tp_normalized)
           (A.get_extent_some expr)
       else
         let* args_checked =
           psequence
             (List.map
                (fun ((_, arg), (_, arg_tp)) ->
                   let* arg = check env arg arg_tp in
                   return ([], arg))
                (ListUtil.zip args args_tps))
         in
         let new_args = A.fold_with_extent (A.N (N.Sequence Dot, args_checked)) (A.get_extent_some expr) in
         return new_args
     | _ -> default_synth_then_unify ())
  | A.N (N.Match, ([], scrut) :: cases) ->
    let* scrut, scrut_tp = synth env scrut in
    let* cases =
      psequence
        (List.map
           (fun (_, case) ->
              match A.view case with
              | A.N (N.MatchCase, [ ([], pat); ([], body) ]) ->
                let* env, pat, body = check_pattern env pat scrut_tp body in
                let* body = check env body tp in
                return ([], A.fold_with_extent (A.N (N.MatchCase, [ [], pat; [], body ])) (A.get_extent_some case))
              | _ ->
                pfail_with_ext ("TC135: Expecting a match case but got " ^ A.show_view case) (A.get_extent_some expr))
           cases)
    in
    return (A.fold_with_extent (A.N (N.Match, [ [], scrut ] @ cases)) (A.get_extent_some expr))
  | _ -> default_synth_then_unify ()

and pattern_fill_implicit_args (pat : A.t) (pat_tp : A.t) : (A.t * A.t) proc_state_m =
  match A.view pat_tp with
  | A.N (N.ImplicitPi, [ ([ bnd ], cod) ]) ->
    let id = Uid.next () in
    let* () = add_to_global_unification_ctx id None in
    let new_var = A.fold_with_extent (A.N (N.UnifiableTp id, [])) (A.get_extent_some pat) in
    let cod = A.subst new_var bnd cod in
    (* TODO: head spine form for implicit arg apps *)
    pattern_fill_implicit_args pat cod
  | _ -> return (pat, pat_tp)

and check_pattern (env : local_env) (pat : A.t) (scrut_tp : A.t) (case_body : A.t)
  : (local_env * A.t * A.t) proc_state_m
  =
  let* scrut_tp = normalize_type scrut_tp in
  with_type_checking_history (HistTwo ("checking pattern ", pat, " against ", scrut_tp))
  @@
  match A.view pat with
  | A.N (N.Builtin N.Unit, [])
  | A.N (N.Builtin (String _), [])
  | A.N (N.Builtin (Int _), [])
  | A.N (N.Builtin (Bool _), []) ->
    let* pat = check env pat scrut_tp in
    return (env, pat, case_body)
  | A.FreeVar name ->
    let* id = Environment.find_binding name in
    (match List.assoc_opt name env.tm, id with
     | Some _, _ (* local var, shadowing*) | None, None ->
       (* fresh var *)
       (* this is a new var*)
       let* id =
         Environment.add_constant
           (PatternVar { tp = scrut_tp; name = Ext.str_with_extent name (A.get_extent_some pat) })
       in
       let pat = A.fold_with_extent (A.N (N.Constant id, [])) (A.get_extent_some pat) in
       let case_body = A.subst pat name case_body in
       return (env, pat, case_body)
     | None, Some id ->
       (* this is an existing constant *)
       let pat = A.fold_with_extent (A.N (N.Constant id, [])) (A.get_extent_some pat) in
       check_pattern env pat scrut_tp case_body)
  | A.N (N.Constant id, []) ->
    let* tp_constant = Environment.lookup_constant id in
    (match tp_constant with
     | DataConstructor { tp = pat_tp; _ } ->
       let* pat, pat_tp = pattern_fill_implicit_args pat pat_tp in
       let* _ = type_unify pat [] pat_tp scrut_tp in
       return (env, pat, case_body)
     | _ ->
       pfail_with_ext
         ("TC139: Expecting a data constructor but got " ^ EngineDataPrint.show_t_constant tp_constant)
         (A.get_extent_some pat))
  | A.N (N.Sequence Dot, args) ->
    (match A.view scrut_tp with
     | A.N (N.Sequence Comma, args_tps) ->
       if List.length args <> List.length args_tps
       then
         pfail_with_ext
           ("TC142: Expecting the same number of arguments and types but got "
            ^ A.show_view scrut_tp
            ^ " for pattern "
            ^ A.show_view pat)
           (A.get_extent_some pat)
       else
         let* env, args, case_body =
           pfold_left
             (fun (env, acc, case_body) ((_, arg), (_, arg_tp)) ->
                let* env, arg, case_body = check_pattern env arg arg_tp case_body in
                return (env, acc @ [ [], arg ], case_body))
             (env, [], case_body)
             (ListUtil.zip args args_tps)
         in
         return (env, A.fold_with_extent (A.N (N.Sequence Dot, args)) (A.get_extent_some pat), case_body)
     | _ ->
       pfail_with_ext ("TC143: Expecting a sequence of comma but got " ^ A.show_view scrut_tp) (A.get_extent_some pat))
  | A.N (N.Ap, [ ([], f); ([], arg) ]) ->
    let* id =
      match A.view f with
      | A.FreeVar name ->
        let* id = Environment.find_binding name in
        (match List.assoc_opt name env.tm, id with
         | Some _, _ (* local var, shadowing*) | None, None ->
           pfail_with_ext ("TC140: Unexpected local/fresh variable but got " ^ A.show_view f) (A.get_extent_some pat)
         | None, Some id -> return id)
      | A.N (N.Constant id, []) -> return id
      | _ ->
        pfail_with_ext
          ("TC374: Expecting a free variable in head position but got " ^ A.show_view pat)
          (A.get_extent_some pat)
    in
    (* this is an existing constant *)
    let* tp_constant = Environment.lookup_constant id in
    (match tp_constant with
     | DataConstructor { tp = pat_tp; _ } ->
       let pat = A.fold_with_extent (A.N (N.Constant id, [])) (A.get_extent_some pat) in
       let* pat, pat_tp = pattern_fill_implicit_args pat pat_tp in
       (match A.view pat_tp with
        | A.N (N.Arrow, [ ([], dom); ([], cod) ]) ->
          let* _ = type_unify pat [] scrut_tp cod in
          let* env, arg, case_body = check_pattern env arg dom case_body in
          let f = A.fold_with_extent (A.N (N.Constant id, [])) (A.get_extent_some f) in
          let pat = A.fold_with_extent (A.N (N.Ap, [ [], f; [], arg ])) (A.get_extent_some pat) in
          return (env, pat, case_body)
        | _ ->
          pfail_with_ext
            ("TC141: Expecting head's type to be an arrow but got " ^ A.show_view pat_tp)
            (A.get_extent_some pat))
     | _ ->
       pfail_with_ext
         ("TC139: Expecting a data constructor but got " ^ EngineDataPrint.show_t_constant tp_constant)
         (A.get_extent_some pat))
  | _ -> pfail_with_ext (__LOC__ ^ "TC137: Expecting a pattern but got " ^ A.show_view pat) (A.get_extent_some pat)
;;

let assert_no_free_vars (tp : A.t) : unit proc_state_m =
  with_type_checking_history (HistOne ("asserting no free vars in ", tp))
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
  with_type_checking_history (HistOne ("checking type is valid: ", tp))
  @@
  let* checked_tp = check_type_valid empty_local_env tp in
  let* () = assert_no_free_vars checked_tp in
  return checked_tp
;;

let check_data_constructor_type_valid_top (tp : A.t) : (A.t * int) proc_state_m =
  with_type_checking_history (HistOne ("checking data constructor type is valid: ", tp))
  @@
  let* checked_tp, id = check_data_constructor_type_valid empty_local_env tp in
  let* () = assert_no_free_vars checked_tp in
  return (checked_tp, id)
;;

let check_top (expr : A.t) (tp : A.t) : A.t proc_state_m =
  with_type_checking_history (HistTwo ("checking ", expr, " against ", tp)) @@ check empty_local_env expr tp
;;

let synth_top (expr : A.t) : (A.t * A.t) proc_state_m =
  with_type_checking_history (HistOne ("synthesizing ", expr)) @@ synth empty_local_env expr
;;
