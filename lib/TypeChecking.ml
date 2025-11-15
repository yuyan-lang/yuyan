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
  else Fail.failwith ("uid " ^ string_of_int uid ^ " not found in global unification context")
;;

(* Until binding contains extent, we need to resort to string.
AbtLib did not design bindings to contain extents.
*)
type bnd_name = Ext.t_str
(* type bnd_name = string *)

let bnd_name_to_string (name : bnd_name) : string = Ext.get_str_content name

(* Bidirectional type checking skeleton *)
type local_tm_env = (bnd_name * A.t) list
type local_env = local_tm_env

let empty_local_env : local_env = []
let extend_local_env_tm (env : local_env) (name : bnd_name) (tp : A.t) : local_env = (name, tp) :: env

let extend_local_env_tp (env : local_env) (name : bnd_name) : local_env =
  extend_local_env_tm env name (A.fold_with_extent (A.N (N.Builtin N.Type, [])) (Ext.get_str_extent name))
;;

let extend_local_env_tm_with_token_info (env : local_env) (name : bnd_name) (tp : A.t) : local_env proc_state_m =
  let* () =
    let* aka_print_tp = aka_print_expr tp in
    TokenInfo.add_token_info name (Hover aka_print_tp)
  in
  return (extend_local_env_tm env name tp)
;;

let find_in_local_env_tm (env : local_env) (name : bnd_name) : (Ext.t * A.t) option =
  match List.find_opt (fun (n, _) -> bnd_name_to_string n = Ext.get_str_content name) env with
  | Some (bnd_name, tp) -> Some (Ext.get_str_extent bnd_name, tp)
  | None -> None
;;

let check_is_type (tp : A.t) : A.t proc_state_m =
  match A.view tp with
  | A.N (N.Builtin N.Type, _) -> return tp
  | _ -> pfail_with_ext "Expecting type but got " (A.get_extent_some tp)
;;

let find_in_local_env_tp (env : local_env) (name : bnd_name) : bnd_name option proc_state_m =
  match List.find_opt (fun (n, _) -> bnd_name_to_string n = Ext.get_str_content name) env with
  | Some (bnd_name, tp) ->
    let* _ = check_is_type tp in
    return (Some bnd_name)
  | None -> return None
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
        | DataExpression _ | ModuleAlias _ ->
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
  | A.FreeVar name1, A.FreeVar name2 when Ext.get_str_content name1 = Ext.get_str_content name2 -> return free_var_names
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
  (* let* normalized_tp = normalize_type tp in *)
  with_type_checking_history (HistOne ("checking type is valid ", tp))
  @@
  match A.view tp with
  | A.FreeVar name ->
    (* TODO: The extent of operands are wrong due to substitution *)
    let* bnd_name = find_in_local_env_tp env name in
    if Option.is_some bnd_name
    then return tp
    else
      let* id = Environment.find_binding name in
      (match id with
       | None ->
         pfail_with_ext
           ("TC98: Free variable not found in the environment: " ^ Ext.get_str_content name)
           (A.get_extent_some tp)
       | Some id ->
         let* extent = Environment.get_extent_of_constant id in
         let* () = TokenInfo.add_token_info name (Definition extent) in
         let* tp_constant = Environment.lookup_constant id in
         (match tp_constant with
          | DataExpression { tp; tm; _ } ->
            (match A.view tp with
             | A.N (N.Builtin N.Type, []) -> return tm
             | _ -> pfail_with_ext (__LOC__ ^ "TC28: Expecting type but got " ^ A.show_view tp) (A.get_extent_some tp))
          | ModuleAlias _ ->
            pfail_with_ext (__LOC__ ^ "TC28: Expecting type but got " ^ A.show_view tp) (A.get_extent_some tp)))
  | A.N (N.Builtin N.Type, []) -> return tp
  | A.N (N.Builtin N.StringType, []) -> return tp
  | A.N (N.Builtin N.IntType, []) -> return tp
  | A.N (N.Builtin N.BoolType, []) -> return tp
  | A.N (N.Builtin N.UnitType, []) -> return tp
  | A.N (N.Builtin N.FloatType, []) -> return tp
  | A.N (N.Builtin N.RefType, []) -> return tp
  | A.N (N.Builtin N.ArrayRefType, []) -> return tp
  | A.N (N.Arrow, [ ([], dom); ([], cod) ]) ->
    let* dom = check_type_valid env dom in
    let* cod = check_type_valid env cod in
    return (A.fold_with_extent (A.N (N.Arrow, [ [], dom; [], cod ])) (A.get_extent_some tp))
  | A.N (N.ExplicitPi, [ ([], dom); ([ bnd_name ], cod) ]) ->
    let* dom = check_type_valid env dom in
    let env' = extend_local_env_tp env bnd_name in
    let* cod = check_type_valid env' cod in
    return (A.fold_with_extent (A.N (N.ExplicitPi, [ [], dom; [ bnd_name ], cod ])) (A.get_extent_some tp))
  | A.N (N.Ap, [ ([], f); ([], _arg) ]) ->
    let* id =
      match A.view f with
      | A.FreeVar name -> Environment.lookup_binding_with_extent_token_info name
      | A.N (N.Constant id, []) -> return id
      | _ -> pfail_with_ext (__LOC__ ^ "TC140: Expecting free variable but got " ^ A.show_view f) (A.get_extent_some tp)
    in
    let* _tp_constant = Environment.lookup_constant id in
    pfail_with_ext (__LOC__ ^ "TC141: Type constructors no longer supported") (A.get_extent_some tp)
  | A.N (N.Constant id, []) ->
    let* tp_constant = Environment.lookup_constant id in
    (match tp_constant with
     | DataExpression _ | ModuleAlias _ ->
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
  | _ -> pfail_with_ext (__LOC__ ^ "TC26: Expecting type but got " ^ A.show_view tp) (A.get_extent_some tp)
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
  | DataExpression { tp; _ } -> return tp
  | _ -> Fail.failwith (__LOC__ ^ " Expecting data expression but got " ^ EngineDataPrint.show_t_constant const)
;;

let partial_resolve_structure_deref (_env : local_env) (expr : A.t) : int proc_state_m =
  match A.view expr with
  | A.FreeVar name ->
    let* id = Environment.lookup_binding_with_extent_token_info name in
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
       let* () = TokenInfo.add_token_info deref_name (Hover filepath) in
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
                       when Ext.get_str_content name = Ext.get_str_content deref_name -> Some id
                     | A.N (N.Declaration (N.ModuleAliasDefn (name, _)), _)
                       when Ext.get_str_content name = Ext.get_str_content deref_name ->
                       failwith "TODO double module alias deref"
                     | _ -> None)
                  (List.rev args)
              with
              | id :: _ ->
                let ret_expr = A.fold_with_extent (A.N (N.Constant id, [])) (A.get_extent_some expr) in
                let* ret_tp = get_tp_for_expr_id id in
                let* extent = Environment.get_extent_of_constant id in
                let* () = TokenInfo.add_token_info deref_name (Definition extent) in
                let* () =
                  let* aka_print_tp = aka_print_expr ret_tp in
                  TokenInfo.add_token_info deref_name (Hover aka_print_tp)
                in
                return (ret_expr, ret_tp)
              | _ ->
                pfail_with_ext
                  (__LOC__ ^ " Name not " ^ Ext.get_str_content deref_name ^ " not found in " ^ filepath)
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
       (match find_in_local_env_tm env name with
        | Some (target_ext, tp) ->
          let* () =
            let* aka_print_tp = aka_print_expr tp in
            TokenInfo.add_token_info name (Hover aka_print_tp)
          in
          let* () = TokenInfo.add_token_info name (Definition target_ext) in
          return (expr, tp)
        | None ->
          let* id = Environment.lookup_binding_with_extent_token_info name in
          let* tp_constant = Environment.lookup_constant id in
          let expr = A.fold_with_extent (A.N (N.Constant id, [])) (A.get_extent_some expr) in
          (match tp_constant with
           | DataExpression { tp; _ } ->
             let* () =
               let* aka_print_tp = aka_print_expr tp in
               TokenInfo.add_token_info name (Hover aka_print_tp)
             in
             return (expr, tp)
           | _ ->
             pfail_with_ext
               (__LOC__ ^ "TC84: Expecting data expression but got " ^ EngineDataPrint.show_t_constant tp_constant)
               (A.get_extent_some expr)))
     | A.N (N.Builtin N.Type, [])
     | A.N (N.Builtin N.IntType, [])
     | A.N (N.Builtin N.FloatType, [])
     | A.N (N.Builtin N.UnitType, [])
     | A.N (N.Builtin N.BoolType, [])
     | A.N (N.Builtin N.StringType, []) ->
       return (expr, A.fold_with_extent (A.N (N.Builtin N.Type, [])) (A.get_extent_some expr))
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
       let* env' = extend_local_env_tm_with_token_info env bnd dom in
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
     | A.N (N.Ap, [ ([], f); ([], arg) ]) ->
       let* f, f_tp = synth env f in
       (match A.view f_tp with
        | A.N (N.Arrow, [ ([], dom); ([], cod) ]) ->
          let* arg = check env arg dom in
          return (A.fold_with_extent (A.N (N.Ap, [ [], f; [], arg ])) (A.get_extent_some expr), cod)
        | _ ->
          pfail_with_ext ("TC134: Expecting its type to be an arrow but got " ^ A.show_view f_tp) (A.get_extent_some f))
     | A.N (N.Constant id, []) ->
       let* tp_constant = Environment.lookup_constant id in
       (match tp_constant with
        | DataExpression { tp; name = Some name; _ } ->
          let* () =
            let* aka_print_tp = aka_print_expr tp in
            TokenInfo.add_token_info
              (Ext.str_with_extent (Ext.get_str_content name) (A.get_extent_some expr))
              (Hover aka_print_tp)
          in
          return (expr, tp)
        | DataExpression { tp; name = None; _ } -> return (expr, tp)
        | _ ->
          pfail_with_ext
            (__LOC__ ^ "TC331: Expecting data expression but got " ^ EngineDataPrint.show_t_constant tp_constant)
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
  | _, _ -> check_after_filling_implicit_lam env expr expr_tp

(* Check expression against a type *)
and check_after_filling_implicit_lam (env : local_env) (expr : A.t) (tp : A.t) : A.t proc_state_m =
  with_type_checking_history (HistTwo ("checking ", expr, " against ", tp))
  @@
  let* tp_normalized = normalize_type tp in
  let default_synth_then_unify () =
    let* synth_expr, synth_tp = synth env expr in
    (* we can assume tp is not implicit pi as it has been filled, and expr is not implicit lam as this was a previosu case *)
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
    let* env' = extend_local_env_tm_with_token_info env bnd sub_tp in
    let* body = check env' body tp in
    return (A.fold_with_extent (A.N (N.LetIn, [ [], sub; [ bnd ], body ])) (A.get_extent_some expr))
  | A.N (N.Lam, [ ([ bnd ], body) ]) ->
    (match A.view tp_normalized with
     | A.N (N.Arrow, [ ([], dom); ([], cod) ]) ->
       let* env' = extend_local_env_tm_with_token_info env bnd dom in
       let* checked_body = check env' body cod in
       return (A.fold_with_extent (A.N (N.Lam, [ [ bnd ], checked_body ])) (A.get_extent_some expr))
     | A.N (N.ExplicitPi, [ ([], dom); ([ bnd_name ], cod) ]) ->
       let* env' = extend_local_env_tm_with_token_info env bnd dom in
       let cod = A.subst (A.free_var bnd) (Ext.get_str_content bnd_name) cod in
       let* checked_body = check env' body cod in
       return (A.fold_with_extent (A.N (N.Lam, [ [ bnd ], checked_body ])) (A.get_extent_some expr))
     | _ ->
       pfail_with_ext
         (__LOC__ ^ "TC133: Expecting its type to be an arrow but got " ^ A.show_view tp_normalized)
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
  | _ -> default_synth_then_unify ()

and pattern_fill_implicit_args (pat : A.t) (pat_tp : A.t) : (A.t * A.t) proc_state_m =
  match A.view pat_tp with
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
    (match find_in_local_env_tm env name, id with
     | Some _, _ (* local var, shadowing*) | None, None ->
       (* fresh var *)
       (* this is a new var - use DataExpression for pattern variables now *)
       let* id =
         Environment.add_constant (DataExpression { tp = scrut_tp; name = Some name; tm = A.fold (A.FreeVar name) })
       in
       let pat = A.fold_with_extent (A.N (N.Constant id, [])) (A.get_extent_some pat) in
       let case_body = A.subst pat (Ext.get_str_content name) case_body in
       return (env, pat, case_body)
     | None, Some id ->
       (* this is an existing constant *)
       let* extent = Environment.get_extent_of_constant id in
       let* () = TokenInfo.add_token_info name (Definition extent) in
       let pat = A.fold_with_extent (A.N (N.Constant id, [])) (A.get_extent_some pat) in
       check_pattern env pat scrut_tp case_body)
  | A.N (N.Constant id, []) ->
    let* _tp_constant = Environment.lookup_constant id in
    pfail_with_ext "TC139: Data constructors no longer supported" (A.get_extent_some pat)
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
  | A.N (N.Ap, [ ([], f); ([], _arg) ]) ->
    let* id =
      match A.view f with
      | A.FreeVar name ->
        let* id = Environment.find_binding name in
        (match find_in_local_env_tm env name, id with
         | Some _, _ (* local var, shadowing*) | None, None ->
           pfail_with_ext ("TC140: Unexpected local/fresh variable but got " ^ A.show_view f) (A.get_extent_some pat)
         | None, Some id ->
           let* extent = Environment.get_extent_of_constant id in
           let* () = TokenInfo.add_token_info name (Definition extent) in
           return id)
      | A.N (N.Constant id, []) -> return id
      | _ ->
        pfail_with_ext
          ("TC374: Expecting a free variable in head position but got " ^ A.show_view pat)
          (A.get_extent_some pat)
    in
    (* this is an existing constant *)
    let* _tp_constant = Environment.lookup_constant id in
    pfail_with_ext "TC139: Data constructors no longer supported in patterns" (A.get_extent_some pat)
  | _ -> pfail_with_ext (__LOC__ ^ "TC137: Expecting a pattern but got " ^ A.show_view pat) (A.get_extent_some pat)
;;

let assert_no_free_vars (tp : A.t) : unit proc_state_m =
  with_type_checking_history (HistOne ("asserting no free vars in ", tp))
  @@
  match A.get_free_vars tp with
  | [] -> return ()
  | free_vars ->
    Fail.failwith
      ("TC_assert_no_free_vars: free variables found in type: "
       ^ String.concat ", " free_vars
       ^ " in type: "
       ^ A.show_view tp)
;;

let check_top (expr : A.t) (tp : A.t) : A.t proc_state_m =
  with_type_checking_history (HistTwo ("checking ", expr, " against ", tp)) @@ check empty_local_env expr tp
;;

let synth_top (expr : A.t) : (A.t * A.t) proc_state_m =
  with_type_checking_history (HistOne ("synthesizing ", expr)) @@ synth empty_local_env expr
;;
