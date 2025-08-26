open EngineData
open! ProcCombinators
module Env = Environment

(* Bidirectional type checking skeleton *)

let check_is_type (tp : A.t) : A.t proc_state_m =
  match A.view tp with
  | A.N (N.Builtin N.Type, _) -> return tp
  | _ -> pfail_with_ext "Expecting type but got " (A.get_extent_some tp)
;;

let rec check_kind_valid (tp : A.t) : A.t proc_state_m =
  match A.view tp with
  | A.N (N.Arrow, [ ([], dom); ([], cod) ]) ->
    let* checked_dom = check_is_type dom in
    let* checked_cod = check_kind_valid cod in
    return (A.fold_with_extent (A.N (N.Arrow, [ [], checked_dom; [], checked_cod ])) (A.get_extent_some tp))
  | _ -> check_is_type tp
;;

(* Check that a type is well-formed *)
let check_type_valid (tp : A.t) : A.t proc_state_m = pfail ("TC_check_type_valid: unimplemented for " ^ A.show_view tp)

(* Synthesize/infer type from an expression *)
let synth (expr : A.t) : A.t proc_state_m = pfail ("TC_synth: unimplemented for " ^ A.show_view expr)

(* Check expression against a type *)
let check (expr : A.t) (tp : A.t) : A.t proc_state_m =
  pfail ("TC_check: unimplemented for expr=" ^ A.show_view expr ^ " against type=" ^ A.show_view tp)
;;

(* Type equality check *)
let type_equal (tp1 : A.t) (tp2 : A.t) : bool proc_state_m =
  pfail ("TC_type_equal: unimplemented for " ^ A.show_view tp1 ^ " and " ^ A.show_view tp2)
;;

(* Normalize a type *)
let normalize_type (tp : A.t) : A.t proc_state_m = pfail ("TC_normalize_type: unimplemented for " ^ A.show_view tp)

(* Normalize an expression *)
let normalize_expr (expr : A.t) : A.t proc_state_m = pfail ("TC_normalize_expr: unimplemented for " ^ A.show_view expr)

let assert_no_free_vars (tp : A.t) : unit proc_state_m =
  match A.get_free_vars tp with
  | [] -> return ()
  | free_vars -> pfail ("TC_assert_no_free_vars: free variables found in type: " ^ String.concat ", " free_vars)
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
