open EngineData
open! ProcCombinators
module Ext = AbtLib.Extent

let default_env : t_env = []
let default_constants : t_constants = []

(* Environment manipulation using get_proc_state and write_proc_state *)

(* Add a binding to the environment - duplicates allowed, most recent takes precedence *)
let add_binding (name : Ext.t_str) (tp : int) : unit proc_state_m =
  let* s = get_proc_state () in
  let new_env = (name, tp) :: s.env in
  write_proc_state { s with env = new_env }
;;

(* Find a binding - returns option *)
let find_binding (name : string) : int option proc_state_m =
  let* s = get_proc_state () in
  match List.find_opt (fun (n, _) -> Ext.get_str_content n = name) s.env with
  | Some (_, tp) -> return (Some tp)
  | None -> return None
;;

(* Lookup a binding - fails if not found *)
let lookup_binding (name : string) : int proc_state_m =
  let* result = find_binding name in
  match result with
  | Some binding -> return binding
  | None -> pfail ("Binding not found: " ^ name)
;;

let add_constant_with_uid (uid : int) (const : t_constant) : unit proc_state_m =
  let* s = get_proc_state () in
  if List.mem_assoc uid s.constants
  then
    pfail
      ("Duplicate constant with uid: " ^ string_of_int uid ^ " and constant: " ^ EngineDataPrint.show_t_constant const)
  else (
    let new_constants = (uid, const) :: s.constants in
    write_proc_state { s with constants = new_constants })
;;

(* Add a constant - no duplicates allowed *)
let add_constant (const : t_constant) : int proc_state_m =
  let uid = Uid.next () in
  let* () = add_constant_with_uid uid const in
  return uid
;;

let update_constant (uid : int) (update : t_constant -> t_constant) : unit proc_state_m =
  let* s = get_proc_state () in
  let rec update_list = function
    | [] -> None
    | (id, const) :: rest when id = uid -> Some ((id, update const) :: rest)
    | binding :: rest ->
      (match update_list rest with
       | Some updated -> Some (binding :: updated)
       | None -> None)
  in
  match update_list s.constants with
  | Some new_constants -> write_proc_state { s with constants = new_constants }
  | None -> pfail ("Cannot update constant with uid " ^ string_of_int uid ^ ": not found")
;;

(* Update the term for an Expression constant *)
let update_constant_term (uid : int) (tm : A.t) : unit proc_state_m =
  update_constant uid (fun const ->
    match const with
    | DataExpression { tp; tm = _ } -> DataExpression { tp; tm = Some tm }
    | _ -> Fail.failwith (__LOC__ ^ ": Expecting data expression, got: " ^ EngineDataPrint.show_t_constant const))
;;

let update_constant_ocaml_bind_name (uid : int) (ocaml_bind_name : string) : unit proc_state_m =
  update_constant uid (fun const ->
    match const with
    | DataConstructor { name; tp; tp_id; _ } ->
      DataConstructor { tp; tp_id; ocaml_bind_name = Some ocaml_bind_name; name }
    | TypeConstructor { name; tp; _ } -> TypeConstructor { tp; ocaml_bind_name = Some ocaml_bind_name; name }
    | _ -> Fail.failwith (__LOC__ ^ ": Expecting data constructor, got: " ^ EngineDataPrint.show_t_constant const))
;;

(* Find a constant - returns option *)
let find_constant (uid : int) : t_constant option proc_state_m =
  let* s = get_proc_state () in
  match List.assoc_opt uid s.constants with
  | Some const -> return (Some const)
  | None ->
    (* look in other files*)
    (match List.assoc_opt uid (CompilationCache.get_all_constants ()) with
     | Some const -> return (Some const)
     | None -> return None)
;;

(* Lookup a constant - fails if not found *)
let lookup_constant (uid : int) : t_constant proc_state_m =
  let* result = find_constant uid in
  match result with
  | Some const -> return const
  | None -> pfail ("Constant not found with uid: " ^ string_of_int uid)
;;
(*
   let import_constants (constants : t_constants) : unit proc_state_m =
  let* _ = psequence (List.map (fun (id, const) -> add_constant_with_uid id const) constants) in
  return ()
;; *)
