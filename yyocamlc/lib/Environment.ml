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

let get_next_uid () : int proc_state_m =
  let* s = get_proc_state () in
  return (List.length s.constants)
;;

(* Add a constant - no duplicates allowed *)
let add_constant (const : t_constant) : int proc_state_m =
  let* s = get_proc_state () in
  let* uid = get_next_uid () in
  if List.mem_assoc uid s.constants
  then pfail ("Duplicate constant with uid: " ^ string_of_int uid)
  else (
    let new_constants = (uid, const) :: s.constants in
    let* () = write_proc_state { s with constants = new_constants } in
    return uid)
;;

(* Update the term for an Expression constant *)
let update_constant_term (uid : int) (tm : A.t) : unit proc_state_m =
  let* s = get_proc_state () in
  let rec update = function
    | [] -> None
    | (id, const) :: rest when id = uid ->
      (match const with
       | DataExpression { tp; tm = _ } -> Some ((id, DataExpression { tp; tm = Some tm }) :: rest)
       | _ -> None)
    | binding :: rest ->
      (match update rest with
       | Some updated -> Some (binding :: updated)
       | None -> None)
  in
  match update s.constants with
  | Some new_constants -> write_proc_state { s with constants = new_constants }
  | None -> pfail ("Cannot update constant with uid " ^ string_of_int uid ^ ": not found or not an Expression")
;;

(* Find a constant - returns option *)
let find_constant (uid : int) : t_constant option proc_state_m =
  let* s = get_proc_state () in
  return (List.assoc_opt uid s.constants)
;;

(* Lookup a constant - fails if not found *)
let lookup_constant (uid : int) : t_constant proc_state_m =
  let* result = find_constant uid in
  match result with
  | Some const -> return const
  | None -> pfail ("Constant not found with uid: " ^ string_of_int uid)
;;

(* Check if a constant exists *)
let constant_exists (uid : int) : bool proc_state_m =
  let* s = get_proc_state () in
  return (List.mem_assoc uid s.constants)
;;
(*
   let wrap_store (f : t -> 'a * t) : 'a proc_state_m = 
  fun s -> 
    let (r, new_env) = f s.store in
    let new_s = {s with store = new_env} in
    Some (r, new_s)

let wrap_store_simple (f : t -> t ) : unit proc_state_m = 
  wrap_store (fun s -> ((), f s)) *)

(*
   let push_env_state_stack (expect : expect) : unit proc_state_m = 
  wrap_store_simple (fun e -> {e with
    expect_state_stack = expect :: e.expect_state_stack;
  })

let pop_env_state_stack () : expect option proc_state_m =
  wrap_store (fun (e : t) ->
  match e.expect_state_stack with
  | [] -> (None, e)
  | x :: xs -> (Some x, {e with expect_state_stack = xs})
  ) *)
