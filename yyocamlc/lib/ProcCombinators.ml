
open EngineData

let return (x : 'a) : 'a proc_state_m = 
    fun s -> Some (x, s)

let returnNone : 'a proc_state_m = 
    fun _ -> None

let ignore  : unit proc_state_m = 
    fun s -> 
      Some ((), s)

let bind  (m : 'a proc_state_m) (f: 'a -> 'b proc_state_m) : 'b proc_state_m = 
    fun s -> 
      match m s with
      | None -> None
      | Some (x, s') -> f x s'

let (>>=) = bind

let _then (m : 'a proc_state_m) (f: 'b proc_state_m) : 'b proc_state_m = 
  fun s ->
      match m s with
      | None -> None
      | Some (_, s') -> f s'

let (>>) = _then

let to_processor (env : env) (process : 'a proc_state_m) : processor = 
  { env = env; process = fun s -> match process s with | None -> None | Some(_, s') -> Some s' }

let to_processor_list (envs : env list) (process : 'a proc_state_m) : processor list = 
  List.map (fun env -> to_processor env process) envs

(* reading inputs *)

let read_any_char : AbtLib.Extent.t_str proc_state_m = 
  fun s -> 
    match CharStream.get_next_char s.input_future with
    | None -> None
    | Some (c, next_cs) -> 
        let new_s = {s with input_future = next_cs} in
        Some (c, new_s)

module PE = ProcessedElement
module Ext = AbtLib.Extent

let push_unknown_char (c : AbtLib.Extent.t_str) : unit proc_state_m = 
  fun s -> 
    let new_s = {s with input_acc = PE.get_unknown_char_t c :: s.input_acc} in
    Some ((), new_s)

let read_one_of_char (l : string list) : AbtLib.Extent.t_str proc_state_m = 
  read_any_char >>= (fun c' -> 
    if List.mem (Ext.get_str_content c') l then 
      return c'
    else 
      returnNone
  )


let read_any_char_except (except : string list) : AbtLib.Extent.t_str proc_state_m = 
  read_any_char >>= (fun c -> 
    if List.mem (Ext.get_str_content c) except then 
      returnNone
    else 
      return c
  )

let read_any_char_except_and_push (except : string list) : unit proc_state_m = 
  read_any_char_except except >>= push_unknown_char


