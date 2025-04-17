
open EngineData

module Ext = AbtLib.Extent
module PE = ProcessedElement

let return (x : 'a) : 'a proc_state_m = 
    fun s -> Some (x, s)

let returnNone () : 'a proc_state_m = 
    fun _ -> None

let pfail (_msg : string) : 'a proc_state_m = 
    fun _ -> None

let ignore () : unit proc_state_m = 
    fun s -> 
      Some ((), s)

let pnot (m : 'a proc_state_m) : unit proc_state_m = 
    fun s -> 
      match m s with
      | None -> Some ((), s)
      | Some (_, _) -> None

let bind  (m : 'a proc_state_m) (f: 'a -> 'b proc_state_m) : 'b proc_state_m = 
    fun s -> 
      match m s with
      | None -> None
      | Some (x, s') -> f x s'

let (>>=) = bind
let (let*) m f = bind m f


let _then (m : 'a proc_state_m) (f: 'b proc_state_m) : 'b proc_state_m = 
  fun s ->
      match m s with
      | None -> None
      | Some (_, s') -> f s'

let (>>) = _then

let to_processor (env : expect) (name : string) (process : 'a proc_state_m) : processor = 
  { expect = env;
    name=name;
   process = fun s -> match process s with | None -> None | Some(_, s') -> Some s' }

let to_processor_list (envs : expect list) (name :string) (process : 'a proc_state_m) : processor list = 
  List.map (fun env -> to_processor env name process) envs

(* reading inputs *)

let read_any_char () : AbtLib.Extent.t_str proc_state_m = 
  fun s -> 
    match CharStream.get_next_char s.input_future with
    | None -> None
    | Some (c, next_cs) -> 
        let new_s = {s with input_future = next_cs} in
        Some (c, new_s)


let push_scanned_char (c : AbtLib.Extent.t_str) : unit proc_state_m = 
  fun s -> 
    let new_s = {s with input_acc = PE.get_scanned_char_t c :: s.input_acc} in
    Some ((), new_s)

type t_char = CharStream.t_char
let read_one_of_char (l : t_char list) : AbtLib.Extent.t_str proc_state_m = 
  read_any_char () >>= (fun c' -> 
    if List.mem (Ext.get_str_content c') l then 
      return c'
    else 
      returnNone ()
  )

(* string is a list of *)
let read_string (l : t_char list) : AbtLib.Extent.t_str proc_state_m = 
  if l = [] then failwith "ET100: empty string" else
  let rec aux acc rem = 
    match rem with
    | [] -> Ext.join_t_str_list acc |> return
    | c :: cs -> 
        let* read_c = read_one_of_char [c] in
        aux (acc@[read_c]) cs
  in
  aux [] l


let read_any_char_except (except : string list) : AbtLib.Extent.t_str proc_state_m = 
  read_any_char () >>= (fun c -> 
    if List.mem (Ext.get_str_content c) except then 
      returnNone ()
    else 
      return c
  )

let read_any_char_except_and_push (except : string list) : unit proc_state_m = 
  read_any_char_except except >>= push_scanned_char


let scan_past_one_of_char (l : string list) : (Ext.t_str list (* intermediate *) * Ext.t_str (* one of char in l*)) proc_state_m = 
  let rec aux acc = 
    let* c = read_any_char () in
    if List.mem (Ext.get_str_content c) l then 
      return (acc, c)
    else 
      aux (acc@[c])
  in
  aux []

let push_elem_on_input_acc (elem : PE.t) : unit proc_state_m = 
  fun s -> 
    let new_s = {s with input_acc = elem :: s.input_acc} in
    Some ((), new_s)

let get_expect_state () : expect proc_state_m =
  fun s -> 
    Some (s.input_expect, s)

let modify_s (f : proc_state -> proc_state) : unit proc_state_m = 
  fun s -> 
    let new_s = f s in
    Some ((), new_s)

let push_expect_state (new_state : expect) : unit proc_state_m = 
  let* _ = Environment.push_env_state_stack new_state in
  let* _ = modify_s (fun s -> {s with input_expect = new_state}) in
  return ()

let pop_expect_state () : expect proc_state_m =
  let* (exp) = Environment.pop_env_state_stack in
  match exp with
  | None -> returnNone ()
  | Some x -> 
      let* _ = modify_s (fun s -> {s with input_expect = x}) in
      return x

let pop_input_acc () : PE.t proc_state_m =
  fun s -> 
    match s.input_acc with
    | [] -> None
    | x :: xs -> 
        let new_s = {s with input_acc = xs} in
        Some (x, new_s)

let assertb (b : bool) : unit proc_state_m = 
  if b then return () else pfail "assertb: assertion failed"