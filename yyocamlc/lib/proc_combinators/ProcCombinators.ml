
open EngineData

module Ext = AbtLib.Extent
module CS = CharStream
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

(* pnot m fails if m succeeds, succeeds without consuming inputs when m fails *)
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


let choice (m1 : 'a proc_state_m) (m2 : 'a proc_state_m) : 'a proc_state_m = 
  fun s -> 
    match m1 s with
    | None -> m2 s
    | Some (x, s') -> Some (x, s')

let choice_l (ms : 'a proc_state_m list) : 'a proc_state_m = 
  List.fold_left choice (returnNone ()) ms

let to_processor_complex (env : expect) (name : string) (process : 'a proc_state_m) : processor = 
  ProcComplex { expect = env;
    name=name;
   process = process;
  }


let to_processor_complex_list (envs : expect list) (name :string) (process : 'a proc_state_m) : processor list = 
  List.map (fun env -> to_processor_complex env name process) envs

let get_proc_state () : proc_state proc_state_m = 
  fun s -> 
    Some (s, s)

let update_proc_state (f : proc_state -> proc_state) : unit proc_state_m = 
  fun s -> 
    let new_s = f s in
    Some ((), new_s)

let write_proc_state (s : proc_state) : unit proc_state_m = 
  fun _ -> 
    Some ((), s)

(* reading inputs *)

let read_any_char () : (CS.t_char * Ext.t) proc_state_m = 
  fun s -> 
    match CharStream.get_next_char s.input_future with
    | None -> None
    | Some (c, next_cs) -> 
        let new_s = {s with input_future = next_cs} in
        let cs_t_char = CharStream.new_t_char (Ext.get_str_content c) in
        Some ((cs_t_char, Ext.get_str_extent c), new_s)


let push_scanned_char (c : CS.t_char * Ext.t) : unit proc_state_m = 
  fun s -> 
    let new_s = {s with input_acc = PE.get_scanned_char_t c :: s.input_acc} in
    Some ((), new_s)

type t_char = CharStream.t_char
let read_one_of_char (l : t_char list) : (CS.t_char * Ext.t) proc_state_m = 
  let* (c', ext) = read_any_char () in 
  if List.mem c' l then 
    return (c', ext)
  else 
    returnNone ()

(* string is a list of *)
let read_string (l : t_char list) : (CS.t_string * Ext.t) proc_state_m = 
  if l = [] then failwith "ET100: empty string" else
  let rec aux acc rem = 
    match rem with
    | [] -> return (List.map fst acc, Ext.combine_extent_list (List.map snd acc))
    | c :: cs -> 
        let* read_c = read_one_of_char [c] in
        aux (acc@[read_c]) cs
  in
  aux [] l


let read_one_of_string (l : CS.t_string list) : (CS.t_string * Ext.t) proc_state_m = 
  choice_l (List.map read_string l)

let read_any_char_except (except : CS.t_char list) : (CS.t_char * Ext.t) proc_state_m = 
  let* (c, ext) = read_any_char () in
  if List.mem (c) except then 
    returnNone ()
  else 
    return (c, ext)

let read_any_char_except_and_push (except : CS.t_char list) : unit proc_state_m = 
  read_any_char_except except >>= push_scanned_char

let remap_t_char_list_with_ext (c : (CS.t_char * Ext.t) list) : CS.t_string * Ext.t = 
  (List.map fst c, Ext.combine_extent_list (List.map snd c))

let scan_past_one_of_char (l : CS.t_char list) : ((CS.t_string *Ext.t) (* intermediate *) * (CS.t_char * Ext.t) (* one of char in l*)) proc_state_m = 
  let rec aux acc = 
    let* (c, ext) = read_any_char () in
    if List.mem (c) l then 
      return ((remap_t_char_list_with_ext acc), (c, ext))
    else 
      aux (acc@[(c, ext)])
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
  let* cur_state = get_expect_state () in
  let* _ = modify_s (fun s -> {s with input_expect = new_state; expect_state_stack = cur_state::s.expect_state_stack}) in
  return ()

let pop_expect_state () : expect proc_state_m =
  let* st = get_proc_state () in
  match st.expect_state_stack with
  | [] -> pfail "pop_expect_state: empty stack"
  | x::tail -> 
      let* _ = modify_s (fun s -> {s with input_expect = x; expect_state_stack = tail}) in
      return x

let pop_input_acc () : PE.t proc_state_m =
  fun s -> 
    match s.input_acc with
    | [] -> None
    | x :: xs -> 
        let new_s = {s with input_acc = xs} in
        Some (x, new_s)

let pop_input_acc_2 () : (PE.t * PE.t) proc_state_m =
  let* x = pop_input_acc () in
  let* y = pop_input_acc () in
  return (x, y)

let pop_input_acc_3 () : (PE.t * PE.t * PE.t) proc_state_m =
  let* x = pop_input_acc () in
  let* y = pop_input_acc () in
  let* z = pop_input_acc () in
  return (x, y, z)

let pop_bin_operand (binop : binary_op) : (PE.t * PE.t) proc_state_m =
  let* (x, y, z) = pop_input_acc_3 () in
  match A.view y with
  | A.N(N.ParsingElem(N.OpKeyword(opid)), []) -> 
    if binop.id = opid then
      return (x, z)
    else
      pfail ("pop_bin_operand: expected " ^ (string_of_int opid) ^ " but got " ^ (string_of_int binop.id))
  | _ -> 
    pfail ("pop_bin_operand: expected OpKeyword but got " ^ (A.show_view y))

let assertb (b : bool) : unit proc_state_m = 
  if b then return () else pfail "assertb: assertion failed"

let pop_input_acc_past (f : PE.t -> bool) : (PE.t list * PE.t) proc_state_m =
  let rec aux acc = 
    let* x = pop_input_acc () in
    if f x then
      return (acc, x)
    else
      aux (x :: acc)
  in
  aux []

let get_current_file_name () : string proc_state_m = 
  let* s = get_proc_state () in
  return s.input_future.filename

let run_processor (proc : processor)  : unit proc_state_m = 
  let* proc_state = get_proc_state () in
  match proc with
  | ProcComplex {expect; name=_; process} -> 
    if expect <> proc_state.input_expect 
    then returnNone ()
    else
    (
      let* _ = process in
      return ()
    )
  | ProcBinOp { id=_;keyword;left_precedence=_;right_precedence=_;fixity=_;reduction=_} ->
    if proc_state.input_expect <> Expression then returnNone ()
    else
      let* read_keyword = read_string keyword in
      push_elem_on_input_acc (PE.get_keyword_t read_keyword);
  | ProcIdentifier id -> 
    if proc_state.input_expect <> Expression then returnNone ()
    else 
      let* string_read = read_string id in
      push_elem_on_input_acc (PE.get_identifier_t string_read)


