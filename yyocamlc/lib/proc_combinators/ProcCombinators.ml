
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

let ptry (m : 'a proc_state_m) : 'a option proc_state_m = 
  fun s -> 
    match m s with
    | None -> Some(None, s)
    | Some (x, s') -> Some (Some x, s')

let assertb (b : bool) : unit proc_state_m = 
  if b then return () else pfail "assertb: assertion failed"

let choice (m1 : 'a proc_state_m) (m2 : 'a proc_state_m) : 'a proc_state_m = 
  fun s -> 
    match m1 s with
    | None -> m2 s
    | Some (x, s') -> Some (x, s')

let choice_l (ms : 'a proc_state_m list) : 'a proc_state_m = 
  List.fold_left choice (returnNone ()) ms

let to_processor_binary_op (env : expect) (name : string) (binop : binary_op) : processor_entry = 
  { expect = env;
    name=name;
   processor = ProcBinOp binop;
  }

let to_processor_identifier (env : expect) (name : string) (id : CharStream.t_string) : processor_entry = 
  { expect = env;
    name=name;
   processor = ProcIdentifier id;
  }
let to_processor_complex (env : expect) (name : string) (process : 'a proc_state_m) : processor_entry = 
  { expect = env;
    name=name;
   processor = ProcComplex process;
  }


let to_processor_complex_list (envs : expect list) (name :string) (process : 'a proc_state_m) : processor_entry list = 
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

let scan_past_one_of_string (t : CS.t_string list) : ((CS.t_string * Ext.t) (* intermediate *) * (CS.t_string * Ext.t) (* one of string in l*)) proc_state_m = 
  let rec aux acc = 
    let* result = ptry (read_one_of_string t) in
    match result with
    | Some((c, ext)) -> return ((remap_t_char_list_with_ext acc), (c, ext))
    | None -> 
      let* (c, ext) = read_any_char () in
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

let pop_expect_state (cur_state : expect) : unit proc_state_m =
  let* st = get_proc_state () in
  let* () = assertb (cur_state = st.input_expect) in
  match st.expect_state_stack with
  | [] -> pfail "pop_expect_state: empty stack"
  | x::tail -> 
      let* _ = modify_s (fun s -> {s with input_expect = x; expect_state_stack = tail}) in
      return ()

let peek_input_acc (idx : int) : PE.t option proc_state_m =
  let* s = get_proc_state () in
  if idx < List.length s.input_acc then
    return (Some (List.nth s.input_acc idx))
  else
    return None

let pop_input_acc () : PE.t proc_state_m =
  let* s = get_proc_state () in
  match s.input_acc with
  | [] -> returnNone ()
  | x :: xs -> 
      let new_s = {s with input_acc = xs} in
      let* _ = write_proc_state new_s in
      return x

let pop_input_acc_2 () : (PE.t * PE.t) proc_state_m =
  let* x = pop_input_acc () in
  let* y = pop_input_acc () in
  return (x, y)

let pop_input_acc_3 () : (PE.t * PE.t * PE.t) proc_state_m =
  let* x = pop_input_acc () in
  let* y = pop_input_acc () in
  let* z = pop_input_acc () in
  return (x, y, z)

let assert_is_correct_operand (meta : binary_op_meta) (elem : PE.t) : unit proc_state_m =
  match A.view elem with
  | A.N(N.ParsingElem(N.OpKeyword(kop)), []) -> 
    if meta.id = kop.id then
      return ()
    else
      pfail ("check_is_operand: expected " ^ (show_binary_op_meta meta) ^ " but got " ^ (show_binary_op_meta kop))
  | _ -> 
    pfail ("check_is_operand: expected OpKeyword but got " ^ (A.show_view elem))

let pop_bin_operand (binop : binary_op_meta) : ((PE.t * PE.t) * Ext.t) proc_state_m =
  let* (x, y, z) = pop_input_acc_3 () in
  let* _ = assert_is_correct_operand binop y in
  return ((x, z), Ext.combine_extent_list [A.get_extent_some x; A.get_extent_some y; A.get_extent_some z])

(* extent is the entirety of expressions *)
let pop_prefix_operand (binop : binary_op_meta) : (PE.t * Ext.t) proc_state_m =
  let* (x, y) = pop_input_acc_2 () in
  let* _ = assert_is_correct_operand binop x in
  return (y, Ext.combine_extent (A.get_extent_some x) (A.get_extent_some y))

let pop_postfix_operand (binop : binary_op_meta) : (PE.t * Ext.t) proc_state_m =
  let* (x, y) = pop_input_acc_2 () in
  let* _ = assert_is_correct_operand binop y in
  return (x, Ext.combine_extent (A.get_extent_some x) (A.get_extent_some y))

let pop_closed_identifier_operand (binop : binary_op_meta) : Ext.t proc_state_m =
  let* (y) = pop_input_acc () in
  let* _ = assert_is_correct_operand binop y in
  return (A.get_extent_some y)


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

(* pushes start elem (identifier or )
CHANGE: I decided that I still want a list of identifiers 
1. Function as applications if separator is 于 or 、 or 以 
    [The idea is that A以B、C、D = A于B于C于D]
2. Form a special sequence type if separator is ， or ；or。
*)
let push_elem_start (es : A.t) : unit proc_state_m = 
  let* acc_top = peek_input_acc 0 in
  match acc_top with
  | None -> push_elem_on_input_acc es
  | Some (x) -> 
    match A.view x with
    | A.N(N.ParsingElem(N.OpKeyword(meta)),[]) -> 
      if meta.fixity = Prefix || meta.fixity = Infix  then
        push_elem_on_input_acc es
      else
        failwith ("ET100: expected prefix or infix but got " ^ (show_binary_op_meta meta) ^ " others should be reduced directly not remain on the stack")
    | _ -> 
      pfail ("ET100: expected OpKeyword but got " ^ (A.show_view x) ^ " cannot push the next identifier")

let push_elem_continue (es : A.t) : unit proc_state_m = 
  let* acc_top = peek_input_acc 0 in
  match acc_top with
  | None -> pfail "PC243: empty stack"
  | Some (x) -> 
    match A.view x with
    | A.N(N.ParsingElem(N.OpKeyword(_)),[]) -> 
      pfail ("ET100: expected things other than but got " ^ (A.show_view x) ^ " cannot push the next identifier")
    | _ -> 
      push_elem_on_input_acc es


let lookup_binary_op (meta : binary_op_meta) : binary_op proc_state_m = 
  let* s = get_proc_state () in
  match List.filter_map (fun x -> 
    match x.processor with ProcBinOp(x) -> if x.meta.id = meta.id then Some(x) else None | _ -> None) s.registry with
  | [x] -> return x
  | _ -> pfail ("ET100: cannot find binary operator " ^ (show_binary_op_meta meta) ^ " in the registry")

(* this reduces postfix and closed operators already on the stack*)
let operator_right_most_reduce () : unit proc_state_m = 
  let* acc_top = peek_input_acc 0 in
  match acc_top with
  | None -> failwith "PC244: empty stack"
  | Some (x) -> 
    match A.view x with
    | A.N(N.ParsingElem(N.OpKeyword(meta)),[]) -> 
      if meta.fixity = Postfix || meta.fixity = ClosedIdentifier then
        let* bin_op = lookup_binary_op meta in
        bin_op.reduction
      else
        failwith ("ET100: expected postfix or closed identifier but got " ^ (show_binary_op_meta meta) ^ " others should be reduced directly not remain on the stack")
    | _ -> 
      failwith ("ET100: expected OpKeyword but got " ^ (A.show_view x) ^ " this method should not be invoked when rightmost reduction is not possible")


(* reduces all operators A + B on the stack of higher precedence*)
let rec operator_precedence_reduce (limit : int) : unit proc_state_m = 
  let* acc_top = peek_input_acc 1 in
  match acc_top with
  | None -> return ()
  | Some (x) -> 
    match A.view x with
    | A.N(N.ParsingElem(N.OpKeyword(meta)),[]) -> 
      if meta.right_precedence > limit then
        let* bin_op = lookup_binary_op meta in
        let* _ = bin_op.reduction in
        operator_precedence_reduce limit
      else
        return () (* cannot reduce, assume successful *)
    | _ -> 
      return () (* cannot reduce, assume successful *)

let run_processor (proc : processor)  : unit proc_state_m = 
  match proc with
  | ProcComplex process -> process
  | ProcBinOp { meta=({id=_;keyword;left_precedence=lp;right_precedence=_;fixity} as meta);reduction=_} ->
      let* (_read_keyword, ext) = read_string keyword in
      let operator_elem = A.annotate_with_extent (A.fold(A.N(N.ParsingElem(N.OpKeyword (meta)), []))) ext in
      (* reduce existing stack*)
      let* _ = (match fixity with
      | Infix | Postfix -> operator_precedence_reduce lp
      | _ -> return ()) in
      (* shift operators onto the stack *)
      let* _ = (match fixity with
        | Prefix | ClosedIdentifier | StartBinding _ -> push_elem_start operator_elem
        | Infix | Postfix -> push_elem_continue operator_elem
        ) in
      (* reduce right most thing for postfix and closed identifier*)
      let* _ = (match fixity with
        | Postfix | ClosedIdentifier -> operator_right_most_reduce ()
        | _ -> return ()) in
      (* also lookahead and parse a binding for start binding*)
      let* _ = (match fixity with
        | StartBinding end_str -> 
            let* ((middle_id, id_ext), _end) = scan_past_one_of_string [end_str] in
            (
              match middle_id with
              | [] -> pfail ("PC335: got empty string for binding")
              | _ -> 
                  let* _ = push_elem_on_input_acc (PE.get_bound_scanned_string_t (middle_id, id_ext)) in
                  return ()
            )
        | _ -> return ()) in
      (* reduce the operator*)
      return ()
  | ProcIdentifier id -> 
      let* string_read = read_string id in
      push_elem_on_input_acc (PE.get_identifier_t string_read)

    
let add_processor_entry_list (proc : processor_entry list) : unit proc_state_m = 
    let* proc_state = get_proc_state () in
    let new_s = {proc_state with registry = proc @ proc_state.registry} in
    write_proc_state new_s
let remove_all_proc_registry_with_input_expect_state (expect : expect) : unit proc_state_m = 
    let* proc_state = get_proc_state () in
    let new_s = {proc_state with registry = List.filter (fun x -> x.expect <> expect) proc_state.registry} in
    write_proc_state new_s

let run_processor_entry (proc : processor_entry)  : unit proc_state_m = 
    let {expect; name=_; processor} = proc in
    let* proc_state = get_proc_state () in
    if expect <> proc_state.input_expect 
    then returnNone ()
    else
      run_processor processor
