
open EngineData

module Ext = AbtLib.Extent
module CS = CharStream
module PE = ProcessedElement

let print_failwith (s : string) : 'a = 
  print_endline s;
  failwith s

let return (x : 'a) : 'a proc_state_m = 
    fun s fc sc -> sc (x, s) fc

let combine_failures ((cur_msg, cur_st) : (proc_error * proc_state) ) (prev : (proc_error list * proc_state) list) : (proc_error list * proc_state) list = 
  (* the idea is that if the new s's inputs are consumed more, then replace rest with new s, 
    if s' inputs are consumed at the same level then append*)
  let cur_idx = cur_st.input_future.idx in
  let prev_idx = List.fold_left (fun acc (_, s) -> max acc s.input_future.idx) 0 prev in
  if cur_idx = prev_idx then
      let to_add, rest = List.partition (fun (_, s) -> 
        s.input_expect = cur_st.input_expect
        && s.input_acc = cur_st.input_acc
        (* && s.input_future.idx = cur_st.input_future.idx *)
        ) prev in
      match to_add with
      | [] -> ([cur_msg], cur_st) :: prev
      | [(xmsg, _)]  -> 
          if List.mem cur_msg xmsg then
            prev
          else
            (cur_msg::xmsg, cur_st)::rest
      | _ -> failwith "PC27: multiple failures with same input state"
  else if cur_idx > prev_idx then
    (
      (* match cur_msg with
      | ErrExpectString _ -> ([cur_msg], cur_st) :: prev (* more input is consumed to produce this error, so should not drop previous ones*)
      | _ ->  *)
        [([cur_msg], cur_st)]
    )
  else
    prev


let pfail_error (msg : proc_error) : 'a proc_state_m = 
    fun s fc _sc -> fc (msg, {s with failures = combine_failures (msg, s) s.failures})

let pfail (msg : string) : 'a proc_state_m = 
  pfail_error (ErrOther msg)

let pfail_with_ext (msg : string) (ext : Ext.t) : 'a proc_state_m = 
  pfail_error (ErrWithExt (msg, ext))

(* pfail_error (ErrOther msg) *)

(* should not reach here*)
let p_internal_error (msg : string) : unit proc_state_m = 
  fun s _fc _sc -> 
    print_failwith ("PC47: " ^ msg ^ "\n" ^ show_proc_state s)

(* let returnNone () : 'a proc_state_m = 
  pfail "returnNone: no error message provided" *)

(* let returnNone () : 'a proc_state_m = 
  pfail "returnNone: no error message provided" *)

let ignore () : unit proc_state_m = 
  return ()

(* pnot m fails if m succeeds, succeeds without consuming inputs when m fails *)
  (* it is a convention that all things do not consume inputs *)
let pnot (m : 'a proc_state_m) : unit proc_state_m = 
    fun s fc sc -> 
      m s (fun _ -> sc ((), s) fc) (fun _ _ -> fc (ErrOther "pnot_fail", s))

let bind  (m : 'a proc_state_m) (f: 'a -> 'b proc_state_m) : 'b proc_state_m = 
    fun s fc sc -> 
      m s fc (fun (x, s') fc' -> f x s' fc' sc)

let (>>=) = bind
let (let*) m f = bind m f


let _then (m : 'a proc_state_m) (f: 'b proc_state_m) : 'b proc_state_m = 
  bind m (fun _ -> f)

let (>>) = _then

(* does not pass along tried failures *)
let ptry (m : 'a proc_state_m) : 'a option proc_state_m = 
  fun s fc sc  -> 
    m s (fun _ -> sc (None, s) fc) (fun (x, s') fc' -> sc (Some x, s') fc')
    (* match m s with
    | None -> Some(None, s)
    | Some (x, s') -> Some (Some x, s') *)

(* pcut P will not backtrack into P *)
let pcut (m : 'a proc_state_m) : 'a proc_state_m = 
  fun s fc sc  -> 
    m s fc (fun (x, s') _fc -> sc (x, s') fc)


let assertb (b : bool) : unit proc_state_m = 
  if b then return () else pfail "assertb: assertion failed"

(* failures in m1 gets passed to m2 *)
let choice (m1 : 'a proc_state_m) (m2 : 'a proc_state_m) : 'a proc_state_m = 
  fun s fc sc  -> 
    m1 s (fun (_, s') -> m2 {s with failures = s'.failures} fc sc) (fun (x, s') fc' -> sc (x, s') fc')

let choice_l (ms : 'a proc_state_m list) : 'a proc_state_m = 
  match ms with
  | (x ::xs) -> List.fold_left choice x xs
  | [] -> pfail "PC74: choice_l: empty list"

(* choice_cut m1 m2 will run m1 with backtracking, and if m1 succeeds 
  at least once, backtracking on m1 will not reach m2
  if m1 completely fails, then m2 will run *)
let choice_cut (m1 : 'a proc_state_m) (m2 : 'a proc_state_m) : 'a proc_state_m = 
  fun s fc sc  -> 
    let success_found = ref false in
    m1 s (fun (msg, s') -> 
      (* when m1 eventually fails, read the success found flag to determine 
      if it has succeeded*)
        if !success_found 
          (* if succeeded, do not backtrack into m2*)
          then (fc (msg, s'))
          (* if not succeeded, backtrack into m2*)
          else (m2 {s with failures = s'.failures} fc sc)
      ) 
    (fun (x, s') fc' -> 
      (* when m1 succeeds, we set a flag*)
      success_found := true;
      sc (x, s') fc')

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
  fun s fc sc -> 
    sc (s, s) fc

let update_proc_state (f : proc_state -> proc_state) : unit proc_state_m = 
  fun s fc sc -> 
    sc ((), f s) fc

let write_proc_state (s : proc_state) : unit proc_state_m = 
  fun _s fc sc -> 
    sc ((), s) fc

(* reading inputs *)

let read_any_char () : (CS.t_char * Ext.t) proc_state_m = 
  let* s = get_proc_state () in
  match CharStream.get_next_char s.input_future with
  | None -> pfail "PC95: Cannot get char"
  | Some (c, next_cs) -> 
      let new_s = {s with input_future = next_cs} in
      let cs_t_char = CharStream.new_t_char (Ext.get_str_content c) in
      let* () = write_proc_state new_s in
      return (cs_t_char, Ext.get_str_extent c)


let push_scanned_char (c : CS.t_char * Ext.t) : unit proc_state_m = 
  let* s = get_proc_state () in
  let new_s = {s with input_acc = PE.get_scanned_char_t c :: s.input_acc} in
  let* () = write_proc_state new_s in
  return ()

type t_char = CharStream.t_char
let read_one_of_char (l : t_char list) : (CS.t_char * Ext.t) proc_state_m = 
  let* (c', ext) = read_any_char () in 
  if List.mem c' l then 
    return (c', ext)
  else 
    (* pfail_error ("PC100: expected one of " ^ (String.concat "," (List.map CharStream.get_t_char l)) ^ " but got " ^ (CharStream.get_t_char c')) *)
    pfail_error (ErrExpectString {
      expecting = l;
      actual = (c', ext);
    })

(* string is a list of *)
let read_string (l : t_char list) : (CS.t_string * Ext.t) proc_state_m = 
  if l = [] then failwith "PC157: empty string" else
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
    pfail ("PC155: expected any char except " ^ (String.concat "," (List.map CharStream.get_t_char except)) ^ " but got " ^ (CharStream.get_t_char c))
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
      if List.is_empty acc 
      then
        return (([],ext), (c, ext))
      else
        return ((remap_t_char_list_with_ext acc), (c, ext))
    else 
      aux (acc@[(c, ext)])
  in
  aux []

let scan_until_one_of_string (t : CS.t_string list) : ((CS.t_string * Ext.t) (* intermediate *) ) proc_state_m = 
  let rec aux acc = 
    let* cur_st = get_proc_state () in
    let* result = ptry (read_one_of_string t) in
    match result with
    | Some(_) -> 
      (* restore before read*)
        let* () = write_proc_state cur_st in
        return ((remap_t_char_list_with_ext acc))
    | None -> 
      let* c_result = ptry (read_any_char ()) in
      match c_result with
      | Some (c, ext) -> aux (acc@[(c, ext)])
      | None -> pfail "PC211: scan_past_one_of_string: EOF encountered before string found"
  in
  (* we pcut here because we either succeed ( there is a scan) or we fail in which case there is no scan
  we don't want to have multiple successful scans here
   *)
  pcut (aux [])

let scan_past_one_of_string (t : CS.t_string list) : ((CS.t_string * Ext.t) (* intermediate *) * (CS.t_string * Ext.t) (* one of string in l*)) proc_state_m = 
  let* (intermediate) = scan_until_one_of_string t in
  let* final = ptry (read_one_of_string t) in
  match final with
  | Some((c, ext)) -> return (intermediate, (c, ext))
  | None ->  failwith "PC232: Should not be None"

let push_elem_on_input_acc (elem : PE.t) : unit proc_state_m = 
  let* s = get_proc_state () in
  let new_s = {s with input_acc = elem :: s.input_acc} in
   write_proc_state new_s


let get_expect_state () : expect proc_state_m =
  let* s = get_proc_state () in
  return s.input_expect

let modify_s (f : proc_state -> proc_state) : unit proc_state_m = 
  let* s = get_proc_state () in
  write_proc_state (f s)

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
  
let get_input_acc_size () : int proc_state_m =
  let* s = get_proc_state () in
  return (List.length s.input_acc)

let pop_input_acc () : PE.t proc_state_m =
  let* s = get_proc_state () in
  match s.input_acc with
  | [] -> pfail ("PC224: Attempting to pop from empty input accumulator: ")
  | x :: xs -> 
      let new_s = {s with input_acc = xs} in
      let* _ = write_proc_state new_s in
      return x

let pop_input_acc_2 () : (PE.t * PE.t) proc_state_m =
  let* x = pop_input_acc () in
  let* y = pop_input_acc () in
  return (y, x)

let pop_input_acc_3 () : (PE.t * PE.t * PE.t) proc_state_m =
  let* x = pop_input_acc () in
  let* y = pop_input_acc () in
  let* z = pop_input_acc () in
  return (z, y, x)

let assert_is_correct_operand (meta : binary_op_meta) (elem : PE.t) : unit proc_state_m =
  match A.view elem with
  | A.N(N.ParsingElem(N.OpKeyword(kop)), []) -> 
    if meta.id = kop.id then
      return ()
    else
      pfail ("check_is_operand: expected " ^ (show_binary_op_meta meta) ^ " but got " ^ (show_binary_op_meta kop))
  | _ -> 
    print_failwith ("PC247: check_is_operand: expected " ^ show_binary_op_meta meta ^ " but got " ^ (A.show_view elem))

let assert_is_not_op_keyword (elem : A.t) : unit proc_state_m =
  match A.view elem with
  | A.N(N.ParsingElem(N.OpKeyword(_)), []) -> 
    pfail_with_ext ("PC247: check_is_not_op_keyword: expected not OpKeyword but got " ^ (A.show_view elem)) (A.get_extent_some elem)
  | _ -> 
    return ()
  
let lookup_binary_op (meta_id : int) : binary_op proc_state_m = 
  let* s = get_proc_state () in
  match List.filter_map (fun x -> 
    match x.processor with ProcBinOp(x) -> if x.meta.id = meta_id then Some(x) else None | _ -> None) s.registry with
  | [x] -> return x
  | [] -> pfail ("PC343: cannot find binary operator " ^ (string_of_int meta_id) ^ " in the registry " ^ (
    String.concat "," (List.filter_map (fun x -> match x.processor with ProcBinOp x -> Some(show_binary_op_meta x.meta) | _ -> None) s.registry)
  ))
  | _ -> print_failwith ("ET346: multiple binary operators " ^ (string_of_int meta_id) ^ " in the registry " ^ (
    String.concat "," (List.filter_map (fun x -> match x.processor with ProcBinOp x -> Some(show_binary_op_meta x.meta) | _ -> None) s.registry)
  ))

(* extent is the entirety of the expressions *)
let pop_op_operands_from_top (binop : binary_op_meta) : ((PE.t list) * Ext.t) proc_state_m = 
  let rec f binop = 
    let* (top_op) = pop_input_acc() in
    let* _ = assert_is_correct_operand binop top_op in
    let top_extent = A.get_extent_some top_op in
    match A.view top_op with
    | A.N(N.ParsingElem(N.OpKeyword(meta)), []) -> 
      (
        match meta.left_fixity with
        | FxNone -> return ([], top_extent)
        | FxOp _ -> (
          let* next_op = pop_input_acc() in
          let* () = assert_is_not_op_keyword next_op in
          return ([next_op], Ext.combine_extent (A.get_extent_some next_op) top_extent)
        )
        | FxBinding prev_op_uid | FxComp prev_op_uid -> (
          let* comp = pop_input_acc() in
          let* () = assert_is_not_op_keyword comp in
          let* prev_op_meta = lookup_binary_op prev_op_uid in
          let* (rest_ops, rest_ext) = f prev_op_meta.meta in
          return ((rest_ops@[comp]), Ext.combine_extent rest_ext top_extent)
        )
      )
    | _ -> pfail ("PC248: expected " ^ show_binary_op_meta binop ^ " but got " ^ (A.show_view top_op) ^ " ")
   in
  f binop

let pop_op_operands_from_second_top (binop : binary_op_meta) : ((PE.t list) * Ext.t) proc_state_m = 
  let* top_operand = pop_input_acc() in
  let* _ = assert_is_not_op_keyword top_operand in
  let* (all_oprands, ext) = pop_op_operands_from_top binop in
  return (all_oprands@[top_operand], Ext.combine_extent (ext) (A.get_extent_some top_operand))

let pop_op_operands_from_top_0 (binop : binary_op_meta) : (Ext.t) proc_state_m = 
  let* (all_oprands, ext) = pop_op_operands_from_top binop in
  match all_oprands with
  | [] -> return ext
  | _ -> failwith ("PC249: expected 0 operand but got more than 0 operands")

let pop_op_operands_from_top_1 (binop : binary_op_meta) : (PE.t * Ext.t) proc_state_m = 
  let* (all_oprands, ext) = pop_op_operands_from_top binop in
  match all_oprands with
  | [x] -> return (x, ext)
  | _ -> failwith ("PC249: expected 1 operand but got more than 1 operands")

let pop_op_operands_from_top_2 (binop : binary_op_meta) : ((PE.t * PE.t) * Ext.t) proc_state_m = 
  let* (all_oprands, ext) = pop_op_operands_from_top binop in
  match all_oprands with
  | [x;y] -> return ((x, y), ext)
  | _ -> failwith ("PC249: expected 2 operands but got more than 2 operands")

let pop_op_operands_from_top_3 (binop : binary_op_meta) : ((PE.t * PE.t * PE.t) * Ext.t) proc_state_m = 
  let* (all_oprands, ext) = pop_op_operands_from_top binop in
  match all_oprands with
  | [x;y;z] -> return ((x, y, z), ext)
  | _ -> failwith ("PC249: expected 3 operands but got more than 3 operands")

let pop_op_operands_from_top_4 (binop : binary_op_meta) : ((PE.t * PE.t * PE.t * PE.t) * Ext.t) proc_state_m = 
  let* (all_oprands, ext) = pop_op_operands_from_top binop in
  match all_oprands with
  | [x;y;z;w] -> return ((x, y, z, w), ext)
  | _ -> failwith ("PC249: expected 4 operands but got more than 4 operands")

let pop_op_operands_from_top_5 (binop : binary_op_meta) : ((PE.t * PE.t * PE.t * PE.t * PE.t) * Ext.t) proc_state_m = 
  let* (all_oprands, ext) = pop_op_operands_from_top binop in
  match all_oprands with
  | [x;y;z;w;v] -> return ((x, y, z, w, v), ext)
  | _ -> failwith ("PC249: expected 5 operands but got more than 5 operands")

let pop_op_operands_from_second_top_1 (binop : binary_op_meta) : (PE.t * Ext.t) proc_state_m = 
  let* (all_oprands, ext) = pop_op_operands_from_second_top binop in
  match all_oprands with
  | [x] -> return (x, ext)
  | _ -> failwith ("PC249: expected 1 operand but got more than 1 operands")

let pop_op_operands_from_second_top_2 (binop : binary_op_meta) : ((PE.t * PE.t) * Ext.t) proc_state_m =
  let* (all_oprands, ext) = pop_op_operands_from_second_top binop in
  match all_oprands with
  | [x;y] -> return ((x, y), ext)
  | _ -> failwith ("PC249: expected 2 operands but got more than 2 operands")

let pop_op_operands_from_second_top_3 (binop : binary_op_meta) : ((PE.t * PE.t * PE.t) * Ext.t) proc_state_m =
  let* (all_oprands, ext) = pop_op_operands_from_second_top binop in
  match all_oprands with
  | [x;y;z] -> return ((x, y, z), ext)
  | _ -> failwith ("PC249: expected 3 operands but got more than 3 operands")

let pop_op_operands_from_second_top_4 (binop : binary_op_meta) : ((PE.t * PE.t * PE.t * PE.t) * Ext.t) proc_state_m =
  let* (all_oprands, ext) = pop_op_operands_from_second_top binop in
  match all_oprands with
  | [x;y;z;w] -> return ((x, y, z, w), ext)
  | _ -> failwith ("PC249: expected 4 operands but got more than 4 operands")

let pop_op_operands_from_second_top_5 (binop : binary_op_meta) : ((PE.t * PE.t * PE.t * PE.t * PE.t) * Ext.t) proc_state_m =
  let* (all_oprands, ext) = pop_op_operands_from_second_top binop in
  match all_oprands with
  | [x;y;z;w;v] -> return ((x, y, z, w, v), ext)
  | _ -> failwith ("PC249: expected 5 operands but got more than 5 operands")


(* extent is the entirety of expressions *)


let pop_bin_operand (binop : binary_op_meta) : ((PE.t * PE.t) * Ext.t) proc_state_m =
  pop_op_operands_from_second_top_2 binop

(* extent is the entirety of expressions *)
let pop_prefix_operand (binop : binary_op_meta) : (PE.t * Ext.t) proc_state_m =
  pop_op_operands_from_second_top_1 binop

let pop_postfix_operand (binop : binary_op_meta) : (PE.t * Ext.t) proc_state_m =
  pop_op_operands_from_top_1 binop

let pop_closed_identifier_operand (binop : binary_op_meta) : Ext.t proc_state_m =
  pop_op_operands_from_top_0 binop


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
      (
        match meta.right_fixity with
        | FxNone ->
          failwith ("PC322: expected prefix or infix but got " ^ (show_binary_op_meta meta) ^ " others should be reduced directly not remain on the stack")
        | _ ->
          push_elem_on_input_acc es
      )
    | A.N(N.ModuleDef, _) 
    ->
      push_elem_on_input_acc es
    | _ -> 
      pfail ("PC324: expected An element that allows ids to be pushed but got " ^ (A.show_view x) ^ " cannot push the next identifier, which is "
        ^ (A.show_view es) ^ " on the stack")

let push_elem_continue (es : A.t) : unit proc_state_m = 
  let* acc_top = peek_input_acc 0 in
  match acc_top with
  | None -> pfail "PC243: empty stack"
  | Some (x) -> 
    match A.view x with
    | A.N(N.ParsingElem(N.OpKeyword(_)),[]) -> 
      pfail ("PC333: expected things  got " ^ (A.show_view x) ^ " cannot push the next identifier " ^ A.show_view es)
    | _ -> 
      push_elem_on_input_acc es





(* this reduces postfix and closed operators already on the stack*)
let operator_right_most_reduce () : unit proc_state_m = 
  let* acc_top = peek_input_acc 0 in
  match acc_top with
  | None -> failwith "PC244: empty stack"
  | Some (x) -> 
    match A.view x with
    | A.N(N.ParsingElem(N.OpKeyword(meta)),[]) -> 
      (
        match meta.right_fixity with
        | FxNone -> 
            let* bin_op = lookup_binary_op meta.id in
            bin_op.reduction
        | _ ->
            failwith ("PC362: expected postfix or closed identifier but got " ^ (show_binary_op_meta meta) ^ " others should be reduced directly not remain on the stack")
      )
    | _ -> 
      failwith ("PC364: expected OpKeyword but got " ^ (A.show_view x) ^ " this method should not be invoked when rightmost reduction is not possible")


(* reduces all operators A + B on the stack of higher precedence*)
let rec operator_precedence_reduce (limit : int) : unit proc_state_m = 
  let* acc_top = peek_input_acc 1 in
  match acc_top with
  | None -> return ()
  | Some (x) -> 
    match A.view x with
    | A.N(N.ParsingElem(N.OpKeyword(meta)),[]) -> 
      (match meta.right_fixity with
        | FxOp rp ->
            if rp > limit then
              let* bin_op = lookup_binary_op meta.id in
              let* _ = bin_op.reduction in
              operator_precedence_reduce limit
            else
              return () (* cannot reduce, assume successful *)
        | _ -> 
          return () (* cannot reduce, assume successful *)
      )
    | _ -> 
      return () (* cannot reduce, assume successful *)


(* reduce until the stack 's second element is comp *)
let rec operator_component_reduce (comp_uid : int) : unit proc_state_m = 
  (* let* stack_size = get_input_acc_size () in
  if stack_size < 2 then 
    pfail ("PC399: Expecting at least 2 elements on the stack but got " ^ string_of_int stack_size)
  else *)
    let* acc_top = peek_input_acc 1 in
    match acc_top with
    | None -> pfail ("PC400: Expecting at least 2 elements on the stack but got " )
    | Some (x) -> 
      match A.view x with
      | A.N(N.ParsingElem(N.OpKeyword(meta)),[]) -> 
        if meta.id = comp_uid then
          return ()
        else
          let* oper = lookup_binary_op meta.id in
          (* this component is eligible for reduction if the right_fixity is an expression-expecting construct, 
            if the right handside is a component exepctation that is different from comp_uid, 
              then it is an error, no parse*)
          (* print_endline ("Operator component reduction called on " ^ (show_binary_op_meta oper.meta) ^ " comp_uid= " ^ (string_of_int comp_uid) ^ " " ); *)
          (
            match oper.meta.right_fixity with
            | FxOp _ -> 
                let* _ = oper.reduction in
                operator_component_reduce comp_uid
            | FxBinding _ | FxComp _ ->
                let* comp_oper = lookup_binary_op comp_uid in
                  pfail ("PC401: Expecting " ^ (show_binary_op_meta comp_oper.meta) ^ " but got " ^ (show_binary_op_meta oper.meta) ^ " in the operator component reduction")
            | FxNone -> failwith ("PC559: FxNone should not be here " ^ (show_binary_op_meta oper.meta) ^ " " )
          )
      | _ -> 
        pfail ("PC402: expecting " ^ (string_of_int comp_uid) ^ " but got " ^ (A.show_view x) ^ " ")




let yy_keyword_chars = CharStream.new_t_string "。（）「」『』"
let process_read_operator (meta : binary_op_meta) (read_ext : Ext.t) : unit proc_state_m = 
  let {id=_;keyword;left_fixity;right_fixity} = meta in
  let* () = if !Flags.show_parse_tracing then (
    print_endline ("=========\n[OP] Read " ^ CS.get_t_string keyword );
    let* st = get_proc_state () in
    print_endline ("[OP] Current state " ^ show_proc_state st);
    return ()
  ) else return () in
  let operator_elem = A.annotate_with_extent (A.fold(A.N(N.ParsingElem(N.OpKeyword (meta)), []))) read_ext in
  (* reduce existing stack based on the left_fixity of the operator element*)
  let* _ = (match left_fixity with
  | FxOp lp -> operator_precedence_reduce lp
  | FxNone -> return ()
  | FxBinding c | FxComp c -> operator_component_reduce c
  ) in
  (* shift operators onto the stack *)
  let* _ = (match left_fixity with
    | FxNone -> push_elem_start operator_elem
    | FxOp _ | FxBinding _ | FxComp _ -> push_elem_continue operator_elem
    ) in
  (* reduce right most thing for postfix and closed identifier*)
  let* _ = (match right_fixity with
    | FxNone -> operator_right_most_reduce ()
    | _ -> return ()) in
  (* also lookahead and parse a binding for start binding*)
  let* _ = (match right_fixity with
    | FxBinding end_str_op_uid -> 
        let* oper = lookup_binary_op end_str_op_uid in
        let* ((middle_id, id_ext)) = scan_until_one_of_string [oper.meta.keyword] in
        (
          match middle_id with
          | [] -> pfail ("PC335: got empty string for binding")
          | _ -> 
              if List.exists (fun x -> List.mem x yy_keyword_chars) middle_id then
                pfail_with_ext ("PC336: got binding " ^ ( CS.get_t_string middle_id) ^ " which contains keywords " ) (id_ext)
              else
                let* _ = push_elem_on_input_acc (PE.get_bound_scanned_string_t (middle_id, id_ext)) in
                return ()
        )
    | _ -> return ()) in
  (* reduce the operator*)
  let* () = if !Flags.show_parse_tracing then (
    print_endline ("=========\n[OP] Successful " ^ CS.get_t_string keyword );
    let* st = get_proc_state () in
    print_endline ("[OP] Current state " ^ show_proc_state st);
        return ()
      ) else return () in
      return ()

let run_processor (proc : processor)  : unit proc_state_m = 
  match proc with
  | ProcComplex process -> process
  | ProcBinOp { meta;reduction=_} ->
      let* (_read_keyword, ext) = read_string meta.keyword in
      (* experiment once an semantic operator parses, no backtrack*)
      if meta.keyword = CS.new_t_string "之书"
        then pcut (process_read_operator meta ext)
        else process_read_operator meta ext
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
    then pfail ("PC100: expected " ^ (show_input_expect expect) ^ " but got " ^ (show_input_expect proc_state.input_expect))
    else
      run_processor processor

let collect_input_acc_identifiers() : CS.t_string list proc_state_m = 
  let* s = get_proc_state () in
  let all_scanned_ids = ListUtil.remove_duplicates (List.concat_map (fun x -> 
    (* aux potentially recursive *)
    let aux y = match A.view y with 
    | A.N(N.ParsingElem(N.BoundScannedString s), []) -> [s]
    | _ ->  List.map (CS.new_t_string) (A.get_free_vars y) in
    aux x
    ) s.input_acc) in
  let all_existing_ids = List.filter_map (fun x -> 
    match x.processor with
    | ProcIdentifier id -> Some id
    | _ -> None) s.registry in
  return (ListUtil.minus all_scanned_ids all_existing_ids)

let run_input_acc_identifiers () : unit proc_state_m = 
  let* all_scanned_ids = collect_input_acc_identifiers () in
  choice_l (List.map (fun x -> 
    run_processor_entry (to_processor_identifier Expression "bid" x)
    ) all_scanned_ids)