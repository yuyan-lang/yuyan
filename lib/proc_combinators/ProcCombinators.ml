open EngineData
open EngineDataPrint
module Ext = AbtLib.Extent
module CS = CharStream

let print_failwith (s : string) : 'a =
  print_endline s;
  failwith s
;;

let return (x : 'a) : 'a proc_state_m = fun s fc sc -> sc (x, s) fc
let get_proc_state () : proc_state proc_state_m = fun s fc sc -> sc (s, s) fc
let update_proc_state (f : proc_state -> proc_state) : unit proc_state_m = fun s fc sc -> sc ((), f s) fc
let write_proc_state (s : proc_state) : unit proc_state_m = fun _s fc sc -> sc ((), s) fc

let bind (m : 'a proc_state_m) (f : 'a -> 'b proc_state_m) : 'b proc_state_m =
  fun s fc sc -> m s fc (fun (x, s') fc' -> f x s' fc' sc)
;;

let ( >>= ) = bind
let ( let* ) m f = bind m f
let _then (m : 'a proc_state_m) (f : 'b proc_state_m) : 'b proc_state_m = bind m (fun _ -> f)
let ( >> ) = _then

let combine_failures (cur_error : proc_error) (prev : proc_error list) : proc_error list =
  (* the idea is that if the new s's inputs are consumed more, then replace rest with new s, 
    if s' inputs are consumed at the same level then append*)
  let cur_idx = cur_error.state.input_future.idx in
  let prev_idx = List.fold_left (fun acc e -> max acc e.state.input_future.idx) 0 prev in
  if cur_idx = prev_idx then cur_error :: prev else if cur_idx > prev_idx then [ cur_error ] else prev
;;

let push_failure_to_s (cur_error : proc_error) (s : proc_state) : proc_state =
  let new_failures = combine_failures cur_error s.failures in
  { s with failures = new_failures }
;;

let pfail_error (cur_error : proc_error) : 'a proc_state_m =
  fun s fc _sc ->
  if !Flags.show_parse_progress
  then print_endline ("Failed because: " ^ cur_error.msg ^ " " ^ Ext.show_extent cur_error.ext);
  fc (push_failure_to_s cur_error s)
;;

let pfail_with_ext (msg : string) (ext : Ext.t) : 'a proc_state_m =
  let* s = get_proc_state () in
  let cur_error = { msg; ext; state = s } in
  pfail_error cur_error
;;

let pfail_inapplicable_parser_rule (_rule_name : string) (_ext : Ext.t) : 'a proc_state_m = fun s fc _sc -> fc s

(* pfail_error (ErrOther msg) *)

(* should not reach here*)
let p_internal_error (msg : string) : unit proc_state_m =
  fun s _fc _sc -> print_failwith ("PC47: " ^ msg ^ "\n" ^ show_proc_state s)
;;

(* let returnNone () : 'a proc_state_m = 
  Fail.failwith "returnNone: no error message provided" *)

(* let returnNone () : 'a proc_state_m = 
  Fail.failwith "returnNone: no error message provided" *)

let ignore () : unit proc_state_m = return ()
let do_nothing : unit proc_state_m = return ()
let do_nothing_shift_action : Ext.t -> unit proc_state_m = fun _ -> return ()

(* pnot m fails if m succeeds, succeeds without consuming inputs when m fails *)
(* it is a convention that all things do not consume inputs *)
let pnot (m : 'a proc_state_m) : unit proc_state_m = fun s fc sc -> m s (fun _ -> sc ((), s) fc) (fun _ _ -> fc s)

let psequence (m : 'a proc_state_m list) : 'a list proc_state_m =
  List.fold_left
    (fun acc m ->
       let* x = acc in
       let* y = m in
       return (x @ [ y ]))
    (return [])
    m
;;

let pfold_left (f : 'a -> 'b -> 'a proc_state_m) (acc : 'a) (m : 'b list) : 'a proc_state_m =
  List.fold_left
    (fun acc y ->
       let* x = acc in
       f x y)
    (return acc)
    m
;;

(* does not pass along tried failures *)
let ptry (m : 'a proc_state_m) : 'a option proc_state_m =
  fun s fc sc -> m s (fun _ -> sc (None, s) fc) (fun (x, s') fc' -> sc (Some x, s') fc')
;;

(* match m s with
    | None -> Some(None, s)
    | Some (x, s') -> Some (Some x, s') *)

(* pcut P will not backtrack into P *)
let pcut (m : 'a proc_state_m) : 'a proc_state_m = fun s fc sc -> m s fc (fun (x, s') _fc -> sc (x, s') fc)

(* cuts off all current backtracking points unless explicitly remembered *)
let pcommit () : unit proc_state_m =
  fun s _fc sc ->
  (* print_endline "COMMITTING"; *)
  (* commiting to the current choice, any backtracking of subsequnt call will now backtrack into 
    last remembered failure handlers*)
  sc ((), s) s.top_failure_handler
;;

let assertb (b : bool) : unit proc_state_m = if b then return () else Fail.failwith "assertb: assertion failed"

(* failures in m1 gets passed to m2 *)
let choice (m1 : 'a proc_state_m) (m2 : 'a proc_state_m) : 'a proc_state_m =
  fun s fc sc -> m1 s (fun s' -> m2 { s with failures = s'.failures } fc sc) (fun (x, s') fc' -> sc (x, s') fc')
;;

let choice_l (ms : 'a proc_state_m list) : 'a proc_state_m =
  match ms with
  | x :: xs -> List.fold_left choice x xs
  | [] -> Fail.failwith "PC74: choice_l: empty list"
;;

(* choice_cut m1 m2 will run m1 with backtracking, and if m1 succeeds 
  at least once, backtracking on m1 will not reach m2
  if m1 completely fails, then m2 will run *)
let choice_cut (m1 : 'a proc_state_m) (m2 : 'a proc_state_m) : 'a proc_state_m =
  fun s fc sc ->
  let success_found = ref false in
  m1
    s
    (fun s' ->
       (* when m1 eventually fails, read the success found flag to determine 
      if it has succeeded*)
       if !success_found (* if succeeded, do not backtrack into m2*)
       then fc s'
       (* if not succeeded, backtrack into m2*)
       else m2 { s with failures = s'.failures } fc sc)
    (fun (x, s') fc' ->
       (* when m1 succeeds, we set a flag*)
       success_found := true;
       sc (x, s') fc')
;;

let get_input_acc_position_as_ext () : Ext.t proc_state_m =
  let* s = get_proc_state () in
  return (s.input_future.filename, (s.input_future.line, s.input_future.col), (s.input_future.line, s.input_future.col))
;;

(* get a non-backtracking list parser *)
let many1 (m : 'a proc_state_m) : 'a list proc_state_m =
  let rec aux acc =
    let* x = ptry m in
    match x with
    | None ->
      if List.is_empty acc
      then
        let* ext = get_input_acc_position_as_ext () in
        pfail_with_ext (__LOC__ ^ " PC144: many1: no match") ext
      else return acc
    | Some x -> aux (acc @ [ x ])
  in
  pcut (aux [])
;;

let to_processor_binary_op (name : string) (binop : binary_op) : processor_entry =
  { id = Uid.next (); name; processor = ProcBinOp binop }
;;

let to_processor_complex (name : string) (process : 'a proc_state_m) : processor_entry =
  { id = Uid.next (); name; processor = ProcComplex process }
;;

let get_scanned_char_t ((c, ext) : CS.t_char * Ext.t) : input_acc_elem = ParsingElem (ScannedChar c, ext)

let get_bound_scanned_string_t ((c, ext) : CS.t_string * Ext.t) : input_acc_elem =
  ParsingElem (BoundScannedString c, ext)
;;

let get_identifier_t ((c, ext) : CS.t_string * Ext.t) : input_acc_elem =
  Expr (A.free_var (Ext.str_with_extent (CS.get_t_string c) ext))
;;

let get_keyword_t ((keyword, ext) : CS.t_string * Ext.t) : input_acc_elem = ParsingElem (Keyword keyword, ext)

let is_keyword (x : input_acc_elem) (string : string) : bool =
  match x with
  | ParsingElem (Keyword s, _) -> CS.get_t_string s = string
  | _ -> false
;;

let with_type_checking_history (msg : tc_history_elem) (cont : 'a proc_state_m) : 'a proc_state_m =
  let* s = get_proc_state () in
  if !Flags.show_type_checking_progress then print_endline ("Type checking history: " ^ show_tc_history_elem s msg);
  let new_s = { s with type_checking_history = msg :: s.type_checking_history } in
  let* () = write_proc_state new_s in
  let* result = cont in
  let* s = get_proc_state () in
  match s.type_checking_history with
  | _ :: tail ->
    let new_s = { s with type_checking_history = tail } in
    let* () = write_proc_state new_s in
    return result
  | _ -> Fail.failwith ("PC209: Type checking history mismatch: " ^ show_tc_history_elem s msg)
;;

(* let clear_type_checking_history () : unit proc_state_m =
  let* s = get_proc_state () in
  write_proc_state { s with type_checking_history = [] }
;; *)

(* reading inputs *)
let peek_any_char () : (CS.t_char * Ext.t) proc_state_m =
  let* s = get_proc_state () in
  match CharStream.peek_next_char s.input_future with
  | None -> Fail.failwith "PC95: Cannot get char"
  | Some c -> return c
;;

let read_any_char () : (CS.t_char * Ext.t) proc_state_m =
  let* s = get_proc_state () in
  match CharStream.get_next_char s.input_future with
  | None -> Fail.failwith "PC95: Cannot get char"
  | Some (c, next_cs) ->
    let new_s = { s with input_future = next_cs } in
    let cs_t_char = CharStream.new_t_char (Ext.get_str_content c) in
    let* () = write_proc_state new_s in
    return (cs_t_char, Ext.get_str_extent c)
;;

let push_scanned_char (c : CS.t_char * Ext.t) : unit proc_state_m =
  let* s = get_proc_state () in
  let new_s = { s with input_acc = get_scanned_char_t c :: s.input_acc } in
  let* () = write_proc_state new_s in
  return ()
;;

type t_char = CharStream.t_char

let read_one_of_char (l : t_char list) : (CS.t_char * Ext.t) proc_state_m =
  let* c', ext = read_any_char () in
  if List.mem c' l
  then return (c', ext)
  else
    (* pfail_error ("PC100: expected one of " ^ (String.concat "," (List.map CharStream.get_t_char l)) ^ " but got " ^ (CharStream.get_t_char c')) *)
    (* pfail_error (ErrExpectString { expecting = l; actual = c', ext }) *)
    pfail_inapplicable_parser_rule "read_one_of_char" ext
;;

(* string is a list of *)
let read_string (l : t_char list) : (CS.t_string * Ext.t) proc_state_m =
  if l = []
  then failwith "PC157: empty string"
  else (
    let rec aux acc rem =
      match rem with
      | [] -> return (List.map fst acc, Ext.combine_extent_list (List.map snd acc))
      | c :: cs ->
        let* read_c = read_one_of_char [ c ] in
        aux (acc @ [ read_c ]) cs
    in
    aux [] l)
;;

let read_one_of_string (l : CS.t_string list) : (CS.t_string * Ext.t) proc_state_m = choice_l (List.map read_string l)

let read_any_char_except (except : CS.t_char list) : (CS.t_char * Ext.t) proc_state_m =
  let* c, ext = read_any_char () in
  if List.mem c except
  then
    Fail.failwith
      ("PC155: expected any char except "
       ^ String.concat "," (List.map CharStream.get_t_char except)
       ^ " but got "
       ^ CharStream.get_t_char c)
  else return (c, ext)
;;

let read_any_char_except_and_push (except : CS.t_char list) : unit proc_state_m =
  read_any_char_except except >>= push_scanned_char
;;

let remap_t_char_list_with_ext (c : (CS.t_char * Ext.t) list) : CS.t_string * Ext.t =
  List.map fst c, Ext.combine_extent_list (List.map snd c)
;;

let scan_past_one_of_char (l : CS.t_char list)
  : ((CS.t_string * Ext.t) * (* intermediate *) (CS.t_char * Ext.t)) (* one of char in l*) proc_state_m
  =
  let rec aux acc =
    let* c, ext = read_any_char () in
    if List.mem c l
    then if List.is_empty acc then return (([], ext), (c, ext)) else return (remap_t_char_list_with_ext acc, (c, ext))
    else aux (acc @ [ c, ext ])
  in
  aux []
;;

let scan_until_one_of_string (t : CS.t_string list) : (CS.t_string * Ext.t) (* intermediate *) proc_state_m =
  let rec aux acc =
    let* cur_st = get_proc_state () in
    let* result = ptry (read_one_of_string t) in
    match result with
    | Some _ ->
      (* restore before read*)
      let* () = write_proc_state cur_st in
      return (remap_t_char_list_with_ext acc)
    | None ->
      let* c_result = ptry (read_any_char ()) in
      (match c_result with
       | Some (c, ext) -> aux (acc @ [ c, ext ])
       | None ->
         Fail.failwith
           ("PC211: scan_past_one_of_string: EOF encountered before one of string found: "
            ^ String.concat "," (List.map CharStream.get_t_string t)))
  in
  (* we pcut here because we either succeed ( there is a scan) or we fail in which case there is no scan
  we don't want to have multiple successful scans here
  *)
  pcut (aux [])
;;

let scan_past_one_of_string (t : CS.t_string list)
  : ((CS.t_string * Ext.t) * (* intermediate *) (CS.t_string * Ext.t)) (* one of string in l*) proc_state_m
  =
  let* intermediate = scan_until_one_of_string t in
  let* final = ptry (read_one_of_string t) in
  match final with
  | Some (c, ext) -> return (intermediate, (c, ext))
  | None -> failwith "PC232: Should not be None"
;;

let push_elem_on_input_acc (elem : input_acc_elem) : unit proc_state_m =
  let* s = get_proc_state () in
  let new_s = { s with input_acc = elem :: s.input_acc; last_input_acc_before_pop = None } in
  (* reset the last_input_acc_before_pop *)
  write_proc_state new_s
;;

let push_elem_on_input_acc_expr (expr : A.t) : unit proc_state_m = push_elem_on_input_acc (Expr expr)

(* let get_expect_state () : expect proc_state_m =
  let* s = get_proc_state () in
  return s.input_expect *)

let modify_s (f : proc_state -> proc_state) : unit proc_state_m =
  let* s = get_proc_state () in
  write_proc_state (f s)
;;

(* let push_expect_state (new_state : expect) : unit proc_state_m =
  let* cur_state = get_expect_state () in
  let* _ =
    modify_s (fun s -> { s with input_expect = new_state; expect_state_stack = cur_state :: s.expect_state_stack })
  in
  return ()
;;

let pop_expect_state (cur_state : expect) : unit proc_state_m =
  let* st = get_proc_state () in
  let* () = assertb (cur_state = st.input_expect) in
  match st.expect_state_stack with
  | [] -> Fail.failwith "pop_expect_state: empty stack"
  | x :: tail ->
    let* _ = modify_s (fun s -> { s with input_expect = x; expect_state_stack = tail }) in
    return ()
;; *)

let peek_input_acc (idx : int) : input_acc_elem option proc_state_m =
  let* s = get_proc_state () in
  if idx < List.length s.input_acc then return (Some (List.nth s.input_acc idx)) else return None
;;

let get_input_acc_elem_ext (elem : input_acc_elem) : Ext.t =
  match elem with
  | Expr exp -> A.get_extent_some exp
  | ParsingElem (_, ext) -> ext
;;

let peek_input_acc_expr () : (A.t * Ext.t) proc_state_m =
  let* s = get_proc_state () in
  match List.hd s.input_acc with
  | Expr expr -> return (expr, A.get_extent_some expr)
  | _ -> Fail.failwith "PC225: Attempting to peek a non-expr from input accumulator"
;;

let peek_input_acc_parsing_elem_bound_scanned_string () : (CS.t_string * Ext.t) proc_state_m =
  let* s = get_proc_state () in
  match List.hd s.input_acc with
  | ParsingElem (BoundScannedString str, ext) -> return (str, ext)
  | _ -> Fail.failwith "PC225: Attempting to peek a non-bound scanned string from input accumulator"
;;

let get_input_acc_size () : int proc_state_m =
  let* s = get_proc_state () in
  return (List.length s.input_acc)
;;

let pop_input_acc () : input_acc_elem proc_state_m =
  let* s = get_proc_state () in
  match s.input_acc with
  | [] -> Fail.failwith "PC224: Attempting to pop from empty input accumulator: "
  | x :: xs ->
    let new_s =
      { s with
        input_acc = xs
      ; last_input_acc_before_pop =
          (match s.last_input_acc_before_pop with
           (* only set it if not set. so that consecutive pops will not overwrite the last_input_acc_before_pop *)
           | None -> Some s.input_acc
           | Some ys -> Some ys)
      }
    in
    let* _ = write_proc_state new_s in
    return x
;;

let pop_input_acc_parsing_elem () : (parsing_elem * Ext.t) proc_state_m =
  let* elem = pop_input_acc () in
  match elem with
  | ParsingElem (x, ext) -> return (x, ext)
  | _ ->
    let ext = get_input_acc_elem_ext elem in
    pfail_with_ext
      (__LOC__ ^ " PC225: Attempting to pop non-parsing element from input accumulator: " ^ show_input_acc_elem elem)
      ext
;;

let pop_input_acc_expr () : (A.t * Ext.t) proc_state_m =
  let* elem = pop_input_acc () in
  match elem with
  | Expr x -> return (x, A.get_extent_some x)
  | _ ->
    let ext = get_input_acc_elem_ext elem in
    pfail_with_ext
      (__LOC__ ^ " PC226: Attempting to pop non-expr element from input accumulator: " ^ show_input_acc_elem elem)
      ext
;;

let pop_input_acc_2 () : (input_acc_elem * input_acc_elem) proc_state_m =
  let* x = pop_input_acc () in
  let* y = pop_input_acc () in
  return (y, x)
;;

let pop_input_acc_2_expr () : (A.t * A.t) proc_state_m =
  let* x, _ = pop_input_acc_expr () in
  let* y, _ = pop_input_acc_expr () in
  return (y, x)
;;

let pop_input_acc_3 () : (input_acc_elem * input_acc_elem * input_acc_elem) proc_state_m =
  let* x = pop_input_acc () in
  let* y = pop_input_acc () in
  let* z = pop_input_acc () in
  return (z, y, x)
;;

let assert_is_correct_op (meta : binary_op_meta) (elem : parsing_elem) : unit proc_state_m =
  match elem with
  | OpKeyword kop ->
    if meta.id = kop.id
    then return ()
    else Fail.failwith ("check_is_operand: expected " ^ show_binary_op_meta meta ^ " but got " ^ show_binary_op_meta kop)
  | _ ->
    print_failwith
      ("PC247: check_is_operand: expected " ^ show_binary_op_meta meta ^ " but got " ^ show_parsing_elem elem)
;;

let assert_is_not_op_keyword (elem : input_acc_elem) : unit proc_state_m =
  match elem with
  | ParsingElem (OpKeyword _, ext) ->
    pfail_with_ext ("PC342: check_is_not_op_keyword: expected not OpKeyword but got " ^ show_input_acc_elem elem) ext
  | _ -> return ()
;;

let lookup_binary_op (meta_id : int) : binary_op proc_state_m =
  let* s = get_proc_state () in
  match
    List.filter_map
      (fun x ->
         match x.processor with
         | ProcBinOp x -> if x.meta.id = meta_id then Some x else None
         | _ -> None)
      s.registry
  with
  | [ x ] -> return x
  | [] ->
    Fail.failwith
      ("PC343: cannot find binary operator "
       ^ string_of_int meta_id
       ^ " in the registry "
       ^ String.concat
           ",\n"
           (List.filter_map
              (fun x ->
                 match x.processor with
                 | ProcBinOp x -> Some (show_binary_op_meta x.meta)
                 | _ -> None)
              s.registry))
  | _ ->
    print_failwith
      ("ET346: multiple binary operators "
       ^ string_of_int meta_id
       ^ " in the registry "
       ^ String.concat
           ","
           (List.filter_map
              (fun x ->
                 match x.processor with
                 | ProcBinOp x -> Some (show_binary_op_meta x.meta)
                 | _ -> None)
              s.registry))
;;

(* extent is the entirety of the expressions *)
let pop_postfix_op_operands (binop : binary_op_meta) : (A.t list * Ext.t) proc_state_m =
  let rec f binop =
    let* top_op, top_extent = pop_input_acc_parsing_elem () in
    let* _ = assert_is_correct_op binop top_op in
    match top_op with
    | OpKeyword meta ->
      (match meta.left_fixity with
       | FxNone -> return ([], top_extent)
       | FxOp _ ->
         let* next_op, next_op_ext = pop_input_acc_expr () in
         return ([ next_op ], Ext.combine_extent next_op_ext top_extent)
       | FxComp prev_op_uid ->
         let* comp, _comp_ext = pop_input_acc_expr () in
         let* prev_op_meta = lookup_binary_op prev_op_uid in
         let* rest_ops, rest_ext = f prev_op_meta.meta in
         return (rest_ops @ [ comp ], Ext.combine_extent rest_ext top_extent)
       | FxBinding prev_op_uid ->
         let* comp, comp_ext = pop_input_acc_parsing_elem () in
         (match comp with
          | BoundScannedString str ->
            let* prev_op_meta = lookup_binary_op prev_op_uid in
            let* rest_ops, rest_ext = f prev_op_meta.meta in
            return
              ( rest_ops @ [ A.free_var (Ext.str_with_extent (CS.get_t_string str) comp_ext) ]
              , Ext.combine_extent rest_ext top_extent )
          | _ -> Fail.failwith ("PC248: expected BoundScannedString but got " ^ show_parsing_elem comp)))
    | _ -> Fail.failwith ("PC248: expected " ^ show_binary_op_meta binop ^ " but got " ^ show_parsing_elem top_op ^ " ")
  in
  f binop
;;

let pop_prefix_op_operands (binop : binary_op_meta) : (A.t list * Ext.t) proc_state_m =
  let* top_operand, top_extent = pop_input_acc_expr () in
  let* all_oprands, ext = pop_postfix_op_operands binop in
  return (all_oprands @ [ top_operand ], Ext.combine_extent ext top_extent)
;;

let pop_postfix_op_operands_0 (binop : binary_op_meta) : Ext.t proc_state_m =
  let* all_oprands, ext = pop_postfix_op_operands binop in
  match all_oprands with
  | [] -> return ext
  | _ -> failwith "PC249: expected 0 operand but got more than 0 operands"
;;

let pop_postfix_op_operands_1 (binop : binary_op_meta) : (A.t * Ext.t) proc_state_m =
  let* all_oprands, ext = pop_postfix_op_operands binop in
  match all_oprands with
  | [ x ] -> return (x, ext)
  | _ -> failwith "PC249: expected 1 operand but got more than 1 operands"
;;

let pop_postfix_op_operands_2 (binop : binary_op_meta) : ((A.t * A.t) * Ext.t) proc_state_m =
  let* all_oprands, ext = pop_postfix_op_operands binop in
  match all_oprands with
  | [ x; y ] -> return ((x, y), ext)
  | _ -> failwith "PC249: expected 2 operands but got more than 2 operands"
;;

let pop_postfix_op_operands_3 (binop : binary_op_meta) : ((A.t * A.t * A.t) * Ext.t) proc_state_m =
  let* all_oprands, ext = pop_postfix_op_operands binop in
  match all_oprands with
  | [ x; y; z ] -> return ((x, y, z), ext)
  | _ -> failwith "PC249: expected 3 operands but got more than 3 operands"
;;

let pop_postfix_op_operands_4 (binop : binary_op_meta) : ((A.t * A.t * A.t * A.t) * Ext.t) proc_state_m =
  let* all_oprands, ext = pop_postfix_op_operands binop in
  match all_oprands with
  | [ x; y; z; w ] -> return ((x, y, z, w), ext)
  | _ -> failwith "PC249: expected 4 operands but got more than 4 operands"
;;

let pop_postfix_op_operands_5 (binop : binary_op_meta) : ((A.t * A.t * A.t * A.t * A.t) * Ext.t) proc_state_m =
  let* all_oprands, ext = pop_postfix_op_operands binop in
  match all_oprands with
  | [ x; y; z; w; v ] -> return ((x, y, z, w, v), ext)
  | _ -> failwith "PC249: expected 5 operands but got more than 5 operands"
;;

let pop_prefix_op_operands_1 (binop : binary_op_meta) : (A.t * Ext.t) proc_state_m =
  let* all_oprands, ext = pop_prefix_op_operands binop in
  match all_oprands with
  | [ x ] -> return (x, ext)
  | _ -> failwith "PC249: expected 1 operand but got more than 1 operands"
;;

let pop_prefix_op_operands_2 (binop : binary_op_meta) : ((A.t * A.t) * Ext.t) proc_state_m =
  let* all_oprands, ext = pop_prefix_op_operands binop in
  match all_oprands with
  | [ x; y ] -> return ((x, y), ext)
  | _ -> failwith "PC249: expected 2 operands but got more than 2 operands"
;;

let pop_prefix_op_operands_3 (binop : binary_op_meta) : ((A.t * A.t * A.t) * Ext.t) proc_state_m =
  let* all_oprands, ext = pop_prefix_op_operands binop in
  match all_oprands with
  | [ x; y; z ] -> return ((x, y, z), ext)
  | _ -> failwith "PC249: expected 3 operands but got more than 3 operands"
;;

let pop_prefix_op_operands_4 (binop : binary_op_meta) : ((A.t * A.t * A.t * A.t) * Ext.t) proc_state_m =
  let* all_oprands, ext = pop_prefix_op_operands binop in
  match all_oprands with
  | [ x; y; z; w ] -> return ((x, y, z, w), ext)
  | _ -> failwith "PC249: expected 4 operands but got more than 4 operands"
;;

let pop_prefix_op_operands_5 (binop : binary_op_meta) : ((A.t * A.t * A.t * A.t * A.t) * Ext.t) proc_state_m =
  let* all_oprands, ext = pop_prefix_op_operands binop in
  match all_oprands with
  | [ x; y; z; w; v ] -> return ((x, y, z, w, v), ext)
  | _ -> failwith "PC249: expected 5 operands but got more than 5 operands"
;;

(* extent is the entirety of expressions *)

let pop_bin_operand (binop : binary_op_meta) : ((A.t * A.t) * Ext.t) proc_state_m = pop_prefix_op_operands_2 binop

(* extent is the entirety of expressions *)
let pop_prefix_operand (binop : binary_op_meta) : (A.t * Ext.t) proc_state_m = pop_prefix_op_operands_1 binop
let pop_postfix_operand (binop : binary_op_meta) : (A.t * Ext.t) proc_state_m = pop_postfix_op_operands_1 binop
let pop_closed_identifier_operand (binop : binary_op_meta) : Ext.t proc_state_m = pop_postfix_op_operands_0 binop

let pop_input_acc_past (f : input_acc_elem -> bool) : (input_acc_elem list * input_acc_elem) proc_state_m =
  let rec aux acc =
    let* x = pop_input_acc () in
    if f x then return (acc, x) else aux (x :: acc)
  in
  aux []
;;

let get_current_file_name () : string proc_state_m =
  let* s = get_proc_state () in
  return s.input_future.filename
;;

let get_free_var (expr : A.t) : Ext.t_str proc_state_m =
  match A.view expr with
  | A.FreeVar name -> return name
  | _ -> Fail.failwith ("BP1269: Expecting free variable, got " ^ A.show_view expr)
;;

let aka_print_expr (expr : A.t) : string proc_state_m =
  let* s = get_proc_state () in
  return (EngineDataPrint.aka_print_expr s expr)
;;
