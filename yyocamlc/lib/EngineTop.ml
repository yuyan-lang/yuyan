open EngineData
open ProcCombinators
open BasicParsing
module Ext = AbtLib.Extent

let do_process_step () : unit proc_state_m =
  let* st = get_proc_state () in
  if (not (CharStream.has_next_char st.input_future)) && List.length st.input_acc = 1
  then failwith "ET57: Should not call process step when we are done";
  (* run the usual processors, if none of succeeded, look for bound identifiers
    if some processor succeeded, discard the bound identifiers
    *)
  (* choice cut has the unnecessary side effct that if only part of a known name belong to an identifier, then that part is also parsed *)
  (* choice_cut *)
  choice (run_processor_entries st.registry) (pfail "ET58: No processor succeeded" (* run_input_acc_identifiers () *))
;;

let rec do_process_entire_stream () : A.t proc_state_m =
  let* st = get_proc_state () in
  if !Flags.show_parse_progress
  then (
    (* if st.input_future.idx mod 40 = 0 then *)
    print_endline
      ("Progress: "
       ^ string_of_int st.input_future.idx
       ^ "/"
       ^ string_of_int (String.length st.input_future.str)
       ^ " "
       ^ CS.show_next_char st.input_future
       ^ " "
       ^ CS.print_vscode_position st.input_future
       ^ " "
       ^ st.last_succeeded_processor.name);
    flush stdout);
  (* if !Flags.show_parse_tracing then print_endline ("=========== STATE ======== \n" ^ show_proc_state st);  *)
  if (not (CharStream.has_next_char st.input_future)) && List.length st.input_acc = 1
  then (
    (* print_endline "SUCCESS"; *)
    match List.hd st.input_acc with
    | Expr x -> return x
    | _ -> failwith ("ET490: Expected an expression but got " ^ show_input_acc_elem (List.hd st.input_acc)))
  else
    (* let* s = get_proc_state () in
      print_endline ("=========== STATE ======== \n" ^ show_proc_state s); *)
    let* _ = do_process_step () in
    do_process_entire_stream ()
;;

let print_proc_errors (msg : proc_error list) : string =
  let expecting =
    List.filter_map
      (function
        | ErrExpectString { expecting = exp; actual = a } -> Some (exp, a)
        | ErrOther _ -> None
        | ErrWithExt _ -> None)
      msg
  in
  let other =
    List.filter_map
      (fun x ->
         match x with
         | ErrExpectString _ -> None
         | ErrOther _ -> Some x
         | ErrWithExt _ -> Some x)
      msg
  in
  let expecting_str =
    if List.length expecting > 0
    then
      "Expecting one of the following strings: "
      ^ String.concat ", " (List.map (fun (x, _) -> "\"" ^ CS.get_t_string x ^ "\"") expecting)
      ^ "\n"
      ^
      if List.length (ListUtil.remove_duplicates (List.map snd expecting)) > 1
      then "But got: " ^ String.concat ", " (List.map (fun (_, x) -> CS.get_t_char (fst x)) expecting)
      else (
        let _, a = List.hd expecting in
        " But got: " ^ CS.get_t_char (fst a) ^ " at the following location: \n" ^ Ext.show_extent_1_based (snd a))
    else ""
  in
  let other_str =
    if List.length other > 0
    then
      "Other errors: "
      ^ String.concat
          ",\n"
          (List.map
             (fun x ->
                match x with
                | ErrExpectString _ -> failwith "ET124"
                | ErrOther s -> s
                | ErrWithExt (s, ext) -> s ^ " at the following location: \n" ^ Ext.show_extent_1_based ext)
             other)
    else ""
  in
  expecting_str ^ (if expecting_str <> "" && other_str <> "" then "\n" else "") ^ other_str
;;

let extract_all_result
      (st : proc_state)
      (processor : A.t proc_state_m)
      (on_success : A.t list -> 'c)
      (on_failure : proc_state -> 'c)
  : 'c
  =
  let result = ref [] in
  let _ =
    processor
      { st with top_failure_handler = (fun s -> if List.is_empty !result then on_failure s else on_success !result) }
      (fun final_s -> if List.is_empty !result then on_failure final_s else on_success !result)
      (fun (r, st') fail_c ->
         result := r :: !result;
         (* print_endline ("EXTRACTALL SUCCESS CALLED" ^ show_proc_state st'); *)
         (* print_endline (">104>>>! result lengt is: " ^ string_of_int (List.length !result) ^ "\n" ); *)
         (* clear st' failure tracking as we now succeeded*)
         (* print_endline ("EXTRACTALL SUCCESS CALLED" ^ show_proc_state st' 
        ^ " OF WHICH FAILURES ARE " ^ (print_proc_errors (List.concat_map fst st'.failures)) ^ "\n" 
        ); *)
         fail_c { st' with failures = [] })
  in
  if List.is_empty !result
  then failwith "SHould not reach here, no result found"
  else
    (* print_endline ("Final state: " ^ (Environment.show_environment st.store)); *)
    on_success !result
;;

let run_top_level (filename : string) (content : string) : A.t =
  let input = CharStream.new_cs filename content in
  let initial_state =
    { input_future = input
    ; input_expect = TopLevel
    ; expect_state_stack = []
    ; input_acc = [ Expr (A.annotate_with_extent (A.fold (A.N (N.ModuleDef, []))) (filename, (0, 0), (0, 0))) ]
    ; store = Environment.default_environment
    ; registry = BuiltinProcessors.default_registry
    ; last_succeeded_processor = to_processor_identifier "initial_none" (CS.new_t_string "[NONE]")
    ; failures = []
    ; (* this is backtracking to top level, directly pass this to handle*)
      top_failure_handler =
        (fun s ->
          print_endline
            ("Failure history has "
             ^ string_of_int (List.length s.failures)
             ^ " entries:\n========================\n"
             ^ String.concat
                 "\n"
                 (List.mapi
                    (fun i (msg, s) ->
                       string_of_int i
                       ^ " failure: "
                       ^ print_proc_errors msg
                       ^ "\n"
                       ^ show_proc_state s
                       ^ "\n-------------------------------")
                    s.failures));
          failwith "Compilation Failed")
    }
  in
  let exception Return of A.t in
  try
    let _ =
      (do_process_entire_stream ()) initial_state initial_state.top_failure_handler (fun (result, _) _ ->
        raise (Return result)
        (* match successes  with
      | [s] -> 
        (* print_endline ("Final state: " ^ (Environment.show_environment s.store)); *)
        raise (Return s)
      | [] -> failwith ("ET76: No final state found")
      | (ss) -> 
        if List.for_all (fun s -> A.eq_abt s (List.hd ss)) ss
          then 
            (
            print_endline ("Final states: " ^ (string_of_int (List.length ss)) ^ " interpretations (CHECK YOUR PARSER) \n");
            raise (Return (List.hd ss))
            )
          else
            (
            print_endline ("Final states: " ^ (string_of_int (List.length ss)) ^ "\n" ^
            String.concat "\n" (List.map A.show_view ss));
            failwith ("ET79: Multiple final states found")
            ) *))
    in
    failwith "ET80: Should not reach here"
  with
  | Return s -> s
;;
