open EngineData
open ProcCombinators
open BasicParsing
open EngineDataPrint
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
  run_processor_entries st.registry
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
  if List.length msg > 0
  then
    string_of_int (List.length msg)
    ^ " errors: "
    ^ String.concat
        ",\n"
        (List.map
           (fun x ->
              match x with
              | { msg = s; ext; state = _ } -> s ^ " at the following location: \n" ^ Ext.show_extent_1_based ext)
           msg)
  else "Did not succeed but no reportable errors"
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

let dump_token_info (filepath : string) (st : proc_state) : unit =
  let path = Filename.concat "./_build/lsp_tokens_info/" (filepath ^ ".tokens.json") in
  FileSystemUtils.makedir_p path;
  let oc = open_out path in
  output_string oc (TokenInfo.token_info_to_json st.tokens_info);
  close_out oc;
  ()
;;

let dump_diagnostic_info (_filepath : string) (st : proc_state) : unit =
  let path = Filename.concat "./_build/lsp_tokens_info/" "diagnostics.json" in
  FileSystemUtils.makedir_p path;
  let oc = open_out path in
  output_string
    oc
    (TokenInfo.token_info_to_json
       (List.map (fun err -> { extent = err.ext; text = "<diagnostic>"; detail = DiagnosticError err.msg }) st.failures));
  close_out oc;
  ()
;;

let run_top_level (filename : string) (content : string) : A.t * t_constants =
  let input = CharStream.new_cs filename content in
  let initial_state =
    { input_future =
        input
        (* ; input_expect = TopLevel
    ; expect_state_stack = [] *)
    ; input_acc = [ Expr (A.annotate_with_extent (A.fold (A.N (N.ModuleDef, []))) (filename, (0, 0), (0, 0))) ]
    ; last_input_acc_before_pop = None
    ; constants = Environment.default_constants
    ; env = Environment.default_env
    ; registry = BuiltinProcessors.default_registry
    ; last_succeeded_processor = { id = -1; name = "initial_none"; processor = ProcComplex (return ()) }
    ; failures = []
    ; (* this is backtracking to top level, directly pass this to handle*)
      top_failure_handler =
        (fun s ->
          dump_diagnostic_info filename s;
          dump_token_info filename s;
          print_endline ("========\nFailure history has " ^ print_proc_errors s.failures ^ "\n========\n");
          failwith "Compilation Failed")
    ; type_checking_history = []
    ; unification_ctx = []
    ; tokens_info = []
    }
  in
  let exception Return of A.t * t_constants in
  try
    let _ =
      (do_process_entire_stream ()) initial_state initial_state.top_failure_handler (fun (result, s) _ ->
        dump_token_info filename s;
        raise (Return (result, s.constants))
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
  | Return (result, constants) -> result, constants
;;
