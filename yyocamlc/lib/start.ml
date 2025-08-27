Printexc.record_backtrace true

let print_all_constants () =
  List.iter
    (fun (filepath, (_, constants)) ->
       print_endline ("Constants in " ^ filepath);
       List.iter
         (fun (id, value) -> print_endline ("  " ^ string_of_int id ^ " = " ^ EngineDataPrint.show_t_constant value))
         (List.rev constants))
    (List.rev !CompilationCache.compiled_files)
;;

let process_file (filename : string) =
  let _ = CompilationManager.compile_or_retrieve_file_content (Unix.realpath filename) in
  (* let _ = print_all_constants () in *)
  let file_path = CompilationManager.output_ocaml () in
  print_endline ("[Done] Writing " ^ file_path);
  CompilationManager.compile_and_run_ocaml file_path
;;

let input_files = ref []

let rec process_args (args : string list) =
  match args with
  | [] -> ()
  | "-v" :: remaining_args ->
    Flags.show_parse_progress := true;
    process_args remaining_args
  | "-vtc" :: remaining_args ->
    Flags.show_type_checking_progress := true;
    process_args remaining_args
  | "-pt" :: remaining_args ->
    Flags.show_parse_tracing := true;
    process_args remaining_args
  | file_name :: remaining_args ->
    input_files := file_name :: !input_files;
    process_args remaining_args
;;

(* Main function to process command line arguments *)
let main () =
  (* get the first arg*)
  let args = List.tl (Array.to_list Sys.argv) in
  process_args args;
  (* process the input files *)
  match !input_files with
  | [ filename ] -> process_file filename
  | fs -> failwith ("S21: Expecting single filename, got" ^ String.concat ", " fs)
;;
