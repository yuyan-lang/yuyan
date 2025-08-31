open EngineData

let compile_or_retrieve_file_content (filepath : string) : (A.t * t_constants) option =
  match ListUtil.find_elem_by_key !CompilationCache.compiled_files filepath with
  | Some result -> Some result
  | None ->
    (* If the file is not already compiled, compile it *)
    (* If the file is already compiled, return the cached result *)
    (* Open the file and read its contents *)
    print_endline ("Processing file: " ^ filepath);
    if not (Sys.file_exists filepath)
    then (
      print_endline ("File not found: " ^ filepath);
      None)
    else (
      let chan = open_in filepath in
      let length = in_channel_length chan in
      let content = really_input_string chan length in
      close_in chan;
      (* Print the contents of the file *)
      let result = EngineTop.run_top_level filepath content in
      print_endline ("Processing file: " ^ filepath ^ " [DONE]");
      CompilationCache.compiled_files := (filepath, result) :: !CompilationCache.compiled_files;
      Some result)
;;

let output_ocaml () : string =
  let files = List.rev !CompilationCache.compiled_files in
  OcamlOutput.output_ocaml_code_top_level files
;;

let compile_and_run_ocaml (ocaml_filepath : string) (output_path : string option) (compile_only : bool) : unit =
  (* -w -26: disable unused var warnings *)
  (* -I +unix -I +str: add unix and str to the search path *)
  (* unix.cma str.cma: add unix and str to the library path *)
  let flags = " -O3 -w -26 -g -I +unix -I +str unix.cmxa str.cmxa " in
  let exe_path =
    match output_path with
    | Some path -> path
    | None -> ocaml_filepath ^ ".exe"
  in
  print_endline ("[Running] ocamlopt " ^ flags ^ " " ^ ocaml_filepath ^ " -o " ^ exe_path);
  let _ = Sys.command ("ocamlopt " ^ flags ^ " " ^ ocaml_filepath ^ " -o " ^ exe_path) in
  (* Only run the executable if compile_only is false *)
  if not compile_only
  then (
    print_endline ("[Running] " ^ exe_path);
    let _ = Sys.command exe_path in
    ())
  else print_endline ("[已编译] " ^ exe_path)
;;

(* let pout, pin, perr = Unix.open_process_args_full "ocaml" [|"ocaml";ocaml_filepath|] (Unix.environment()) in
  let result = input_line pout in
  let error = input_line perr in
  print_endline "1";
  close_in pout;
  print_endline "2";
  close_out pin;
  print_endline "3";
  close_in perr;
  print_endline "4";
  print_endline ("[Done] Running ocaml " ^ ocaml_filepath);
  print_endline ("stdout:\n" ^ result);
  print_endline ("stderr:\n" ^ error) *)

let () = compilation_manager_get_file_hook := compile_or_retrieve_file_content
