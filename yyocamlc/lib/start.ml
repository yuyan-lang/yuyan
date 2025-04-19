Printexc.record_backtrace true;;


let process_file (filename : string) =
  CompilationManager.compile_or_retrieve_file_content filename
  

let input_files = ref []
let rec process_args (args : string list) =
  match args with
  | [] -> ()
  | "-pt"::remaining_args -> 
    (
        Flags.show_parse_tracing := true;
        process_args remaining_args
    )
  | file_name::remaining_args -> (
  input_files := file_name::!input_files;
  process_args remaining_args
  )
(* Main function to process command line arguments *)
let main () = 
  (* get the first arg*)
  let args = List.tl (Array.to_list (Sys.argv)) in
  process_args args;
  (* process the input files *)
  match !input_files with
  | [filename] -> process_file filename
  | fs -> failwith ("S21: Expecting single filename, got" ^ String.concat ", " fs)