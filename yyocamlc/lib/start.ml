Printexc.record_backtrace true;;


let process_file (filename : string) =
  CompilationManager.compile_or_retrieve_file_content filename
  

(* Main function to process command line arguments *)
let main () = 
  (* get the first arg*)
  let args = List.tl (Array.to_list (Sys.argv)) in
  match args with
  | [filename] -> process_file filename
  | _ -> failwith ("S21: Expecting single filename")