(* Printexc.record_backtrace true;; *)


let process_file (filename : string) =
  (* Open the file and read its contents *)
  print_endline ("Processing file: " ^ filename);
  let chan = open_in filename in
  let length = in_channel_length chan in
  let content = really_input_string chan length in
  close_in chan;
  (* Print the contents of the file *)
  EngineTop.run_top_level filename content 
  (* Print the length of the file *)
  (* let content_list = CharStream.to_utf8_list content in *)
  (* print_endline (ListUtil.show_list content_list (fun x -> x)) *)
  (* Print the length of the file *)

(* Main function to process command line arguments *)
let main () = 
  (* get the first arg*)
  let args = List.tl (Array.to_list (Sys.argv)) in
  match args with
  | [filename] -> process_file filename
  | _ -> failwith ("S21: Expecting single filename")