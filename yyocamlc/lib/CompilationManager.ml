
open EngineData

let compiled_files : (string, A.t) map ref = ref []

let compile_or_retrieve_file_content (filepath : string) : A.t option = 
  match ListUtil.find_elem_by_key !compiled_files filepath with
  | Some (result) ->  Some(result)
  | None -> 
   (* If the file is not already compiled, compile it *)
   (* If the file is already compiled, return the cached result *)
   (* Open the file and read its contents *)
   print_endline ("Processing file: " ^ filepath);
   if not (Sys.file_exists filepath) then
    (
      print_endline ("File not found: " ^ filepath);
      None
    )
   else
    (
      let chan = open_in filepath in
      let length = in_channel_length chan in
      let content = really_input_string chan length in
      close_in chan;
      (* Print the contents of the file *)
      let result = EngineTop.run_top_level filepath content in
      compiled_files := (filepath, result) :: !compiled_files;
      Some(result)
    )

let () = compilation_manager_get_file_hook := compile_or_retrieve_file_content