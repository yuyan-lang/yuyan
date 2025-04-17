open EngineData
open ProcCombinators

module PE = ProcessedElement

let get_module_real_path (module_name_parsed : PE.t list) : string proc_state_m (* path *)  = 
  match List.map A.view module_name_parsed with
  | [] -> failwith "Module name cannot be empty"
  | [A.N(N.Builtin(N.String path), _)] ->
      let* path = 
      (if String.starts_with ~prefix:"./" path then
        let* cur_module_path = get_current_file_name () in
        let cur_module_dir = Filename.dirname cur_module_path in
        let path = Filename.concat cur_module_dir (String.sub path 2 (String.length path - 2)) in
          return path
      else 
        return path
      ) in
      let realpath = Unix.realpath path in
      realpath
  | _ -> failwith "Expecting single file path"

  



let process_imports (directive : PE.t) (module_name_parsed : PE.t list) : unit proc_state_m = 
  let directive_string = 
    (match A.view directive with
    | A.N(N.ParsingElem(N.Keyword(s, _)), _) -> s
    | _ -> failwith "Expected a keyword"
    ) in
  let* path = get_module_real_path module_name_parsed in
  CompilationManager.compile_or_retrieve_file_content path;

  
  
