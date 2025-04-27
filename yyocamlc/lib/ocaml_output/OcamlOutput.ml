open EngineData

let convert_to_ocaml_identifier_no_uid (input: string) : string = 
  String.concat "" (
    List.map (fun c -> 
      let char =  CS.get_t_char c in
      if String.length char = 1 then
        (match char.[0] with
        | '/'
        -> "_"
        | _ -> char
        )
      else 
        (
          PinyinConverter.convert_to_pinyin c
        )
      ) (CS.new_t_string input)
  )

let get_module_name_from_filepath (filepath : string) : string = 
  "M_" ^ (convert_to_ocaml_identifier_no_uid (filepath))

let get_ocaml_code (expr : A.t) : string = 
  match A.view expr with
  | _ -> failwith ("OO5: OCaml output not implemented for expression: " ^ A.show_view expr)

let get_ocaml_code_for_module (module_expr : A.t) : string = 
  let process_declarations (_decls : A.t list) : string list = 
    ["(*<decls>*)"]
  in
  match A.view module_expr with
  | A.N(N.ModuleDef, (decls))-> 
    "(struct \n" ^
    String.concat "\n" (process_declarations (List.map snd decls)) ^
    "\nend)"
  | _ -> failwith ("OO30: Expecting module expression, got: " ^ A.show_view module_expr)

let get_ocaml_code_top_level (files : (string * A.t) list) : string = 
  let output_file (filepath : string) (expr : A.t) : string = 
    let code = get_ocaml_code_for_module expr in
    Printf.sprintf "(* %s *)\nmodule %s = %s" 
      filepath
      (get_module_name_from_filepath filepath) 
      code
  in
  String.concat "\n" (List.map (fun (filepath, expr) -> output_file filepath expr) files)


let rec makedir_p (path : string) : unit = 
  let dir = Filename.dirname path in
  (* print_endline ("makedir_p: " ^ dir);   *)
  if not (Sys.file_exists dir) then
    (
      makedir_p dir;
      Unix.mkdir dir 0o755
    )

let output_ocaml_code_top_level (files : (string * A.t) list) : string = 
  let final_string = get_ocaml_code_top_level files in
  (* write to ./.yybuild.nosync/ocaml/<lastfilename>.ml *)
  let filename = List.hd (List.rev (List.map fst files)) in
  let path = Filename.concat "./.yybuild.nosync/ocaml" (
    convert_to_ocaml_identifier_no_uid (Filename.basename filename) ^ ".ml") in
  makedir_p path;
  let oc = open_out path in
  output_string oc final_string;
  close_out oc;
  path




