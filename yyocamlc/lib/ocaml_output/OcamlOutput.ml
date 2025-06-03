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

let get_identifier_name (expr : A.t) : string = 
  match A.view expr with
  | A.FreeVar(name) -> "v_" ^ convert_to_ocaml_identifier_no_uid name
  | _ -> failwith ("OO33: Expecting free variable, got: " ^ A.show_view expr)


let get_ocaml_code (expr : A.t) : string = 
  match A.view expr with
  | A.N(N.Builtin(StringType), []) -> "Builtin.stringType"
  | A.N(N.Builtin(IntType), []) -> "Builtin.intType"
  | A.N(N.Builtin(BoolType), []) -> "Builtin.boolType"
  | A.N(N.Builtin(UnitType), []) -> "Builtin.unitType"
  | A.N(N.Builtin(FloatType), []) -> "Builtin.floatType"
  | A.N(N.Builtin(Type), []) -> "Builtin.typeType"
  | A.N(N.Builtin(N.Int i ), []) -> "Builtin.intValue " ^ string_of_int i
  | A.N(N.Builtin(N.Float(int_part,decimal_part)), []) -> "Builtin.floatValue " ^ int_part ^ "." ^ decimal_part
  | A.N(N.Builtin(N.String s ), []) -> "Builtin.stringValue " ^ "\"" ^ String.escaped s ^ "\""
  | A.N(N.Builtin(N.Unit), []) -> "Builtin.unitValue"
  | A.N(N.Builtin(N.Bool true), []) -> "Builtin.trueValue"
  | A.N(N.Builtin(N.Bool false), []) -> "Builtin.falseValue"
  | _ -> ("(TODO OO25 expr: " ^ A.show_view expr ^ ")")
  (* | _ -> failwith ("OO5: OCaml output not implemented for expression: " ^ A.show_view expr) *)
let process_declaration (decl : A.t) : string = 
  match A.view decl with
  | A.N(N.Declaration(N.ConstantDefn), [[],name;[], value]) ->  (
    match A.view name with
    | A.FreeVar(_) -> (
      "let " ^ get_identifier_name name ^ " = " ^ get_ocaml_code value
    )
    | _ -> failwith ("OO31: Expecting free variable, got: " ^ A.show_view name)
  )
  | _ -> ("(TODO OO32 decl: " ^ A.show_view decl ^ ")")
  (* | _ -> Fail.failwith ("OO29: OCaml output not implemented for declaration: " ^ A.show_view decl) *)


let get_ocaml_code_for_module (module_expr : A.t) : string = 
  let process_declarations (decls : A.t list) : string list = 
    List.map process_declaration decls
  in
  match A.view module_expr with
  | A.N(N.ModuleDef, (decls))-> 
    "(struct \n" ^
    String.concat "\n" (process_declarations (List.map snd decls)) ^
    "\nend)"
  | _ -> failwith ("OO30: Expecting module expression, got: " ^ A.show_view module_expr)

let prelude = 
  String.concat "\n" [
  "type dt = ..";
  "module Builtin = struct";
  "type dt += C_BuiltinStringType";
  "let stringType = C_BuiltinStringType";
  "type dt += C_BuiltinIntType";
  "let intType = C_BuiltinIntType";
  "type dt += C_BuiltinBoolType";
  "let boolType = C_BuiltinBoolType";
  "type dt += C_BuiltinUnitType";
  "let unitType = C_BuiltinUnitType";
  "type dt += C_BuiltinFloatType";
  "let floatType = C_BuiltinFloatType";
  "type dt += C_BuiltinTypeType";
  "let typeType = C_BuiltinTypeType";
  "type dt += C_BuiltinTrueValue";
  "let trueValue = C_BuiltinTrueValue";
  "type dt += C_BuiltinFalseValue";
  "let falseValue = C_BuiltinFalseValue";
  "type dt += C_BuiltinUnitValue";
  "let unitValue = C_BuiltinUnitValue";
  "type dt += C_BuiltinFloatValue of float";
  "let floatValue v = C_BuiltinFloatValue v";
  "type dt += C_BuiltinStringValue of string";
  "let stringValue v = C_BuiltinStringValue v";
  "type dt += C_BuiltinIntValue of int";
  "let intValue v = C_BuiltinIntValue v";
  "end"
  ]
let get_ocaml_code_top_level (files : (string * A.t) list) : string = 
  let output_file (filepath : string) (expr : A.t) : string = 
    let code = get_ocaml_code_for_module expr in
    Printf.sprintf "(* %s *)\nmodule %s = %s" 
      filepath
      (get_module_name_from_filepath filepath) 
      code
  in
  prelude ^ "\n" ^
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




