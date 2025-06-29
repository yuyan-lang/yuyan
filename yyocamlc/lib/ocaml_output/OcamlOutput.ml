open EngineData

let convert_to_ocaml_identifier_no_uid (input: string) : string = 
  String.concat "" (
    List.map (fun c -> 
      let char =  CS.get_t_char c in
      if String.length char = 1 then
        (match char.[0] with
        | '/' -> "_"
        | '-' -> "MINUS"
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

let get_constructor_name (expr : A.t) : string = 
  match A.view expr with
  | A.FreeVar(name) -> "C_" ^ convert_to_ocaml_identifier_no_uid name
  | _ -> failwith ("OO33: Expecting free variable, got: " ^ A.show_view expr)

let get_type_constructor_name (expr : A.t) : string = 
  match A.view expr with
  | A.FreeVar(name) -> "t_" ^ convert_to_ocaml_identifier_no_uid name
  | _ -> failwith ("OO33: Expecting free variable, got: " ^ A.show_view expr)

let get_identifier_name (expr : A.t) : string = 
  match A.view expr with
  | A.FreeVar(name) -> "v_" ^ convert_to_ocaml_identifier_no_uid name
  | _ -> failwith ("OO33: Expecting free variable, got: " ^ A.show_view expr)

let rec get_ocaml_type (tp_expr : A.t) : string = 
  match A.view tp_expr with
  | A.N((N.ExplicitPi | N.ImplicitPi), [[],cod;[_bound_name],dom]) -> (
    "(" ^ get_ocaml_type cod ^ " -> " ^ get_ocaml_type dom ^ ")"
  )
  | A.N((N.Arrow), [[],cod;[],dom]) -> (
    "(" ^ get_ocaml_type cod ^ " -> " ^ get_ocaml_type dom ^ ")"
  )
  | A.N(N.StructureDeref(_label), [[],_subject]) -> (
    "dt"
    (* "dt " ^ label ^ " " *)
  )
  | A.FreeVar(_name) -> (
    "dt"
    (* "dt " ^ name ^ " " *)
  )
  | A.N(N.Ap, _) -> (
    "dt"
    (* "dt (* " ^ A.show_view tp_expr ^ " *) " *)
  )
  | _ -> ("(TODO OO30 tp_expr: " ^ A.show_view tp_expr ^ ")")

let rec get_ocaml_constructor_type (tp_expr : A.t) : string list = 
  match A.view tp_expr with
  | A.N((N.ExplicitPi | N.ImplicitPi), [[],dom;[_],cod]) 
  | A.N((N.Arrow), [[],cod;[],dom]) -> (
    get_ocaml_type dom :: get_ocaml_constructor_type cod
  )
  | A.N(N.StructureDeref(_label), [[],_subject]) -> (
    (* ["dt " ^ label ^ " "] *)
    ["dt"]
  )
  | A.FreeVar(_name) -> (
    (* ["dt " ^ name ^ " "] *)
    ["dt"]
  )
  | A.N(N.Ap, _) -> (
    ["dt"]
    (* ["dt " ^ A.show_view tp_expr ^ " "] *)
  )
  | _ -> ["(TODO OO30 cons_tp_expr: " ^ A.show_view tp_expr ^ ")"]

let get_ocaml_tp_expr (tp_expr : A.t) : string = 
  match A.view tp_expr with
  | A.FreeVar(_) -> "dt"
  | _ -> failwith ("OO 78: Not yet implemented, got: " ^ A.show_view tp_expr)

let get_ocaml_type_constructor_type (tp_expr : A.t) : string = 
  let rec aux (tp_expr : A.t) : A.t list = 
    match A.view tp_expr with
    | A.N((N.ExplicitPi | N.ImplicitPi), [[],_dom;[_],_cod])  -> (
      failwith ("OO 78: Higher-order kinds not supported")
    )
    | A.N((N.Arrow), [[],dom;[],cod]) -> (
      dom :: aux cod
    )
    | _ -> failwith ("(TODO OO30 tp_expr: " ^ A.show_view tp_expr ^ ")")
  in
  let tps = aux tp_expr |> List.map get_ocaml_tp_expr in
  "(" ^ String.concat ", " tps ^ ")"

let rec get_ocaml_code_for_pattern (pattern : A.t) : string = 
  (* TODO: pattern match elaboration *)
  match A.view pattern with
  | A.FreeVar(_) -> get_identifier_name pattern
  | A.N(N.Ap, ([], func)::args) -> (
    match A.view func with
    | A.FreeVar(_) -> (
      get_constructor_name func ^ " (" ^ String.concat ", " (List.map (fun x -> snd x |> get_ocaml_code_for_pattern) args) ^ ")"
    )
    | _ -> get_ocaml_code_for_pattern func ^ " (" ^ String.concat ", " (List.map (fun x -> snd x |> get_ocaml_code_for_pattern) args) ^ ")"
  )
  | A.N(N.Sequence("、"), args) -> (
    "(" ^ String.concat ", " (List.map (fun x -> snd x |> get_ocaml_code_for_pattern) args) ^ ")"
  )
  | A.N(N.StructureDeref(label), [[],subject]) -> (
    match A.view subject with
    | A.N(N.FileRef(filepath), []) -> (
      get_module_name_from_filepath filepath ^ "." ^ (get_constructor_name (A.free_var label))
    )
    | _ -> Fail.failwith ("OO94: Not yet implemented, got: " ^ A.show_view pattern)
  )
  | A.N(N.Builtin(N.String s), []) -> (
    "(Builtin.C_BuiltinStringValue \"" ^ String.escaped s ^ "\")"
  )
  | A.N(N.Builtin(N.Int i), []) -> (
    "(Builtin.C_BuiltinIntValue " ^ string_of_int i ^ ")"
  )
  | A.N(N.Builtin(N.Unit), []) -> (
    "(Builtin.C_BuiltinUnitValue)"
  )
  | A.N(N.Builtin(N.Bool true), []) -> (
    "Builtin.C_BuiltinTrueValue"
  )
  | A.N(N.Builtin(N.Bool false), []) -> (
    "Builtin.C_BuiltinFalseValue"
  )
  | _ -> Fail.failwith ("OO86: Not yet implemented, got: " ^ A.show_view pattern)

let rec get_ocaml_code (expr : A.t) : string = 
  match A.view expr with
  | A.N(N.Builtin(StringType), []) -> "Builtin.stringType"
  | A.N(N.Builtin(IntType), []) -> "Builtin.intType"
  | A.N(N.Builtin(BoolType), []) -> "Builtin.boolType"
  | A.N(N.Builtin(UnitType), []) -> "Builtin.unitType"
  | A.N(N.Builtin(FloatType), []) -> "Builtin.floatType"
  | A.N(N.Builtin(Type), []) -> "Builtin.typeType"
  | A.N(N.Builtin(N.Int i ), []) -> "(Builtin.intValue " ^ string_of_int i ^ ")"
  | A.N(N.Builtin(N.Float(int_part,decimal_part)), []) -> "(Builtin.floatValue " ^ int_part ^ "." ^ decimal_part ^ ")"
  | A.N(N.Builtin(N.String s ), []) -> "(Builtin.stringValue " ^ "\"" ^ String.escaped s ^ "\")"
  | A.N(N.Builtin(N.Unit), []) -> "Builtin.unitValue"
  | A.N(N.Builtin(N.Bool true), []) -> "Builtin.trueValue"
  | A.N(N.Builtin(N.Bool false), []) -> "Builtin.falseValue"
  | A.N(N.Builtin(N.RaiseException), []) -> "Builtin.raiseException"
  | A.N(N.ExternalCall(name), []) -> "ExternalCall." ^ name
  | A.N(N.Lam, [[name],body]) -> (
    "fun " ^ get_identifier_name (A.free_var name) ^ "-> " ^ get_ocaml_code body
  )
  | A.FreeVar(_name) -> (
    get_identifier_name expr 
    (* ^ "" ^ name ^ "" *)
  )
  | A.N(N.Ap, [[], func;[], arg]) -> (
    "(" ^ get_ocaml_code func ^ " " ^ get_ocaml_code arg ^ ")"
  )
  | A.N(N.FileRef(filepath), []) -> (
    "(module " ^ get_module_name_from_filepath filepath ^ ")"
  )
  | A.N(N.IfThenElse, [[],cond;[],then_branch;[],else_branch]) -> (
    "(if Builtin.yy_dt_to_bool (" ^ get_ocaml_code cond ^ ") then " ^ get_ocaml_code then_branch ^ " else " ^ get_ocaml_code else_branch ^ ")"
  )
  | A.N(N.StructureDeref(label), [[],subject]) -> (
    "(" ^ get_ocaml_code subject ^ ")." ^ get_identifier_name (A.free_var label)
  )
  |A.N(N.Match, ([],subject)::cases) -> (
    let get_ocaml_code_for_case (case : A.t) : string = 
      match A.view case with
      | A.N(N.MatchCase, [[],pattern;[],body]) -> (
        "| (" ^ get_ocaml_code_for_pattern pattern ^ ") -> (" ^ get_ocaml_code body ^ ")"
      )
      | _ -> Fail.failwith ("OO31: Expecting match case, got: " ^ A.show_view case)
    in
    "(match (" ^ get_ocaml_code subject ^ ") with " ^ String.concat " " (List.map (fun (_, case) -> get_ocaml_code_for_case case) cases) ^ ")"
  )
  | A.N(N.Sequence("、"), args) -> (
    "(" ^ String.concat "," (List.map (fun x -> snd x |> get_ocaml_code) args) ^ ")"
  )
  | _ -> ("(TODO OO25 expr: " ^ A.show_view expr ^ ")")
  (* | _ -> failwith ("OO5: OCaml output not implemented for expression: " ^ A.show_view expr) *)

let process_declaration_in_module (env : OutputEnv.t) (decl : A.t) (decls : A.t list) : string =  
  match A.view decl with
  | A.N(N.Declaration(N.ConstantDefn), [[],name;[], value]) ->  (
    match A.view name with
    | A.FreeVar(fvname) -> (
      match OutputEnv.find_entry_and_remove env fvname with
      | None ->(

      "let " ^ get_identifier_name name ^ " = " ^ get_ocaml_code value
      )
      | Some(tp) -> (
        "let rec " ^ get_identifier_name name ^ " : " ^ get_ocaml_type tp ^ " = " ^ get_ocaml_code value
      )
    )
    | _ -> failwith ("OO31: Expecting free variable, got: " ^ A.show_view name)
  )
  | A.N(N.Declaration(N.ConstantDecl), [[],name;[], value]) ->  (
    match A.view name with
    | A.FreeVar(name) -> (
      OutputEnv.add_entry env name value;
      "(* " ^ name ^ " decl *)"
    )
    | _ -> failwith ("OO31: Expecting free variable, got: " ^ A.show_view name)
  )
  | A.N(N.Declaration(N.ConstructorDecl), [[],name;[], value]) -> (
    match A.view name with
    | A.FreeVar(sname) -> (
      let args = get_ocaml_constructor_type value in
      let args_literals = (List.map (fun arg -> "v_" ^ string_of_int arg) (ListUtil.range 0 (List.length args))) in
      "(* " ^ sname ^ " cons *)\n" ^
      "type dt += " ^ get_constructor_name name  ^ " of " ^ String.concat " * " args ^ "\n" ^
      "let " ^ get_identifier_name name ^ " " ^ String.concat " "  args_literals
      ^ " = " ^ get_constructor_name name ^ " (" ^ String.concat "," args_literals ^ ")"
    )
    | _ -> failwith ("OO31: Expecting free variable, got: " ^ A.show_view name)
  )
  | A.N(N.Declaration(N.TypeConstructorDecl), [[],name;[], value]) -> (
    match A.view name with
    | A.FreeVar(sname) -> (
      let args = get_ocaml_type_constructor_type value in
      "(* " ^ sname ^ " type cons *)\n" ^
      "type " ^ args ^ " " ^ get_type_constructor_name name ^ " = "
    )
    | _ -> failwith ("OO31: Expecting free variable, got: " ^ A.show_view name)
  )
  | A.N(N.Declaration(N.CustomOperatorDecl), _) -> ""
  | _ -> ("(TODO OO32 decl: " ^ A.show_view decl ^ ")")
  (* | _ -> Fail.failwith ("OO29: OCaml output not implemented for declaration: " ^ A.show_view decl) *)


let group_declarations (decls : A.t list) : (A.t * A.t list) list = 
  match decls with
  | [] -> []
  | hd :: tl -> (
    match A.view hd with
    | (A.N(N.Declaration(N.ConstantDecl), _) as this)  -> (
      failwith ""
    )
    | _ -> failwith ""
  )
  | _ -> (failwith ("OO242: Expecting constant declaration, got: " ^ A.show_view decls))

let get_ocaml_code_for_module (module_expr : A.t) : string = 
  let env = OutputEnv.new_env () in
  let process_declarations (decls : A.t list) : string list = 
    List.map (process_declaration_in_module env) decls
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
  "exception BuiltinException of dt";
  "exception YYTypeError";
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
  "let raiseException _ v = raise (BuiltinException v)";
  "(* conversion functions *)";
  "let yy_dt_to_bool v = match v with C_BuiltinTrueValue -> true | C_BuiltinFalseValue -> false | _ -> raise YYTypeError";
  "let yy_bool_to_dt b = if b then C_BuiltinTrueValue else C_BuiltinFalseValue";
  "let yy_dt_to_int v = match v with C_BuiltinIntValue v -> v | _ -> raise YYTypeError";
  "let yy_int_to_dt v = C_BuiltinIntValue v";
  "let yy_dt_to_float v = match v with C_BuiltinFloatValue v -> v | _ -> raise YYTypeError";
  "let yy_float_to_dt v = C_BuiltinFloatValue v";
  "let yy_dt_to_string v = match v with C_BuiltinStringValue v -> v | _ -> raise YYTypeError";
  "let yy_string_to_dt v = C_BuiltinStringValue v";
  "let yy_dt_to_unit v = match v with C_BuiltinUnitValue -> () | _ -> raise YYTypeError";
  "let yy_unit_to_dt () = C_BuiltinUnitValue";
  "end";
  "module ExternalCall = struct";
  "let yyNewRef v = ref v";
  "let yyReadRef v = !v";
  "let yyWriteRef v new_value = v := new_value";
  "let yyNewRefArray v length = Array.make length v";
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




