open EngineData
module StringSet = Set.Make (String)
module IntSet = Set.Make (Int)

let convert_to_ocaml_identifier_no_uid (input : string) : string =
  String.concat
    ""
    (List.map
       (fun c ->
          let char = CS.get_t_char c in
          if String.length char = 1
          then (
            match char.[0] with
            | '/' -> "_"
            | '-' -> "MINUS"
            | _ -> char)
          else PinyinConverter.convert_to_pinyin c)
       (CS.new_t_string input))
;;

(* Variable environment for mapping Chinese names to v_<uid> identifiers *)
type var_env = (string * string) list

let empty_var_env : var_env = []

let add_var_env (env : var_env) (bnd_name : string) : var_env * string (* var name *) =
  let new_name = "v_" ^ string_of_int (Uid.next ()) in
  (bnd_name, new_name) :: env, new_name
;;

let lookup_var_env (env : var_env) (chinese_name : string) : string =
  match List.assoc_opt chinese_name env with
  | Some ocaml_name -> ocaml_name
  | None -> failwith (__LOC__ ^ ": cannot find " ^ chinese_name ^ " in var_env")
;;

let get_or_create_var_id (env : var_env ref) (chinese_name : string) : string =
  match List.assoc_opt chinese_name !env with
  | Some ocaml_name -> ocaml_name
  | None ->
    let new_name = "v_" ^ string_of_int (Uid.next ()) in
    env := (chinese_name, new_name) :: !env;
    new_name
;;

let lookup_constant_id (id : int) : t_constant =
  match List.assoc_opt id (CompilationCache.get_all_constants ()) with
  | Some x -> x
  | None -> Fail.failwith ("OO232: Cannot find constant " ^ string_of_int id)
;;

let get_comment_str (str : string) : string = "(* " ^ str ^ " *)"
let annotate_with_name (str : string) : string = " " ^ get_comment_str str
let get_comment_ext_str (ext_str : Ext.t_str) : string = get_comment_str (Ext.get_str_content ext_str)
let annotate_with_name_ext (ext_str : Ext.t_str) : string = " " ^ get_comment_ext_str ext_str

let rec get_ocaml_code_for_pattern (var_env : var_env) (pattern : A.t) : string =
  (* TODO: pattern match elaboration *)
  match A.view pattern with
  | A.N (N.Builtin (N.String _), [])
  | A.N (N.Builtin (N.Int _), [])
  | A.N (N.Builtin N.Unit, [])
  | A.N (N.Builtin (N.Bool _), []) -> get_ocaml_code var_env pattern
  | A.N (N.Sequence Dot, elems) ->
    "(" ^ String.concat ", " (List.map (fun (_, x) -> get_ocaml_code_for_pattern var_env x) elems) ^ ")"
  | A.N (N.Constant id, []) ->
    (match lookup_constant_id id with
     | DataConstructor { name; _ } -> "C_" ^ string_of_int id ^ annotate_with_name_ext name
     | PatternVar { name; _ } -> "p_" ^ string_of_int id ^ annotate_with_name_ext name
     | tcons -> Fail.failwith (__LOC__ ^ ": Expecting data constructor, got: " ^ EngineDataPrint.show_t_constant tcons))
  | A.N (N.Ap, [ ([], func); ([], arg) ]) ->
    "(" ^ get_ocaml_code_for_pattern var_env func ^ " " ^ get_ocaml_code_for_pattern var_env arg ^ ")"
  | _ -> Fail.failwith (__LOC__ ^ ": Not yet implemented, got: " ^ A.show_view pattern)

and get_ocaml_code (var_env : var_env) (expr : A.t) : string =
  match A.view expr with
  | A.N (N.Builtin (N.Int i), []) -> string_of_int i
  | A.N (N.Builtin (N.Float (int_part, decimal_part)), []) -> int_part ^ "." ^ decimal_part
  | A.N (N.Builtin (N.String s), []) -> "\"" ^ String.escaped s ^ "\""
  | A.N (N.Builtin N.Unit, []) -> "()"
  | A.N (N.Builtin (N.Bool true), []) -> "true"
  | A.N (N.Builtin (N.Bool false), []) -> "false"
  | A.N (N.Lam, [ ([ name ], body) ]) ->
    let env', v_name = add_var_env var_env name in
    "(fun " ^ v_name ^ "-> " ^ get_ocaml_code env' body ^ ")"
  | A.FreeVar name -> lookup_var_env var_env name
  | A.N (N.Ap, [ ([], func); ([], arg) ]) -> "(" ^ get_ocaml_code var_env func ^ " " ^ get_ocaml_code var_env arg ^ ")"
  | A.N (N.IfThenElse, [ ([], cond); ([], then_branch); ([], else_branch) ]) ->
    "(if ("
    ^ get_ocaml_code var_env cond
    ^ ") then "
    ^ get_ocaml_code var_env then_branch
    ^ " else "
    ^ get_ocaml_code var_env else_branch
    ^ ")"
  | A.N (N.Match, ([], subject) :: cases) ->
    let get_ocaml_code_for_case (case : A.t) : string =
      match A.view case with
      | A.N (N.MatchCase, [ ([], pattern); ([], body) ]) ->
        "| (" ^ get_ocaml_code_for_pattern var_env pattern ^ ") -> (" ^ get_ocaml_code var_env body ^ ")"
      | _ -> Fail.failwith ("OO232: Expecting match case, got: " ^ A.show_view case)
    in
    "(match ("
    ^ get_ocaml_code var_env subject
    ^ ") with "
    ^ String.concat " " (List.map (fun (_, case) -> get_ocaml_code_for_case case) cases)
    ^ ")"
  | A.N (N.Sequence Dot, args) ->
    "(" ^ String.concat "," (List.map (fun x -> snd x |> get_ocaml_code var_env) args) ^ ")"
  | A.N (N.ImplicitLam, [ ([ bnd ], body) ]) ->
    let env', bnd_name = add_var_env var_env bnd in
    get_comment_str ("(type " ^ bnd_name ^ annotate_with_name bnd ^ " )") ^ get_ocaml_code env' body
  | A.N (N.ImplicitAp, [ ([], func); ([], targ) ]) ->
    "(" ^ get_ocaml_code var_env func ^ " " ^ get_comment_str (get_type_code var_env targ) ^ ")"
  | A.N (N.ExternalCall fname, args) ->
    "(" ^ fname ^ " (" ^ String.concat ", " (List.map (fun (_, x) -> get_ocaml_code var_env x) args) ^ "))"
  | A.N (N.Constant id, []) ->
    (match lookup_constant_id id with
     | TypeConstructor { name; _ } -> "t_" ^ string_of_int id ^ annotate_with_name_ext name
     | tcons -> Fail.failwith (__LOC__ ^ ": Expecting type constructor, got: " ^ EngineDataPrint.show_t_constant tcons))
  | _ -> Fail.failwith (__LOC__ ^ ": Not yet implemented, got: " ^ A.show_view expr)

and get_type_code (env : var_env) (tp : A.t) : string =
  match A.view tp with
  | A.FreeVar name -> "'" ^ lookup_var_env env name
  | A.N (N.Ap, [ ([], func); ([], arg) ]) -> get_type_code env arg ^ " " ^ get_type_code env func
  | A.N (N.Constant id, []) ->
    (match lookup_constant_id id with
     | TypeConstructor _ -> "t_" ^ string_of_int id
     | _ -> Fail.failwith (__LOC__ ^ ": Expecting type constructor, got: " ^ string_of_int id))
  | A.N (N.Sequence Comma, args) -> "(" ^ String.concat " * " (List.map (fun (_, x) -> get_type_code env x) args) ^ ")"
  | A.N (N.Builtin StringType, []) -> "string"
  | A.N (N.Builtin IntType, []) -> "int"
  | A.N (N.Builtin BoolType, []) -> "bool"
  | A.N (N.Builtin UnitType, []) -> "unit"
  | A.N (N.Builtin FloatType, []) -> "float"
  | A.N (N.Arrow, [ ([], dom); ([], cod) ]) -> "(" ^ get_type_code env dom ^ " -> " ^ get_type_code env cod ^ ")"
  | A.N (N.ImplicitPi, [ ([ bnd ], cod) ]) ->
    let env', y_name = add_var_env env bnd in
    get_comment_str ("'" ^ y_name) ^ get_type_code env' cod
  | A.N (N.UnifiableTp id, []) -> "?" ^ string_of_int id
  | _ -> Fail.failwith (__LOC__ ^ ": Expecting free variable, got: " ^ A.show_view tp)
;;

let rec get_data_constructor_type_code (env : var_env) (id : int) (tp : A.t) : string =
  match A.view tp with
  | A.N (N.Arrow, [ ([], dom); ([], cod) ]) ->
    "| C_" ^ string_of_int id ^ " : " ^ get_type_code env dom ^ " -> " ^ get_type_code env cod
  | A.N (N.ImplicitPi, [ ([ bnd ], cod) ]) ->
    let env', _ = add_var_env env bnd in
    get_data_constructor_type_code env' id cod
  | A.N (N.Ap, _) -> "| C_" ^ string_of_int id ^ " : " ^ get_type_code env tp
  | _ -> Fail.failwith (__LOC__ ^ ": Expecting arrow or implicit pi, got: " ^ A.show_view tp)
;;

let get_type_constructor_code (_env : var_env) (id : int) (tp : A.t) : string =
  match A.view tp with
  | A.N (N.Arrow, [ ([], _); ([], _) ]) -> "_ t_" ^ string_of_int id ^ " = "
  | A.N (N.Builtin Type, []) -> "t_" ^ string_of_int id ^ " = "
  | _ -> Fail.failwith (__LOC__ ^ ": Expecting arrow or type, got: " ^ A.show_view tp)
;;

(* Generate type definitions only *)
let generate_type_definitions (constants : t_constants) : string =
  let all_type_defs =
    List.filter_map
      (fun (tcons_id, const) ->
         match const with
         | TypeConstructor { name; tp } ->
           Some
             (get_comment_ext_str name
              ^ "\n"
              ^ get_type_constructor_code empty_var_env tcons_id tp
              ^ "\n"
              ^
              let constructors =
                List.filter_map
                  (fun (cons_id, const) ->
                     match const with
                     | DataConstructor { name; tp; tp_id } ->
                       if tp_id = tcons_id then Some (cons_id, tp, name) else None
                     | _ -> None)
                  constants
              in
              if List.length constructors > 0
              then
                String.concat
                  "\n"
                  (List.map
                     (fun (cons_id, cons_tp, cons_name) ->
                        get_comment_ext_str cons_name
                        ^ "\n"
                        ^ get_data_constructor_type_code empty_var_env cons_id cons_tp)
                     constructors)
              else "|")
         | _ -> None)
      constants
  in
  if List.length all_type_defs = 0 then "" else "type\n" ^ String.concat "\nand\n" all_type_defs
;;

let get_ocaml_code_for_module (filepath : string) (module_expr : A.t) (constants : t_constants) : string =
  match A.view module_expr with
  | A.N (N.ModuleDef, decls) ->
    get_comment_str filepath
    ^ "\n\n"
    ^ generate_type_definitions constants
    ^ String.concat
        "\n"
        (List.map
           (fun (_, decl) ->
              match A.view decl with
              | A.N (N.Declaration N.CustomOperatorDecl, _) -> get_comment_str ("Not Impl: " ^ A.show_view decl)
              | A.N (N.Declaration N.ModuleAliasDefn, _) -> get_comment_str ("Not Impl: " ^ A.show_view decl)
              | A.N (N.Declaration (N.CheckedConstantDefn (name, id)), []) ->
                (match lookup_constant_id id with
                 | DataExpression { tp; tm = Some tm } ->
                   let is_recursive = List.mem id (AbtUtil.get_referenced_constant_ids tm) in
                   get_comment_ext_str name
                   ^ "\n"
                   ^ "let "
                   ^ (if is_recursive then "rec " else "")
                   ^ "v_"
                   ^ string_of_int id
                   ^ " : "
                   ^ get_type_code empty_var_env tp
                   ^ " = "
                   ^ get_ocaml_code empty_var_env tm
                   ^ "\n"
                 | TypeConstructor _ | DataConstructor _ -> ""
                 | TypeExpression tp ->
                   get_comment_ext_str name
                   ^ "\n"
                   ^ "type t_"
                   ^ string_of_int id
                   ^ " = "
                   ^ get_type_code empty_var_env tp
                 | tcons ->
                   Fail.failwith (__LOC__ ^ ": Expecting data expression, got: " ^ EngineDataPrint.show_t_constant tcons))
              | _ -> Fail.failwith (__LOC__ ^ ": Doesn't know how to handle declaration: " ^ A.show_view decl))
           decls)
  | _ -> failwith ("OO30: Expecting module expression, got: " ^ A.show_view module_expr)
;;

let get_ocaml_code_top_level (files : (string * (A.t * t_constants)) list) : string =
  String.concat
    "\n"
    (List.map (fun (filepath, (expr, constants)) -> get_ocaml_code_for_module filepath expr constants) files)
;;

let rec makedir_p (path : string) : unit =
  let dir = Filename.dirname path in
  (* print_endline ("makedir_p: " ^ dir);   *)
  if not (Sys.file_exists dir)
  then (
    makedir_p dir;
    Unix.mkdir dir 0o755)
;;

let output_ocaml_code_top_level (files : (string * (A.t * t_constants)) list) : string =
  let final_string = get_ocaml_code_top_level files in
  (* write to ./.yybuild.nosync/ocaml/<lastfilename>.ml *)
  let filename = List.hd (List.rev (List.map (fun (filepath, _) -> filepath) files)) in
  let path =
    Filename.concat "./.yybuild.nosync/ocaml" (convert_to_ocaml_identifier_no_uid (Filename.basename filename) ^ ".ml")
  in
  makedir_p path;
  let oc = open_out path in
  output_string oc final_string;
  close_out oc;
  path
;;
