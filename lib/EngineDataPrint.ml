open EngineData

let show_fixity (f : fixity) : string =
  (* let show_meta_in_fixity (b : binary_op_meta) : string =
  "" ^ CS.get_t_string b.keyword ^ ", id=" ^ string_of_int b.id 
  in *)
  match f with
  | FxOp (Some i) -> "FxOp(" ^ string_of_int i ^ ")"
  | FxOp None -> "FxOp(None)"
  | FxNone -> "FxNone"
  | FxBinding b -> "FxBinding(" ^ string_of_int b ^ ")"
  | FxComp b -> "FxComp(" ^ string_of_int b ^ ")"
;;

let show_operator_classification (c : operator_classification) : string =
  match c with
  | Structural -> "S"
  | Expression -> "E"
  | UserDefined -> "U"
;;

let show_binary_op_meta (b : binary_op_meta) : string =
  let { id; keyword; left_fixity; right_fixity; classification } = b in
  "BinOp("
  ^ CS.get_t_string keyword
  ^ ", id="
  ^ string_of_int id
  ^ ", l="
  ^ show_fixity left_fixity
  ^ ", r="
  ^ show_fixity right_fixity
  ^ ", classification="
  ^ show_operator_classification classification
  ^ ")"
;;

let show_parsing_elem (p : parsing_elem) : string =
  match p with
  | ScannedChar s -> "UnknownChar(" ^ CS.get_t_char s ^ ")"
  | Keyword s -> "Keyword(" ^ CS.get_t_string s ^ ")"
  | OpKeyword i -> "OpKeyword(" ^ show_binary_op_meta i ^ ")"
  | BoundScannedString s -> "BoundScannedString(" ^ show_string (CS.get_t_string s) ^ ")"
;;

let show_t_constant (c : t_constant) : string =
  match c with
  | TypeConstructor { name; tp; ocaml_bind_name } ->
    "TypeConstructor("
    ^ Ext.get_str_content name
    ^ ", "
    ^ A.show_view tp
    ^ ", "
    ^ (match ocaml_bind_name with
       | Some s -> s
       | None -> "None")
    ^ ")"
  | DataConstructor { name; tp; tp_id; ocaml_bind_name } ->
    "DataConstructor("
    ^ Ext.get_str_content name
    ^ ", "
    ^ A.show_view tp
    ^ ", "
    ^ string_of_int tp_id
    ^ ", "
    ^ (match ocaml_bind_name with
       | Some s -> s
       | None -> "None")
    ^ ")"
  | TypeExpression tp -> "TypeExpression(" ^ A.show_view tp ^ ")"
  | DataExpression { tp; tm; name } ->
    "DataExpression("
    ^ (match name with
       | Some name -> Ext.get_str_content name
       | None -> "None")
    ^ ", "
    ^ A.show_view tp
    ^ ", "
    ^ (match tm with
       | None -> "None"
       | Some tm -> A.show_view tm)
    ^ ")"
  | PatternVar { tp; name } -> "PatternVar(" ^ Ext.get_str_content name ^ ", " ^ A.show_view tp ^ ")"
  | ModuleAlias { name; filepath } -> "ModuleAlias(" ^ Ext.get_str_content name ^ ", " ^ filepath ^ ")"
;;

let pretty_print_expr (x : A.t) : string =
  match A.view x with
  | A.N (N.ModuleDef, args) ->
    (match List.rev args with
     | ([], x) :: ([], y) :: args ->
       "ModuleDef( "
       ^ string_of_int (List.length args)
       ^ " elements + "
       ^ A.show_view x
       ^ ", "
       ^ A.show_view y
       ^ ", "
       ^ ")"
     | _ -> A.show_view x)
  | _ -> A.show_view x
;;

let rec aka_print_expr (s : proc_state) (x : A.t) : string =
  match A.view x with
  | A.FreeVar name -> Ext.get_str_content name
  (* | A.N (N.Ap, [ ([], f); ([], targ) ]) ->
    "(" ^ aka_print_expr s f ^ " " ^ aka_print_expr s targ ^ ")" *)
  | A.N (N.UnifiableTp id, []) ->
    (match List.assoc_opt id s.unification_ctx |> Option.join with
     | Some tp -> aka_print_expr s tp
     | None -> "?" ^ string_of_int id)
  | A.N (N.Constant id, []) ->
    (match List.assoc_opt id s.constants with
     | Some c ->
       (match c with
        | TypeConstructor { name; _ } -> Ext.get_str_content name
        | DataConstructor { name; _ } -> Ext.get_str_content name
        | TypeExpression _ ->
          (match List.filter (fun (_, cod_id) -> cod_id = id) s.env with
           | [] -> "TE"
           | (name, _) :: _ -> Ext.get_str_content name)
        | DataExpression _ ->
          (match List.filter (fun (_, cod_id) -> cod_id = id) s.env with
           | [] -> "DE"
           | (name, _) :: _ -> Ext.get_str_content name)
        | PatternVar { name; _ } -> Ext.get_str_content name
        | ModuleAlias { name; _ } -> Ext.get_str_content name)
     | None ->
       (match List.filter (fun (_, cod_id) -> cod_id = id) s.env with
        | [] -> "Constant(" ^ string_of_int id ^ ")"
        | (name, _) :: _ -> Ext.get_str_content name))
  | A.N (node_type, args) ->
    let arg_str =
      List.map
        (fun (bound_vars, arg) ->
           String.concat "" (List.map (fun v -> Ext.get_str_content v ^ ".") bound_vars) ^ "" ^ aka_print_expr s arg)
        args
    in
    let arg_str = "[" ^ String.concat "; " arg_str ^ "]" in
    YYNode.show node_type ^ "" ^ arg_str
;;

let show_input_acc_elem (x : input_acc_elem) : string =
  match x with
  | Expr e -> "Expr(" ^ pretty_print_expr e ^ ")"
  | ParsingElem (p, _) -> "ParsingElem(" ^ show_parsing_elem p ^ ")"
;;

let show_input_acc (acc : input_acc_elem list) : string =
  "[" ^ String.concat ";\n " (List.map show_input_acc_elem acc) ^ "]"
;;

(* let show_input_expect (e : expect) : string =
  match e with
  | Expr e -> "Expr(" ^ pretty_print_expr e ^ ")"
  | TopLevel -> "TopLevel"
;; *)

(* | Scanning (InString) -> "Scanning InString"
  | Scanning (InComment) -> "Scanning InComment"
  | Scanning (InLibrarySearch (s)) -> "Scanning InLibrarySearch(" ^ s ^ ")" *)

let show_processor (p : processor) : string =
  match p with
  | ProcComplex _ -> "ProcComplex "
  | ProcBinOp { meta; _ } -> "ProcBinOp: " ^ show_binary_op_meta meta
;;

let show_processor_entry (p : processor_entry) : string =
  match p with
  | { id; name; processor } ->
    "ProcEntry: " ^ name ^ ", id=" ^ string_of_int id ^ ", processor: " ^ show_processor processor
;;

let show_t_constant_short (c : t_constant) : string =
  match c with
  | TypeConstructor _ -> "TC"
  | DataConstructor { tp_id; _ } -> "DC" ^ string_of_int tp_id
  | TypeExpression _ -> "TE"
  | DataExpression { tm; _ } ->
    "DE"
    ^
      (match tm with
      | None -> "N"
      | Some _ -> "S")
  | PatternVar _ -> "PV"
  | ModuleAlias _ -> "MA"
;;

let show_tc_history_elem (s : proc_state) (h : tc_history_elem) : string =
  match h with
  | HistOne (msg, e) -> msg ^ aka_print_expr s e ^ " aka " ^ msg ^ A.show_view e
  | HistTwo (s1, e1, s2, e2) ->
    s1 ^ aka_print_expr s e1 ^ s2 ^ aka_print_expr s e2 ^ " aka " ^ s1 ^ A.show_view e1 ^ s2 ^ A.show_view e2
;;

let show_proc_state (s : proc_state) : string =
  "ProcState: "
  ^ "\ninput_future: "
  ^ CharStream.print_cs s.input_future
  ^ ", "
  ^ "\ninput_future: "
  ^ CharStream.show_cs s.input_future
  ^ ", "
  (* ^ "\ninput_expect: "
  ^ show_input_expect s.input_expect
  ^ ", " *)
  ^ "\ninput_acc: "
  ^ show_input_acc s.input_acc
  ^ ", "
  ^ "\nlast_input_acc_before_pop: "
  ^ (match s.last_input_acc_before_pop with
     | None -> "None"
     | Some xs -> "Some(" ^ show_input_acc xs ^ ")")
  ^ ", "
  (* ^ "\nregistry: " ^ String.concat "\n, " (List.map show_processor_entry s.registry) ^ ", "   *)
  ^ "\n registry: "
  ^ string_of_int (List.length s.registry)
  ^ " entries, "
  ^ "\nfailures: "
  ^ string_of_int (List.length s.failures)
  ^ " entries, "
  ^ "\nlast_succeeded_processor: "
  ^ show_processor_entry s.last_succeeded_processor
  ^ ", "
  ^ "\nenvironment: "
  ^ String.concat ", " (List.map (fun (name, id) -> Ext.get_str_content name ^ ":" ^ string_of_int id) s.env)
  ^ "\nconstants: "
  ^ String.concat ", " (List.map (fun (id, c) -> string_of_int id ^ ":" ^ show_t_constant_short c) s.constants)
  ^ "\ntype_checking_history: "
  ^ String.concat "\n" (List.map (fun h -> " - " ^ show_tc_history_elem s h) s.type_checking_history)
;;
