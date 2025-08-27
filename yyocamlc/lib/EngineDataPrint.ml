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

let show_binary_op_meta (b : binary_op_meta) : string =
  let { id; keyword; left_fixity; right_fixity } = b in
  "BinOp("
  ^ CS.get_t_string keyword
  ^ ", id="
  ^ string_of_int id
  ^ ", l="
  ^ show_fixity left_fixity
  ^ ", r= "
  ^ show_fixity right_fixity
  ^ ")"
;;

let show_parsing_elem (p : parsing_elem) : string =
  match p with
  | ScannedChar s -> "UnknownChar(" ^ CS.get_t_char s ^ ")"
  | Keyword s -> "Keyword(" ^ CS.get_t_string s ^ ")"
  | OpKeyword (i, _) -> "OpKeyword(" ^ show_binary_op_meta i ^ ")"
  | BoundScannedString s -> "BoundScannedString(" ^ show_string (CS.get_t_string s) ^ ")"
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

let show_t_constant (c : t_constant) : string =
  match c with
  | TypeConstructor { name; tp } -> "TypeConstructor(" ^ Ext.get_str_content name ^ ", " ^ A.show_view tp ^ ")"
  | DataConstructor { name; tp; tp_id } ->
    "DataConstructor(" ^ Ext.get_str_content name ^ ", " ^ A.show_view tp ^ ", " ^ string_of_int tp_id ^ ")"
  | TypeExpression tp -> "TypeExpression(" ^ A.show_view tp ^ ")"
  | DataExpression { tp; tm } ->
    "DataExpression("
    ^ A.show_view tp
    ^ ", "
    ^ (match tm with
       | None -> "None"
       | Some tm -> A.show_view tm)
    ^ ")"
  | PatternVar { tp } -> "PatternVar(" ^ A.show_view tp ^ ")"
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
;;

let show_tc_history_elem (h : tc_history_elem) : string =
  match h with
  | HistOne (s, e) -> s ^ A.show_view e ^ " aka " ^ s ^ pretty_print_expr e
  | HistTwo (s1, e1, s2, e2) ->
    s1 ^ A.show_view e1 ^ s2 ^ A.show_view e2 ^ " aka " ^ s1 ^ pretty_print_expr e1 ^ s2 ^ pretty_print_expr e2
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
  ^ String.concat "\n" (List.map (fun s -> " - " ^ show_tc_history_elem s) s.type_checking_history)
;;
(* "\nlast_succeeded_processor: " ^ show_processor_entry s.last_succeeded_processor ^ ", "  *)
