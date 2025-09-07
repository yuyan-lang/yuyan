open EngineData
open ProcCombinators

let add_token_info (text : Ext.t_str) (token_info : token_info_detail) : unit proc_state_m =
  let* s = get_proc_state () in
  let new_tokens_info =
    { extent = Ext.get_str_extent text; text = Ext.get_str_content text; detail = token_info } :: s.tokens_info
  in
  write_proc_state { s with tokens_info = new_tokens_info }
;;

let show_semantic_token_type (semantic_token_type : semantic_token_type) : string =
  match semantic_token_type with
  | StringConstant -> "StringConstant"
  | NumericConstant -> "NumericConstant"
  | StructureKeyword -> "StructureKeyword"
  | ExpressionKeyword -> "ExpressionKeyword"
  | Identifier -> "Identifier"
  | UserDefinedOperatorKeyword -> "UserDefinedOperatorKeyword"
  | Comment -> "Comment"
;;

let print_token_info_detail (token_info_detail : token_info_detail) : string =
  match token_info_detail with
  | SemanticToken semantic_token_type -> "SemanticToken(" ^ show_semantic_token_type semantic_token_type ^ ")"
  | Definition extent -> "Definition(" ^ Ext.show_extent extent ^ ")"
  | Hover content -> "Hover(" ^ content ^ ")"
  | DiagnosticError content -> "DiagnosticError(" ^ content ^ ")"
;;

let print_token_info (token_info : token_info list) : string =
  String.concat
    "\n"
    (List.map
       (fun (token_info : token_info) ->
          token_info.text ^ " " ^ Ext.show_extent token_info.extent ^ " " ^ print_token_info_detail token_info.detail)
       token_info)
;;

let extent_to_json ((fname, (start_line, start_col), (end_line, end_col)) : Ext.t) : string =
  "{\"file\": \""
  ^ fname
  ^ "\", \"start_line\": "
  ^ string_of_int start_line
  ^ ", \"start_col\": "
  ^ string_of_int start_col
  ^ ", \"end_line\": "
  ^ string_of_int end_line
  ^ ", \"end_col\": "
  ^ string_of_int end_col
  ^ "}"
;;

let token_detail_to_json (token_info_detail : token_info_detail) : string =
  match token_info_detail with
  | SemanticToken semantic_token_type ->
    "{\"type\": \"SemanticToken\", \"semantic_token_type\": \"" ^ show_semantic_token_type semantic_token_type ^ "\"}"
  | Definition extent -> "{\"type\": \"Definition\", \"extent\": " ^ extent_to_json extent ^ "}"
  | Hover content -> "{\"type\": \"Hover\", \"content\": \"" ^ StringEscape.escaped_unicode content ^ "\"}"
  | DiagnosticError content ->
    "{\"type\": \"DiagnosticError\", \"content\": \"" ^ StringEscape.escaped_unicode content ^ "\"}"
;;

let token_info_to_json (token_info : token_info list) : string =
  "["
  ^ String.concat
      ",\n"
      (List.map
         (fun (token_info : token_info) ->
            "{\"text\": \""
            ^ StringEscape.escaped_unicode token_info.text
            ^ "\", \"extent\": "
            ^ extent_to_json token_info.extent
            ^ ",\n    \"detail\": "
            ^ token_detail_to_json token_info.detail
            ^ "}")
         token_info)
  ^ "]"
;;
