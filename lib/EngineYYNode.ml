module Ext = AbtLib.Extent
module CS = CharStream

let show_string (s : string) : string =
  (* replace \n with \\n *)
  let s = String.concat "\\n" (String.split_on_char '\n' s) in
  s
;;

module YYNode = struct
  type builtin =
    | String of string
    | Int of int
    | Bool of bool
    | Float of string * string
    | Unit
    | UnderscorePattern (* 「」  or （） *)
    | Library of string (* special symbol globally available denoting library root, string denotes a filepath *)
    | StringType
    | IntType
    | BoolType
    | UnitType
    | FloatType
    | RefType
    | ArrayRefType
    | Type
    | RaiseException
    | TryCatch
    | CustomOperatorString of CS.t_string * Ext.t (* this is used for custom operators *)

  type declaration =
    | CustomOperatorDecl
    | ModuleAliasDefn of Ext.t_str * int (* module file path *)
    | ConstantDeclPlaceholder
    | CheckedConstantDefn of Ext.t_str * int
    | ReexportedCheckedConstantDefn of Ext.t_str * int
    | CheckedDirectExpr of int

  type sequence_type =
    | Dot
    | Comma

  type t =
    | Builtin of builtin
    | Declaration of declaration
    | StructureDeref of Ext.t_str (* label *)
    | TupleDeref of int (* numeric projection *)
    | CheckedTupleDeref of
        { idx : int
        ; len : int
        }
    | ModuleDef
    | FileRef of string (* Library is a folder/file, FileRef is a checked file*)
    | ExplicitPi
    | ImplicitPi
    | Arrow
    | Ap
    | ImplicitAp
    | Sequence of sequence_type
    | Match
    | MatchCase
    | TypingAnnotation (* A名x*)
    | Lam
    | ImplicitLam
    | TypedLam
    | ExternalCall of string
    | IfThenElse
    | LetIn
    | RecLetIn
    | UnifiableTp of int
    | Constant of int (* uid of the constant *)
    | ComponentFoldRight (* used for custom operators *)

  let arity (t : t) : int list option =
    match t with
    | Builtin _ -> Some []
    | Declaration (ModuleAliasDefn (_, _)) -> Some []
    | Declaration CustomOperatorDecl -> Some [ 0; 0 ]
    | Declaration ConstantDeclPlaceholder -> Some []
    | Declaration (CheckedConstantDefn (_, _)) -> Some []
    | Declaration (ReexportedCheckedConstantDefn (_, _)) -> Some []
    | Declaration (CheckedDirectExpr _) -> Some []
    | StructureDeref _ -> Some [ 0 ]
    | TupleDeref _ -> Some [ 0 ]
    | CheckedTupleDeref _ -> Some [ 0 ]
    | ModuleDef -> None
    | FileRef _ -> Some []
    | ExplicitPi -> Some [ 0; 1 ]
    | ImplicitPi -> Some [ 1 ]
    | Arrow -> Some [ 0; 0 ]
    | Ap -> None (* also multi-func app exists here *)
    | ImplicitAp -> Some [ 0; 0 ]
    | Sequence _ -> None (* multiargs are flat*)
    | Match -> None (* first arg expr, rest cases *)
    | MatchCase -> Some [ 0; 0 ]
    | Lam -> Some [ 1 ]
    | ImplicitLam -> Some [ 1 ]
    | TypedLam -> Some [ 0; 1 ]
    | TypingAnnotation -> Some [ 0; 0 ]
    | ExternalCall _ -> None (* external call is a special case, arguments appear directly *)
    | IfThenElse -> Some [ 0; 0; 0 ] (* if, then, else *)
    | LetIn -> Some [ 0; 1 ] (* let, in, expr *)
    | RecLetIn -> Some [ 0; 1; 1 ] (* rec let type, defn, in expr *)
    | Constant _ -> Some []
    | UnifiableTp _ -> Some []
    | ComponentFoldRight -> Some [ 0; 0; 0 ]
  ;;

  (* f acc init *)

  let show_builtin (b : builtin) : string =
    match b with
    | String s -> "\"" ^ s ^ "\""
    | Int i -> string_of_int i
    | Bool b -> string_of_bool b
    | Unit -> "unit"
    | UnderscorePattern -> "_"
    | Library s -> "Library(" ^ s ^ ")"
    | StringType -> "StringType"
    | IntType -> "IntType"
    | BoolType -> "BoolType"
    | UnitType -> "UnitType"
    | FloatType -> "FloatType"
    | RefType -> "RefType"
    | ArrayRefType -> "ArrayRefType"
    | Float (s1, s2) -> "Float(" ^ s1 ^ "." ^ s2 ^ ")"
    | Type -> "Type"
    | RaiseException -> "RaiseException"
    | TryCatch -> "TryCatch"
    | CustomOperatorString (s, _) -> "CustomOperatorString(" ^ show_string (CS.get_t_string s) ^ ")"
  ;;

  let show_declaration (d : declaration) : string =
    match d with
    | ModuleAliasDefn (name, path) -> "ModuleAliasDefn(" ^ Ext.get_str_content name ^ ", " ^ string_of_int path ^ ")"
    | CustomOperatorDecl -> "CustomOperatorDecl"
    | ConstantDeclPlaceholder -> "ConstantDeclPlaceholder"
    | CheckedConstantDefn (name, uid) ->
      "CheckedConstantDefn(" ^ Ext.get_str_content name ^ ", " ^ string_of_int uid ^ ")"
    | ReexportedCheckedConstantDefn (name, uid) ->
      "ReexportedCheckedConstantDefn(" ^ Ext.get_str_content name ^ ", " ^ string_of_int uid ^ ")"
    | CheckedDirectExpr uid -> "CheckedDirectExpr(" ^ string_of_int uid ^ ")"
  ;;

  let show_sequence_type (s : sequence_type) : string =
    match s with
    | Dot -> "、"
    | Comma -> "，"
  ;;

  let show (t : t) : string =
    match t with
    | Builtin b -> "Builtin(" ^ show_builtin b ^ ")"
    | Declaration d -> "Declaration(" ^ show_declaration d ^ ")"
    | StructureDeref s -> "StructureDeref(" ^ Ext.get_str_content s ^ ")"
    | TupleDeref i -> "TupleDeref(" ^ string_of_int i ^ ")"
    | CheckedTupleDeref { idx; len } -> "CheckedTupleDeref(" ^ string_of_int idx ^ ", " ^ string_of_int len ^ ")"
    | ModuleDef -> "ModuleDef"
    | FileRef s -> "FileRef(" ^ s ^ ")"
    | ExplicitPi -> "Π"
    | ImplicitPi -> "Π(implicit)"
    | Arrow -> "->"
    | Ap -> "Ap"
    | ImplicitAp -> "Ap(implicit)"
    | Sequence s -> "Sequence(" ^ show_sequence_type s ^ ")"
    | Match -> "Match"
    | MatchCase -> "MatchCase"
    | Lam -> "λ"
    | ImplicitLam -> "λ(implicit)"
    | TypedLam -> "λₜ"
    | TypingAnnotation -> "TypingAnnotationt"
    | ExternalCall s -> "ExternalCall(" ^ s ^ ")"
    | IfThenElse -> "IfThenElse"
    | LetIn -> "LetIn"
    | RecLetIn -> "RecLetIn"
    | UnifiableTp uid -> "UnifiableTp(" ^ string_of_int uid ^ ")"
    | Constant uid -> "Constant(" ^ string_of_int uid ^ ")"
    | ComponentFoldRight -> "ComponentFoldRight"
  ;;
end
