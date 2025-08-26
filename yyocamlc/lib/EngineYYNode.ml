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
    | Type
    | RaiseException
    | TryCatch
    | CustomOperatorString of CS.t_string (* this is used for custom operators *)

  type declaration =
    | ConstantDefn
    | ConstantDecl
    | ConstructorDecl
    | TypeDefn
    | TypeConstructorDecl
    | CustomOperatorDecl
    | ModuleAliasDefn
    | DirectExpr

  type t =
    | Builtin of builtin
    | Declaration of declaration
    | StructureDeref of string (* label *)
    | TupleDeref of int (* numeric projection *)
    | ModuleDef
    | FileRef of string (* Library is a folder/file, FileRef is a checked file*)
    | ExplicitPi
    | ImplicitPi
    | Arrow
    | Ap
    | Sequence of string (* e.g. ， 或 、*)
    | Match
    | MatchCase
    | TypingAnnotation (* A名x*)
    | Lam
    | TypedLam
    | ExternalCall of string
    | IfThenElse
    | LetIn
    | RecLetIn

  let arity (t : t) : int list option =
    match t with
    | Builtin _ -> Some []
    | Declaration ConstantDefn -> Some [ 0; 0 ]
    | Declaration ConstantDecl -> Some [ 0; 0 ]
    | Declaration ConstructorDecl -> Some [ 0; 0 ]
    | Declaration TypeConstructorDecl -> Some [ 0; 0 ]
    | Declaration ModuleAliasDefn -> Some [ 0; 0 ]
    | Declaration DirectExpr -> Some [ 0 ]
    | Declaration CustomOperatorDecl -> Some [ 0; 0 ]
    | Declaration TypeDefn -> Some [ 0; 0 ]
    | StructureDeref _ -> Some [ 0 ]
    | TupleDeref _ -> Some [ 0 ]
    | ModuleDef -> None
    | FileRef _ -> Some []
    | ExplicitPi -> Some [ 0; 1 ]
    | ImplicitPi -> Some [ 0; 1 ]
    | Arrow -> Some [ 0; 0 ]
    | Ap -> None (* also multi-func app exists here *)
    | Sequence _ -> None (* multiargs are flat*)
    | Match -> None (* first arg expr, rest cases *)
    | MatchCase -> Some [ 0; 0 ]
    | Lam -> Some [ 1 ]
    | TypedLam -> Some [ 0; 1 ]
    | TypingAnnotation -> Some [ 0; 0 ]
    | ExternalCall _ -> Some []
    | IfThenElse -> Some [ 0; 0; 0 ] (* if, then, else *)
    | LetIn -> Some [ 0; 1 ] (* let, in, expr *)
    | RecLetIn -> Some [ 0; 0; 1 ] (* rec let type, defn, in expr *)
  ;;

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
    | Float (s1, s2) -> "Float(" ^ s1 ^ "." ^ s2 ^ ")"
    | Type -> "Type"
    | RaiseException -> "RaiseException"
    | TryCatch -> "TryCatch"
    | CustomOperatorString s -> "CustomOperatorString(" ^ show_string (CS.get_t_string s) ^ ")"
  ;;

  let show_declaration (d : declaration) : string =
    match d with
    | ConstantDefn -> "ConstantDefn"
    | ConstantDecl -> "ConstantDecl"
    | ConstructorDecl -> "ConstructorDecl"
    | TypeConstructorDecl -> "TypeConstructorDecl"
    | ModuleAliasDefn -> "ModuleAliasDefn"
    | DirectExpr -> "DirectExpr"
    | CustomOperatorDecl -> "CustomOperatorDecl"
    | TypeDefn -> "TypeDefn"
  ;;

  let show (t : t) : string =
    match t with
    | Builtin b -> "Builtin(" ^ show_builtin b ^ ")"
    | Declaration d -> "Declaration(" ^ show_declaration d ^ ")"
    | StructureDeref s -> "StructureDeref(" ^ s ^ ")"
    | TupleDeref i -> "TupleDeref(" ^ string_of_int i ^ ")"
    | ModuleDef -> "ModuleDef"
    | FileRef s -> "FileRef(" ^ s ^ ")"
    | ExplicitPi -> "Π"
    | ImplicitPi -> "Π(implicit)"
    | Arrow -> "->"
    | Ap -> "Ap"
    | Sequence s -> "Sequence(" ^ s ^ ")"
    | Match -> "Match"
    | MatchCase -> "MatchCase"
    | Lam -> "λ"
    | TypedLam -> "λₜ"
    | TypingAnnotation -> "TypingAnnotationt"
    | ExternalCall s -> "ExternalCall(" ^ s ^ ")"
    | IfThenElse -> "IfThenElse"
    | LetIn -> "LetIn"
    | RecLetIn -> "RecLetIn"
  ;;
end
