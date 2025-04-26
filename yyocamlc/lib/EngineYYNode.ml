
module Ext = AbtLib.Extent
module CS = CharStream

type fixity = 
              FxOp of int option
            | FxNone 
            | FxBinding of int (* uid of the component *)
            | FxComp of int 
and binary_op_meta = {
  id : int;
  keyword : CS.t_string;
  (* fixity is the behavior of the operator when viewed from left for left_fixity, and right for right_fixity *)
  left_fixity : fixity; 
  right_fixity : fixity;
  (* 
    interpretation of left_fixity 
        FxOp of int (* infix or postfix *) [precedence when viewed from left]
      | None (* prefix or identifier *) [viewing prefix from left is the same as viewing an identifier]
      | FxBinding of binary_op_meta  [ends a binding]
      | FxComp of binary_op_meta (* previos component, e.g. A【】B, B's leftfix is FxComp of A*)
    interpretation of right_fixity
        FxOp of int (* infix or prefix *) [precedence when viewed from right]
      | FxNone (* postfix or identifier *) [vieweing postfix from right is the same as viewing an identifier]
      | FxBinding of binary_op_meta  [starts a binding]
      | FxComp of binary_op_meta (* previos component, e.g. A【】B, A's rightfix is FxComp of B*)
  *)
}

let show_string (s : string) : string = 
  (* replace \n with \\n *)
  let s = String.concat "\\n" (String.split_on_char '\n' s) in
  s

let show_fixity (f : fixity) : string =
  (* let show_meta_in_fixity (b : binary_op_meta) : string =
    "" ^ CS.get_t_string b.keyword ^ ", id=" ^ string_of_int b.id 
  in *)
  match f with
  | FxOp (Some i) -> "FxOp(" ^ string_of_int i ^ ")"
  | FxOp (None) -> "FxOp(None)"
  | FxNone -> "FxNone"
  | FxBinding (b) -> "FxBinding(" ^ string_of_int b ^ ")"
  | FxComp (b) -> "FxComp(" ^ string_of_int b ^ ")"


let show_binary_op_meta (b : binary_op_meta) : string =
  let {id; keyword; left_fixity; right_fixity} = b in
  "BinOp(" ^ CS.get_t_string keyword ^ ", id=" ^ string_of_int id ^
    ", l=" ^ show_fixity left_fixity  ^
    ", r= " ^ show_fixity right_fixity ^
     ")"

module YYNode  = struct
  type builtin = String of string
               | Int of int
               | Bool of bool
               | Float of string * string
               | Unit
               | UnderscorePattern (* 「」  or （） *)
               | Library of string  (* special symbol globally available denoting library root, string denotes a filepath *)
               | StringType
               | IntType
               | BoolType
               | UnitType
               | FloatType
               | Type
               | RaiseException
               | TryCatch
                | CustomOperatorString of CS.t_string (* this is used for custom operators *)


  type parsing_elem = ScannedChar of CS.t_char
                    | Keyword of CS.t_string
                    | BoundScannedString of CS.t_string (* this is used for operator that binds a name *)
                    | OpKeyword of binary_op_meta (* uid of the binary op *)

  type declaration = ConstantDefn 
                  | ConstantDecl 
                  | ConstructorDecl
                  | CustomOperatorDecl
                  | DirectExpr

  type t = Builtin of builtin
         | ParsingElem of parsing_elem
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

  let arity (t : t) : int list option = 
    match t with
    | Builtin (_) -> Some([])
    | ParsingElem (_) -> Some([])
    | Declaration ConstantDefn  -> Some([0; 0])
    | Declaration ConstantDecl  -> Some([0; 0])
    | Declaration ConstructorDecl -> Some([0; 0])
    | Declaration DirectExpr -> Some([0])
    | Declaration CustomOperatorDecl -> Some([0; 0])
    | StructureDeref (_) -> Some([0])
    | TupleDeref (_) -> Some([0])
    | ModuleDef -> None
    | FileRef (_) -> Some([])
    | ExplicitPi -> Some([0; 1])
    | ImplicitPi -> Some([0; 1])
    | Arrow -> Some([0; 0])
    | Ap -> None (* also multi-func app exists here *)
    | Sequence _ -> None (* multiargs are flat*)
    | Match -> None (* first arg expr, rest cases *)
    | MatchCase -> Some([0; 0])
    | Lam -> Some([1])
    | TypedLam -> Some([0;1])
    | TypingAnnotation -> Some([0; 0])
    | ExternalCall _ -> Some([])
    | IfThenElse -> Some([0; 0; 0]) (* if, then, else *)
    | LetIn -> Some([0; 1]) (* let, in, expr *)






  let show_builtin (b : builtin) : string = 
    match b with
    | String (s) -> "\"" ^ s ^ "\""
    | Int (i) -> string_of_int i
    | Bool (b) -> string_of_bool b
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
    | CustomOperatorString (s) -> "CustomOperatorString(" ^ show_string (CS.get_t_string s) ^ ")"


  let show_parsing_elem (p : parsing_elem) : string =
    match p with
    | ScannedChar (s) -> "UnknownChar(" ^ CS.get_t_char s ^ ")"
    | Keyword (s) -> "Keyword(" ^ CS.get_t_string s ^ ")"
    | OpKeyword (i) -> "OpKeyword(" ^ show_binary_op_meta i ^ ")"
    | BoundScannedString (s) -> "BoundScannedString(" ^ show_string (CS.get_t_string s) ^ ")"

  let show_declaration (d : declaration) : string =
    match d with
    | ConstantDefn -> "ConstantDefn"
    | ConstantDecl -> "ConstantDecl"
    | ConstructorDecl -> "ConstructorDecl"
    | DirectExpr -> "DirectExpr"
    | CustomOperatorDecl -> "CustomOperatorDecl"


  let show (t : t) : string =
    match t with
    | Builtin (b) -> "Builtin(" ^ show_builtin b ^ ")"
    | ParsingElem (p) -> "ParsingElem(" ^ show_parsing_elem p ^ ")"
    | Declaration (d) -> "Declaration(" ^ show_declaration d ^ ")"
    | StructureDeref (s) -> "StructureDeref(" ^ s ^ ")"
    | TupleDeref (i) -> "TupleDeref(" ^ string_of_int i ^ ")"
    | ModuleDef -> "ModuleDef"
    | FileRef (s) -> "FileRef(" ^ s ^ ")"
    | ExplicitPi -> "Π"
    | ImplicitPi -> "Π(implicit)"
    | Arrow -> "->"
    | Ap -> "Ap"
    | Sequence (s) -> "Sequence(" ^ s ^ ")"
    | Match -> "Match"
    | MatchCase -> "MatchCase"
    | Lam -> "λ"
    | TypedLam -> "λₜ"
    | TypingAnnotation -> "TypingAnnotationt"
    | ExternalCall (s) -> "ExternalCall(" ^ s ^ ")"
    | IfThenElse -> "IfThenElse"
    | LetIn -> "LetIn"




end