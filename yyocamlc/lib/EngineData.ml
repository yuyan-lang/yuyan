

(* A state is a charstream (for pending output )
a list of currently processed stack elements (and environment at that time (for backtracking?) )
and a current environment generator

[Actually it is a list of them, or a decision tree of past states]
*)

module Ext = AbtLib.Extent
module CS = CharStream

type fixity = 
              FxOp of int 
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
  | FxOp (i) -> "FxOp(" ^ string_of_int i ^ ")"
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
         | TypeAnnotated (* A名x*)
         | Lam
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
    | Sequence _ -> Some([0; 0])
    | Match -> None (* first arg expr, rest cases *)
    | MatchCase -> Some([0; 0])
    | Lam -> Some([1])
    | TypeAnnotated -> Some([0; 0])
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
    | TypeAnnotated -> "TypeAnnotated"
    | ExternalCall (s) -> "ExternalCall(" ^ s ^ ")"
    | IfThenElse -> "IfThenElse"
    | LetIn -> "LetIn"




end
module N = YYNode

module YYAbt = AbtLib.Abt(YYNode)
module A = YYAbt

type scan_expect = InString | InComment | InLibrarySearch of string (* library path*)
let all_scan_env = [InString; InComment]

type expect = Expression | Scanning of scan_expect
let all_expects = [Expression]@(List.map (fun x -> Scanning x) all_scan_env)


type ('a, 'b) map = ('a * 'b) list

type t_environment = 
{
  constants : (Ext.t_str * A.t * A.t) list
}



type proc_error = ErrExpectString of {
  expecting: CS.t_string;
  actual: CS.t_char * Ext.t;
 } | ErrOther of string | ErrWithExt of string * Ext.t

(* processing state *)
type proc_state = {
  input_future : CharStream.t;
  input_expect : expect;
  expect_state_stack : expect list;
  input_acc : A.t list;
  store : t_environment;
  registry : processor_registry;
  last_succeeded_processor : processor_entry; (* for debugging on parsing *)
  failures : (proc_error list * proc_state) list;
  top_failure_handler : failure_handler_arg_type -> unit; (* this is the top-level failure handler for cutting off 
  backtracking. Useful a combinator to commit (e.g. if subsequent things fail, instead of 
    backtracking to the failure continuation that I am given, call this to return top level) *)
}
and failure_handler_arg_type = (proc_error * proc_state)
and processor = ProcComplex of unit proc_state_m
              | ProcBinOp of binary_op 
              | ProcIdentifier of CharStream.t_string (* these two only run in the Expression environment*)
and processor_entry = {
  (* uid: int; unique id for this entry, may record this number on input_acc and may remove it later *)
  expect : expect;
  name : string;
  processor : processor;
}
and processor_registry = processor_entry list
and 'a proc_state_m = proc_state 
                      -> (failure_handler_arg_type -> unit) (* failure continuation*) 
                      -> (('a * proc_state) -> (failure_handler_arg_type -> unit) -> unit) (* success continuation *) 
                      -> unit
and binary_op = {
  meta: binary_op_meta;
  reduction : unit proc_state_m; (* reduction will be invoked if 
    later operator is of fixity Infix or Postfix whose precedence is lower
    *)
    (* you are expected to modify the input elems stack *)
}
(* for mixfix operators, _y_g_h_k_, you have all the generated precedence of yghk set to zero 
(does not reduce for any operator, then the reduction is encoded when passing along k)
*)

let compilation_manager_get_file_hook  : (string (* filepath *) ->  A.t option) ref =  ref (fun _ -> failwith "compilation_manager_get_file_hook not set")

let pretty_print_elem (x : A.t) : string = 
  match A.view x with
  | A.N(N.ModuleDef, args) -> (
    match List.rev args with
    | ([], x)::([], y)::args ->
        "ModuleDef( " 
        ^ string_of_int (List.length args) ^ " elements + "
        ^ A.show_view x ^ ", " ^
        A.show_view y ^ ", " ^ ")"
    | _ -> A.show_view x
  )
  | _ -> A.show_view x

let show_input_acc (acc : A.t list) : string =
  "[" ^ String.concat ";\n " (List.map pretty_print_elem acc) ^ "]"
let show_input_expect (e : expect) : string =
  match e with
  | Expression -> "Expression"
  | Scanning (InString) -> "Scanning InString"
  | Scanning (InComment) -> "Scanning InComment"
  | Scanning (InLibrarySearch (s)) -> "Scanning InLibrarySearch(" ^ s ^ ")"




let show_processor (p : processor) : string =
  match p with
  | ProcComplex _ -> 
    "ProcComplex " 
  | ProcBinOp {meta; reduction=_} ->
    "ProcBinOp: " ^ show_binary_op_meta meta
  | ProcIdentifier id -> 
    "ProcIdentifier: " ^ CS.get_t_string id
let show_processor_entry (p : processor_entry) : string =
  match p with
  | {expect; name; processor} -> 
    "ProcEntry: " ^ name ^ ", expect: " ^ show_input_expect expect ^
    ", processor: " ^ show_processor processor
let show_proc_state (s : proc_state) : string =
  "ProcState: " ^
  "\ninput_future: " ^ CharStream.print_cs s.input_future ^ ", " ^
  "\ninput_future: " ^ CharStream.show_cs s.input_future ^ ", " ^
  "\ninput_expect: " ^ show_input_expect s.input_expect ^ ", " ^
  "\ninput_acc: " ^ show_input_acc s.input_acc ^ ", "  
  (* ^ "\nregistry: " ^ String.concat "\n, " (List.map show_processor_entry s.registry) ^ ", "   *)
  ^ "\n registry: " ^ (string_of_int (List.length s.registry)) ^ " entries, " ^
  "\nfailures: " ^ string_of_int (List.length s.failures) ^ " entries, " ^
  "\nlast_succeeded_processor: " ^ show_processor_entry s.last_succeeded_processor ^ ", "  
  (* "\nlast_succeeded_processor: " ^ show_processor_entry s.last_succeeded_processor ^ ", "  *)