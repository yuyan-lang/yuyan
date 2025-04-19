

(* A state is a charstream (for pending output )
a list of currently processed stack elements (and environment at that time (for backtracking?) )
and a current environment generator

[Actually it is a list of them, or a decision tree of past states]
*)

module Ext = AbtLib.Extent
module CS = CharStream

type fixity = Prefix 
            | Infix 
            | Postfix 
            | StartBinding of CharStream.t_string (* end string *)
            | ClosedIdentifier (* this is used for syntax sugar for identifers (* identifier standing for some expression not a free variables *) *)
type binary_op_meta = {
  id : int;
  keyword : CS.t_string;
  left_precedence: int; (* precedence viewed from left , this value should be set to zero for left-fix operators [I don't think we look at this value for lfix operators]*)
  right_precedence: int; (* precedence viewed from right , this value should be set to zero for right-fix operators [I don't think we look at this value for rfix operators as reduction will be called]*)
  fixity : fixity; 
}

let show_fixity (f : fixity) : string =
  match f with
  | Prefix -> "Prefix"
  | Infix -> "Infix"
  | Postfix -> "Postfix"
  | StartBinding (s) -> "StartBinding(" ^ CS.get_t_string s ^ ")"
  | ClosedIdentifier -> "ClosedIdentifier"

let show_binary_op_meta (b : binary_op_meta) : string =
  let {id; keyword; left_precedence; right_precedence; fixity} = b in
  "BinOp(" ^ CS.get_t_string keyword ^ ", id=" ^ string_of_int id ^
    ", lp=" ^ string_of_int left_precedence ^
    ", rp= " ^ string_of_int right_precedence ^
    ", f=" ^ show_fixity fixity ^ ")"

module YYNode  = struct
  type builtin = String of string
               | Int of int
               | Bool of bool
               | Unit
               | UnderscorePattern (* 「」  or （） *)
               | Library of string  (* special symbol globally available denoting library root, string denotes a filepath *)

  type parsing_elem = ScannedChar of CS.t_char
                    | Keyword of CS.t_string
                    | BoundScannedString of CS.t_string (* this is used for operator that binds a name *)
                    | OpKeyword of binary_op_meta (* uid of the binary op *)

  type declaration = ConstantDefn 

  type t = Builtin of builtin
         | ParsingElem of parsing_elem
         | Declaration of declaration
         | StructureDeref of string (* label *)
         | ModuleDef 

  let arity (t : t) : int list option = 
    match t with
    | Builtin (_) -> Some([])
    | ParsingElem (_) -> Some([])
    | Declaration ConstantDefn  -> Some([0; 0])
    | StructureDeref (_) -> Some([0])
    | ModuleDef -> None


  let show_builtin (b : builtin) : string = 
    match b with
    | String (s) -> "\"" ^ s ^ "\""
    | Int (i) -> string_of_int i
    | Bool (b) -> string_of_bool b
    | Unit -> "unit"
    | UnderscorePattern -> "_"
    | Library s -> "Library(" ^ s ^ ")"

  let show_parsing_elem (p : parsing_elem) : string =
    match p with
    | ScannedChar (s) -> "UnknownChar(" ^ CS.get_t_char s ^ ")"
    | Keyword (s) -> "Keyword(" ^ CS.get_t_string s ^ ")"
    | OpKeyword (i) -> "OpKeyword(" ^ show_binary_op_meta i ^ ")"
    | BoundScannedString (s) -> "BoundScannedString(" ^ CS.get_t_string s ^ ")"

  let show_declaration (d : declaration) : string =
    match d with
    | ConstantDefn -> "ConstantDefn"

  let show (t : t) : string =
    match t with
    | Builtin (b) -> "Builtin(" ^ show_builtin b ^ ")"
    | ParsingElem (p) -> "ParsingElem(" ^ show_parsing_elem p ^ ")"
    | Declaration (d) -> "Declaration(" ^ show_declaration d ^ ")"
    | StructureDeref (s) -> "StructureDeref(" ^ s ^ ")"
    | ModuleDef -> "ModuleDef"

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
  actual: CS.t_char;
 } | ErrOther of string

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
}
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
                      -> (proc_error * proc_state -> unit) (* failure continuation*) 
                      -> (('a * proc_state) -> (proc_error * proc_state -> unit) -> unit) (* success continuation *) 
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

let show_input_acc (acc : A.t list) : string =
  "[" ^ String.concat "; " (List.map A.show_view acc) ^ "]"
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
  "\ninput_future: " ^ CharStream.show_cs s.input_future ^ ", " ^
  "\ninput_expect: " ^ show_input_expect s.input_expect ^ ", " ^
  "\ninput_acc: " ^ show_input_acc s.input_acc ^ ", " ^
  "\nlast_succeeded_processor: " ^ show_processor_entry s.last_succeeded_processor ^ ", " 