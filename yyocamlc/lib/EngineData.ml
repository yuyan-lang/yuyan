

(* A state is a charstream (for pending output )
a list of currently processed stack elements (and environment at that time (for backtracking?) )
and a current environment generator

[Actually it is a list of them, or a decision tree of past states]
*)


include EngineYYNode

module N = YYNode

module YYAbt = AbtLib.Abt(YYNode)
module A = YYAbt

(* type scan_expect = InString | InComment | InLibrarySearch of string  *)
(* library path*)
(* let all_scan_env = [InString; InComment] *)

type expect = Expr of A.t | TopLevel
(* | Scanning of scan_expect *)
(* let all_expects = [Expression]@(List.map (fun x -> Scanning x) all_scan_env) *)


type ('a, 'b) map = ('a * 'b) list

type t_environment = 
{
  constants : (Ext.t_str * A.t * A.t) list
}



type proc_error = ErrExpectString of {
  expecting: CS.t_string;
  actual: CS.t_char * Ext.t;
 } | ErrOther of string | ErrWithExt of string * Ext.t

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


type parsing_elem = ScannedChar of CS.t_char
| Keyword of CS.t_string
| BoundScannedString of CS.t_string (* this is used for operator that binds a name *)
| OpKeyword of binary_op_meta (* uid of the binary op *)

type void = |
type monad_ret_tp = void
type input_acc_elem = Expr of A.t | ParsingElem of parsing_elem * Ext.t
(* processing state *)
type proc_state = {
  input_future : CharStream.t;
  input_expect : expect;
  expect_state_stack : expect list;
  input_acc : input_acc_elem list;
  store : t_environment;
  registry : processor_registry;
  last_succeeded_processor : processor_entry; (* for debugging on parsing *)
  failures : (proc_error list * proc_state) list;
  top_failure_handler : failure_handler_arg_type -> monad_ret_tp; (* this is the top-level failure handler for cutting off 
  backtracking. Useful a combinator to commit (e.g. if subsequent things fail, instead of 
    backtracking to the failure continuation that I am given, call this to return top level) *)
}
and failure_handler_arg_type = proc_state
and processor = ProcComplex of unit proc_state_m
              | ProcBinOp of binary_op 
              | ProcIdentifier of CharStream.t_string (* these two only run in the Expression environment*)
and processor_entry = {
  id: int; 
  name : string;
  processor : processor;
}
and processor_registry = processor_entry list
and 'a proc_state_m = proc_state 
                      -> (failure_handler_arg_type -> monad_ret_tp) (* failure continuation*) 
                      -> (('a * proc_state) -> (failure_handler_arg_type -> monad_ret_tp) -> monad_ret_tp) (* success continuation *) 
                      -> monad_ret_tp
and binary_op = {
  meta: binary_op_meta;
  reduction : unit proc_state_m; (* reduction will only be invoked on the last operator 
in an operator chain, and only when we have a parse *)
  shift_action : unit proc_state_m; (* shift action will be invoked after the operator is shifted onto the stack *)
}

let compilation_manager_get_file_hook  : (string (* filepath *) ->  A.t option) ref =  ref (fun _ -> failwith "compilation_manager_get_file_hook not set")

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
  

  let show_parsing_elem (p : parsing_elem) : string =
    match p with
    | ScannedChar (s) -> "UnknownChar(" ^ CS.get_t_char s ^ ")"
    | Keyword (s) -> "Keyword(" ^ CS.get_t_string s ^ ")"
    | OpKeyword (i) -> "OpKeyword(" ^ show_binary_op_meta i ^ ")"
    | BoundScannedString (s) -> "BoundScannedString(" ^ show_string (CS.get_t_string s) ^ ")"

let pretty_print_expr (x : A.t) : string = 
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

let show_input_acc_elem (x : input_acc_elem) : string =
  match x with
    | Expr (e) -> "Expr(" ^ pretty_print_expr e ^ ")"
    | ParsingElem (p, _) -> "ParsingElem(" ^ show_parsing_elem p ^ ")"

let show_input_acc (acc : input_acc_elem list) : string =
  "[" ^ String.concat ";\n " (List.map show_input_acc_elem acc) ^ "]"
let show_input_expect (e : expect) : string =
  match e with
  | Expr (e) -> "Expr(" ^ pretty_print_expr e ^ ")"
  | TopLevel -> "TopLevel"
  (* | Scanning (InString) -> "Scanning InString"
  | Scanning (InComment) -> "Scanning InComment"
  | Scanning (InLibrarySearch (s)) -> "Scanning InLibrarySearch(" ^ s ^ ")" *)




let show_processor (p : processor) : string =
  match p with
  | ProcComplex _ -> 
    "ProcComplex " 
  | ProcBinOp {meta; _} ->
    "ProcBinOp: " ^ show_binary_op_meta meta
  | ProcIdentifier id -> 
    "ProcIdentifier: " ^ CS.get_t_string id
let show_processor_entry (p : processor_entry) : string =
  match p with
  | {id; name; processor} -> 
    "ProcEntry: " ^ name ^ ", id=" ^ string_of_int id ^ ", processor: " ^ show_processor processor
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