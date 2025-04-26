

(* A state is a charstream (for pending output )
a list of currently processed stack elements (and environment at that time (for backtracking?) )
and a current environment generator

[Actually it is a list of them, or a decision tree of past states]
*)


include EngineYYNode

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

type void = |
type monad_ret_tp = void
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
  top_failure_handler : failure_handler_arg_type -> monad_ret_tp; (* this is the top-level failure handler for cutting off 
  backtracking. Useful a combinator to commit (e.g. if subsequent things fail, instead of 
    backtracking to the failure continuation that I am given, call this to return top level) *)
}
and failure_handler_arg_type = proc_state
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
                      -> (failure_handler_arg_type -> monad_ret_tp) (* failure continuation*) 
                      -> (('a * proc_state) -> (failure_handler_arg_type -> monad_ret_tp) -> monad_ret_tp) (* success continuation *) 
                      -> monad_ret_tp
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