

(* A state is a charstream (for pending output )
a list of currently processed stack elements (and environment at that time (for backtracking?) )
and a current environment generator

[Actually it is a list of them, or a decision tree of past states]
*)

module Ext = AbtLib.Extent
module CS = CharStream
module YYNode  = struct
  type builtin = String of string
               | Int of int
               | Bool of bool
               | Unit
               | UnderscorePattern (* 「」  or （） *)

  type parsing_elem = ScannedChar of CS.t_char
                    | Keyword of CS.t_string

  type t = Builtin of builtin
         | ParsingElem of parsing_elem

  let arity (t : t) : int list option = 
    match t with
    | Builtin (_) -> Some([])
    | ParsingElem (_) -> Some([])


  let show_builtin (b : builtin) : string = 
    match b with
    | String (s) -> "\"" ^ s ^ "\""
    | Int (i) -> string_of_int i
    | Bool (b) -> string_of_bool b
    | Unit -> "unit"
    | UnderscorePattern -> "_"

  let show_parsing_elem (p : parsing_elem) : string =
    match p with
    | ScannedChar (s) -> "UnknownChar(" ^ CS.get_t_char s ^ ")"
    | Keyword (s) -> "Keyword(" ^ CS.get_t_string s ^ ")"

  let show (t : t) : string =
    match t with
    | Builtin (b) -> "Builtin(" ^ show_builtin b ^ ")"
    | ParsingElem (p) -> "ParsingElem(" ^ show_parsing_elem p ^ ")"

end
module N = YYNode

module YYAbt = AbtLib.Abt(YYNode)
module A = YYAbt

type scan_expect = InString | InComment 
let all_scan_env = [InString; InComment]

type expect = Expression | Scanning of scan_expect
let all_expects = [Expression]@(List.map (fun x -> Scanning x) all_scan_env)


type ('a, 'b) map = ('a * 'b) list

type t_environment = 
{
  constants : (Ext.t_str * A.t * A.t) list
}



type fixity = Prefix 
            | Infix 
            | Postfix 
            | StartBinding of CharStream.t_string (* end string *)

(* processing state *)
type proc_state = {
  input_future : CharStream.t;
  input_expect : expect;
  expect_state_stack : expect list;
  input_acc : A.t list;
  store : t_environment;
  registry : processor_registry;
  last_succeeded_processor : processor; (* for debugging on parsing *)
}
and processor = ProcComplex of {
  expect : expect; (* the environment of the processor should run *)
  name : string;
  (* future_cond : CharStream.t -> bool; to decide whether it can process this state *)
  process : unit proc_state_m
} | ProcBinOp of binary_op | ProcIdentifier of CharStream.t_string (* these two only run in the Expression environment*)
and processor_registry = processor list
and 'a proc_state_m = proc_state -> ('a * proc_state) option
and binary_op = {
  id : int;
  keyword : CS.t_string;
  left_precedence: int; (* precedence viewed from left *)
  right_precedence: int; (* precedence viewed from right *)
  fixity : fixity; 
  reduction : unit proc_state_m; (* reduction will be invoked if 
    later operator is of fixity Infix or Postfix whose precedence is lower
    *)
    (* you are expected to modify the input elems stack *)
}
(* for mixfix operators, _y_g_h_k_, you have all the generated precedence of yghk set to zero 
(does not reduce for any operator, then the reduction is encoded when passing along k)
*)

let compilation_manager_get_file_hook  : (string (* filepath *) ->  A.t) ref =  ref (fun _ -> failwith "compilation_manager_get_file_hook not set")

let show_input_acc (acc : A.t list) : string =
  "[" ^ String.concat "; " (List.map A.show_view acc) ^ "]"
let show_input_expect (e : expect) : string =
  match e with
  | Expression -> "Expression"
  | Scanning (InString) -> "Scanning InString"
  | Scanning (InComment) -> "Scanning InComment"

let show_fixity (f : fixity) : string =
  match f with
  | Prefix -> "Prefix"
  | Infix -> "Infix"
  | Postfix -> "Postfix"
  | StartBinding (s) -> "StartBinding(" ^ CS.get_t_string s ^ ")"


let show_processor (p : processor) : string =
  match p with
  | ProcComplex {expect; name; process=_} -> 
    "ProcComplex: " ^ name ^ ", expect: " ^ show_input_expect expect
  | ProcBinOp {id; keyword; left_precedence; right_precedence; fixity; reduction=_} ->
    "ProcBinOp: " ^ CS.get_t_string keyword ^ ", id: " ^ string_of_int id ^
    ", left_precedence: " ^ string_of_int left_precedence ^
    ", right_precedence: " ^ string_of_int right_precedence ^
    ", fixity: " ^ show_fixity fixity
  | ProcIdentifier id -> 
    "ProcIdentifier: " ^ CS.get_t_string id
let show_proc_state (s : proc_state) : string =
  "ProcState: " ^
  "\ninput_future: " ^ CharStream.show_cs s.input_future ^ ", " ^
  "\ninput_expect: " ^ show_input_expect s.input_expect ^ ", " ^
  "\ninput_acc: " ^ show_input_acc s.input_acc ^ ", " ^
  "\nlast_succeeded_processor: " ^ show_processor s.last_succeeded_processor ^ ", " 