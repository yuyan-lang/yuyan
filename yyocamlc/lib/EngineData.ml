

(* A state is a charstream (for pending output )
a list of currently processed stack elements (and environment at that time (for backtracking?) )
and a current environment generator

[Actually it is a list of them, or a decision tree of past states]
*)

module Ext = AbtLib.Extent
module YYNode  = struct
  type builtin = String of string
               | Int of int
               | Bool of bool
               | Unit
               | UnderscorePattern (* 「」  or （） *)

  type parsing_elem = ScannedChar of string
                    | Keyword of string * int (* id of the operator responsible for this keyword *)

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
    | ScannedChar (s) -> "UnknownChar(" ^ s ^ ")"
    | Keyword (s, _) -> "Keyword(" ^ s ^ ")"

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
  expect_state_stack : expect list;
  constants : (Ext.t_str * A.t * A.t) list
}

(* processing state *)
type proc_state = {
  input_future : CharStream.t;
  input_expect : expect;
  input_acc : A.t list;
  store : t_environment;
  registry : processor_registry;
  last_succeeded_processor : string; (* for debugging on parsing *)
}
and processor = {
  expect : expect; (*the environment of the processor should*)
  name : string;
  (* future_cond : CharStream.t -> bool; to decide whether it can process this state *)
  process : proc_state -> proc_state option; (* to process this state *)
}
and processor_registry = processor list

type 'a proc_state_m = proc_state -> ('a * proc_state) option


let show_input_acc (acc : A.t list) : string =
  "[" ^ String.concat "; " (List.map A.show_view acc) ^ "]"
let show_input_expect (e : expect) : string =
  match e with
  | Expression -> "Expression"
  | Scanning (InString) -> "Scanning InString"
  | Scanning (InComment) -> "Scanning InComment"
let show_proc_state (s : proc_state) : string =
  "ProcState: " ^
  "\ninput_future: " ^ CharStream.show_cs s.input_future ^ ", " ^
  "\ninput_expect: " ^ show_input_expect s.input_expect ^ ", " ^
  "\ninput_acc: " ^ show_input_acc s.input_acc ^ ", " ^
  "\nlast_succeeded_processor: " ^ s.last_succeeded_processor ^ ", " 