(* A state is a charstream (for pending output )
a list of currently processed stack elements (and environment at that time (for backtracking?) )
and a current environment generator

[Actually it is a list of them, or a decision tree of past states]
*)

include EngineYYNode
module N = YYNode
module YYAbt = AbtLib.Abt (YYNode)
module A = YYAbt

(* type scan_expect = InString | InComment | InLibrarySearch of string  *)
(* library path*)
(* let all_scan_env = [InString; InComment] *)

(* type expect =
  | Expr of A.t
  | TopLevel *)
(* | Scanning of scan_expect *)
(* let all_expects = [Expression]@(List.map (fun x -> Scanning x) all_scan_env) *)

type ('a, 'b) map = ('a * 'b) list

type t_constant =
  | TypeConstructor of
      { name : Ext.t_str
      ; tp : A.t
      ; ocaml_bind_name : string option
        (* optionally bind an ocaml type to this type. 
      If specified, this type constructor is compiled as the underlying ocaml type constructor.*)
      }
  | DataConstructor of
      { name : Ext.t_str
      ; tp : A.t
      ; tp_id : int
      ; ocaml_bind_name : string option
        (* optionally bind an ocaml type to this type. 
      If specified, this data constructor is compiled as the underlying ocaml constructor.*)
      }
  | TypeExpression of A.t
  | DataExpression of
      { name : Ext.t_str option
      ; tp : A.t
      ; tm : A.t option (* is None if it is a recursive definition (forward declaration only)*)
      }
  | PatternVar of
      { name : Ext.t_str
      ; tp : A.t
      }
  | ModuleAlias of
      { name : Ext.t_str
      ; filepath : string
      }

type t_env = (Ext.t_str * int (* int is the uid of the constant, tp *)) list
type t_constants = (int * t_constant) list

type proc_error =
  | ErrExpectString of
      { expecting : CS.t_string
      ; actual : CS.t_char * Ext.t
      }
  | ErrOther of string
  | ErrWithExt of string * Ext.t

type fixity =
  | FxOp of int option
  | FxNone
  | FxBinding of int
  (* uid of the component *)
  | FxComp of int

and binary_op_meta =
  { id : int
  ; keyword : CS.t_string
  ; (* fixity is the behavior of the operator when viewed from left for left_fixity, and right for right_fixity *)
    left_fixity : fixity
  ; right_fixity : fixity
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

type void = |
type monad_ret_tp = void

type tc_history_elem =
  | HistOne of string * A.t
  | HistTwo of string * A.t * string * A.t

type parsing_elem =
  | ScannedChar of CS.t_char
  | Keyword of CS.t_string
  | BoundScannedString of CS.t_string (* this is used for operator that binds a name *)
  | OpKeyword of binary_op_meta * unit proc_state_m (* uid of the binary op *)

and input_acc_elem =
  | Expr of A.t
  | ParsingElem of parsing_elem * Ext.t
(* this will be executed after the 
element is poped via pop_*fix_operator_x where the  *)

(* processing state *)
and proc_state =
  { input_future : CharStream.t
    (* ; input_expect : expect *)
    (* ; expect_state_stack : expect list *)
  ; input_acc : input_acc_elem list
  ; last_input_acc_before_pop : input_acc_elem list option
  ; env : t_env
  ; constants : t_constants
  ; registry : processor_registry
  ; last_succeeded_processor : processor_entry (* for debugging on parsing *)
  ; failures : (proc_error list * proc_state) list
  ; top_failure_handler : failure_handler_arg_type -> monad_ret_tp
    (* this is the top-level failure handler for cutting off 
  backtracking. Useful a combinator to commit (e.g. if subsequent things fail, instead of 
    backtracking to the failure continuation that I am given, call this to return top level) *)
  ; type_checking_history : tc_history_elem list
  ; unification_ctx : (int * A.t option) list
  }

and failure_handler_arg_type = proc_state

and processor =
  | ProcComplex of unit proc_state_m
  | ProcBinOp of binary_op

and processor_entry =
  { id : int
  ; name : string
  ; processor : processor
  }

and processor_registry = processor_entry list

and 'a proc_state_m =
  proc_state
  -> (failure_handler_arg_type -> monad_ret_tp)
  -> (* failure continuation*)
     ('a * proc_state -> (failure_handler_arg_type -> monad_ret_tp) -> monad_ret_tp)
  -> (* success continuation *)
     monad_ret_tp

and shift_action = unit proc_state_m proc_state_m

and binary_op =
  { meta : binary_op_meta
  ; reduction : unit proc_state_m
    (* reduction will only be invoked on the last operator 
in an operator chain, and only when we have a parse *)
  ; shift_action : shift_action
    (* shift action will be invoked before the operator is shifted onto the stack, 
  shift action should return a pop action *)
  }

let compilation_manager_get_file_hook : (string (* filepath *) -> (A.t * t_constants) option) ref =
  ref (fun _ -> failwith "compilation_manager_get_file_hook not set")
;;
