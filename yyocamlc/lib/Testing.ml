(* Type-level Peano numbers *)
type zero = Zero
type 'n succ = Succ

(* Type-level function witness *)
type ('a, 'b) tfunc = ..

(* GADT proof for addition *)
type (_, _, _) add =
  | Add_zero : ('n, zero, 'n) add
  | Add_succ : ('n, 'm, 'p) add -> ('n, 'm succ, 'p succ) add

(* Type-level addition function *)
module type ADD = sig
  type n1
  type n2
  type sum
  val proof : (n1, n2, sum) add
end

(* Function application witness *)
type ('f, 'arg, 'result) apply = ..

(* First-class module package *)
(* type ('a, 'b) fn_box = (module functor (X : 'a) -> 'b) *)

(* Example: Type-level doubling function *)
type double = Double
type (_, _, _) double_apply =
  | Double_zero : (double, zero, zero) double_apply
  | Double_succ : (double, 'n, 'm) double_apply ->
    (double, 'n succ, 'm succ succ) double_apply

(* Vector type with length tracking *)
type ('a, 'n) vec =
  | [] : ('a, zero) vec
  | (::) : 'a * ('a, 'n) vec -> ('a, 'n succ) vec

(* Type-safe zip_with using type-level functions *)
let rec zip_with :
  type a b c n. (a -> b -> c) -> (a, n) vec -> (b, n) vec -> (c, n) vec =
  fun f l1 l2 ->
  match l1, l2 with
  | [], [] -> []
  | x::xs, y::ys -> f x y :: zip_with f xs ys

(* Type-level function application *)
let apply_type_fn :
  type f a r. (f, a, r) apply -> a -> r =
  fun _ x -> x

(* Example usage *)
let v1 = [1; 2; 3]
let v2 = [4; 5; 6]

(* This will type-check *)
let good = zip_with (+) v1 v2

(* This would fail to type-check
let bad = zip_with (+) v1 [7]
*)