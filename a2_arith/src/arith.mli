(* Data type definitions *)
type t =
    True 
  | False 
  | If of t * t * t
  | Zero 
  | Succ of t
  | Pred of t
  | IsZero of t

(* small-step evaluator *)
val eval : t -> t

(* multi-step evaluator *)
val evalAll : t -> t

(*
    big-step evaluator
    Your job is to complete the code for this function in arith.ml
*)
val evalBig : t -> t