open Format

type ty = 
      Bool
    | Fun of ty * ty

type t =
      True
    | False
    | If of t * t * t
    | Var of string
    | Abs of string * ty * t
    | App of t * t

exception TypeError

let rec tyEquals (ty1, ty2) =
  match (ty1, ty2) with
    (Bool, Bool) -> true
    | (Fun (s1, s2), Fun (s3, s4)) ->
      tyEquals (s1, s3) && tyEquals (s2, s4)
    | _ -> false
      
(*
    Exercise: complete the code for the following check(env, t) function.
    As an example, We have given a partial implementation, please add
    your code at the TODO to make the test samples pass correctly.
*)
let rec check (env, t): ty =
  match t with
    | True -> Bool
    | False -> Bool
    | Var x -> env x
    | If (t1, t2, t3) ->
        (*  Todo 1: Add your code here. *)
        let tipe1 = check (env, t1) in
        let tipe2 = check (env, t2) in
        let tipe3 = check (env, t3) in
          if tyEquals(tipe1, Bool) && tyEquals(tipe2, tipe3)
          then tipe2
          else raise TypeError
    
    | Abs (x, ty, t) ->
      Fun (ty, check ((fun y -> if x = y
                               then ty
                               else env y)
                      , t))
    | App (t1, t2) ->
        let left = check(env, t1) in
        let right = check(env, t2) in
          match left with
            | Fun(ty1, ty2) when tyEquals(right, ty1) -> ty2
            | _ -> raise TypeError
        (*  Todo 2: Add your code here. *)

let typeCheck t = 
  check ((fun x -> raise TypeError), t)

(* unit test *)
let e1 = App (Abs ("x", Bool, Var "x"), True)
let r1 = typeCheck e1

let e2 = App (Abs ("x", Fun (Bool, Bool), Var "x")
            , (Abs ("x", Bool, Var "x")))
let r2 = typeCheck e2

let e3 = App (Abs ("y", Bool, Var "y"), False)
let r3 = typeCheck e3

let e4 = Abs ("y", Bool, If (Var "y", False, True))
let r4 = typeCheck e4
let e5 = True
let r5 = typeCheck e5

let e6 = If (True, False, True)
let r6 = typeCheck e6


let _ = (if r1 != r2
        then printf "Pass\n"
        else printf "Fail\n")

let _ = (if r1 = r3
        then printf "Pass\n"
        else printf "Fail\n")

let _ = (if r2 = r4
        then printf "Pass\n"
        else printf "Fail\n")

let _ = (if r5 = r6
        then printf "Pass\n"
        else printf "Fail\n")

