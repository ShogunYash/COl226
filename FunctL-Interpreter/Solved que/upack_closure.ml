(* Types for our simple language of lambda expressions with integers and booleans *)
type exp =
  | Var of string
  | Abs of string * exp        (* lambda abstraction: λx.e *)
  | App of exp * exp           (* application: (e1 e2) *)
  | Int of int                 (* integer constants *)
  | Bool of bool               (* boolean constants *)
  | If of exp * exp * exp      (* conditional: if e1 then e2 else e3 *)

(* A closure pairs an expression with an environment (a table mapping variables to closures) *)
type closure = Closure of exp * env
and env = (string * closure) list

(* A substitution function: subst e x r substitutes all free occurrences of x in e by r *)
let rec subst (e : exp) (x : string) (r : exp) : exp =
  match e with
  | Var y ->
      if y = x then r else Var y
  | Abs (y, e1) ->
      if y = x then Abs (y, e1)
      else Abs (y, subst e1 x r)
  | App (e1, e2) ->
      App (subst e1 x r, subst e2 x r)
  | Int n -> Int n
  | Bool b -> Bool b
  | If (e1, e2, e3) ->
      If (subst e1 x r, subst e2 x r, subst e3 x r)

(*
  The unpack function takes a closure (an expression packaged with an environment)
  and recursively substitutes for each variable x found in the environment by the
  (unpacked) expression corresponding to that closure.
  
  That is, given a closure: Closure(e, gamma), we process each binding in gamma.
  For each (x, cl), we recursively unpack cl to get an expression ue, and then
  substitute ue for x in our expression e.
*)
let rec unpack (Closure (e, gamma)) : exp =
  let rec aux (e : exp) (env : env) : exp =
    match env with
    | [] -> e
    | (x, cl) :: rest ->
        (* Process the remainder of the environment first *)
        let e' = aux e rest in
        (* Recursively unpack the closure bound to x *)
        let ue = unpack cl in
        (* Substitute the unpacked expression for x in e' *)
        subst e' x ue
  in
  aux e gamma

(* 
  Example:

  Consider the lambda-term without any names:
      Closure (Abs("x", App(Var "x", Var "y")), [("y", Closure (Int 3, []))])
  
  Here, the function body is (x y) and the variable y is free with its binding in the table.
  Unpacking this closure recursively substitutes 3 for y, resulting in the expression:
      Abs("x", App(Var "x", Int 3))
  
  This demonstrates how unpack removes the environment and “closes over” the free variable.
*)
  
(* A helper function to pretty-print expressions for testing purposes *)
let rec exp_to_string (e : exp) : string =
  match e with
  | Var x -> x
  | Abs(x, e1) -> "(\\" ^ x ^ ". " ^ exp_to_string e1 ^ ")"
  | App(e1, e2) -> "(" ^ exp_to_string e1 ^ " " ^ exp_to_string e2 ^ ")"
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | If(e1, e2, e3) ->
      "if " ^ exp_to_string e1 ^ " then " ^ exp_to_string e2 ^ " else " ^ exp_to_string e3

(* Test the unpack function *)
let () =
  (* Build a closure for the expression: λx. (x y) with y bound to 3 in the environment *)
  let cl = Closure (Abs ("x", App (Var "x", Var "y")),
                    [("y", Closure (Int 3, []))]) in
  let unpacked = unpack cl in
  print_endline (exp_to_string unpacked)
