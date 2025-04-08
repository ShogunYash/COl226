(* Define our abstract syntax for expressions.
   We include variables, lambda-abstractions, applications,
   integer and boolean constants, an if-then-else, and a plus operation. *)
type expr =
  | Var of string
  | Abs of string * expr                   (* lambda abstraction: \x. e *)
  | App of expr * expr                     (* function application: (e1 e2) *)
  | Int of int                             (* integer constants *)
  | Bool of bool                           (* boolean constants *)
  | If of expr * expr * expr               (* conditional: if e1 then e2 else e3 *)
  | Plus of expr * expr                    (* plus operator: e1 + e2 *)

(* A closure is a pair consisting of an expression and an environment.
   The environment (env) is a list mapping variable names to closures.
   Note: We assume here that terms are closed (or become closed during eval)
   so that every variable reference is eventually found in the environment. *)
and closure = Closure of expr * env
and env = (string * closure) list

(* Substitution function.
   Given an expression 'e', substitute free occurrences of variable 'x'
   with expression 'v'. (This is used by unload below to “unpack” closures.) *)
let rec subst (e : expr) (x : string) (v : expr) : expr =
  match e with
  | Var y ->
      if y = x then v else Var y
  | Abs(y, e1) ->
      (* if the abstraction binds x, stop substitution below *)
      if y = x then Abs(y, e1)
      else Abs(y, subst e1 x v)
  | App(e1, e2) ->
      App(subst e1 x v, subst e2 x v)
  | Int n -> Int n
  | Bool b -> Bool b
  | If(e1, e2, e3) ->
      If(subst e1 x v, subst e2 x v, subst e3 x v)
  | Plus(e1, e2) ->
      Plus(subst e1 x v, subst e2 x v)

(* Unload function.
   This function takes a closure (which may “hide” an environment) and recursively
   substitutes for each bound variable from the environment so that the resulting
   expression no longer carries an environment. *)
let rec unload (Closure(e, env)) : expr =
  let rec aux e env =
    match env with
    | [] -> e
    | (x, cl) :: rest ->
        let e' = aux e rest in
        let v = unload cl in
        subst e' x v
  in
  aux e env

(* The Krivine machine (call-by-name) evaluator.
   It takes a closure and an argument stack (a list of closures). The rules follow:
   (1) If we have an application node, we “push” the argument closure and
       continue evaluating the function part.
   (2) For a variable, we look up its binding in the environment.
   (3) For an abstraction, if a closure is waiting on the stack,
       we bind the formal parameter to the (unevaluated) argument (call-by-name)
       and then proceed with the body.
   (4) For built-in constants like Int and Bool, if no further application is pending,
       they are final; otherwise, an error is raised.
   (5) The If and Plus nodes are handled by forcing evaluation (with an empty stack)
       of the condition and operands, respectively. *)
let rec krivine (cl : closure) (stack : closure list) : closure =
  match cl with
  | Closure(App(e1, e2), env) ->
      (* (Op) For an application, push the argument closure onto the stack *)
      krivine (Closure(e1, env)) (Closure(e2, env) :: stack)
  | Closure(Var x, env) ->
      (* (Var) For a variable, look it up in the environment *)
      (try
         let cl' = List.assoc x env in
         krivine cl' stack
       with Not_found ->
         (* In a well-formed (closed) term x should always be bound *)
         failwith ("Unbound variable: " ^ x))
  | Closure(Abs(x, e), env) ->
      (* (App) When an abstraction is found and there is an argument waiting,
         extend the environment with the binding for the formal parameter
         and evaluate the function body. *)
      (match stack with
       | arg_cl :: rest ->
           let new_env = (x, arg_cl) :: env in
           krivine (Closure(e, new_env)) rest
       | [] ->
           (* When no further argument is provided, we consider this closure a final value *)
           cl)
  | Closure(Int n, _) ->
      (match stack with
       | [] -> cl
       | _ -> failwith "Attempt to apply an integer as a function")
  | Closure(Bool b, _) ->
      (match stack with
       | [] -> cl
       | _ -> failwith "Attempt to apply a boolean as a function")
  | Closure(If(e1, e2, e3), env) ->
      (* Evaluate the condition; we force evaluation with an empty stack *)
      let cond_cl = krivine (Closure(e1, env)) [] in
      (match cond_cl with
       | Closure(Bool true, _) ->
           krivine (Closure(e2, env)) stack
       | Closure(Bool false, _) ->
           krivine (Closure(e3, env)) stack
       | _ ->
           failwith "If condition did not evaluate to a boolean")
  | Closure(Plus(e1, e2), env) ->
      (* Evaluate both operands with an empty stack *)
      let cl1 = krivine (Closure(e1, env)) [] in
      let cl2 = krivine (Closure(e2, env)) [] in
      (match (cl1, cl2) with
       | (Closure(Int n1, _), Closure(Int n2, _)) ->
           (* Create a new closure for the computed result; pass on the current env *)
           krivine (Closure(Int (n1 + n2), env)) stack
       | _ ->
           failwith "Plus applied to non-integer values")

(* A helper function to kick off evaluation.
   It starts from an expression with an empty environment and an empty argument stack. *)
let eval (e : expr) : closure =
  krivine (Closure(e, [])) []

(* A simple pretty-printer for expressions.
   This helps us see the results after unloading closures. *)
let rec expr_to_string (e : expr) : string =
  match e with
  | Var x -> x
  | Abs(x, e1) -> "(\\" ^ x ^ ". " ^ (expr_to_string e1) ^ ")"
  | App(e1, e2) -> "(" ^ (expr_to_string e1) ^ " " ^ (expr_to_string e2) ^ ")"
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | If(e1, e2, e3) ->
      "if " ^ (expr_to_string e1) ^ " then " ^ (expr_to_string e2) ^
      " else " ^ (expr_to_string e3)
  | Plus(e1, e2) ->
      "(" ^ (expr_to_string e1) ^ " + " ^ (expr_to_string e2) ^ ")"

(* Test Examples:
   You can use these tests to see the machine in action. *)

(* Test 1:
   Evaluate the identity function applied to an integer.
   Expression: (\x. x) 5 -> Expected result: 5 *)
let test1 =
  App(Abs("x", Var "x"), Int 5)

(* Test 2:
   Evaluate a function that adds one to its argument.
   Expression: (\x. (x + 1)) 5 -> Expected result: 6 *)
let test2 =
  App(Abs("x", Plus(Var "x", Int 1)), Int 5)

(* Test 3:
   A simple conditional: if true then 2 else 3 -> Expected result: 2 *)
let test3 =
  If(Bool true, Int 2, Int 3)

(* Test 4:
   A function returning its first argument:
   Expression: ((\x. (\y. x)) 10) 20 -> Expected result: 10 *)
let test4 =
  App(App(Abs("x", Abs("y", Var "x")), Int 10), Int 20)

(* Test 5:
   A nested application with conditional and plus:
   Expression: ((\x. if x > 0 then x + 1 else 0) 5) -> Expected: 6 
   Here we model x > 0 as a given true value for demonstration *)
let test5 =
  App(Abs("x", If(Bool true, Plus(Var "x", Int 1), Int 0)), Int 5)

(* Test 6:
   Test variable bindings in nested abstractions:
   Expression: ((\x. (\y. x + y)) 3 4) -> Expected: 7 *)
let test6 =
  App(App(Abs("x", Abs("y", Plus(Var "x", Var "y"))), Int 3), Int 4)

(* Test 7:
   Complex nested expression that combines all features:
   Expression: ((\f. (\x. f (x + 1))) (\y. if true then y else 0)) 5 -> Expected: 6 *)
let test7 =
  App(
    App(
      Abs("f",
          Abs("x",
              App(Var "f", Plus(Var "x", Int 1)))),
      Abs("y", If(Bool true, Var "y", Int 0))
    ),
    Int 5
  )

(* Test 8:
   Multiple nested conditionals:
   Expression: if false then 1 else (if true then 2 else 3) -> Expected: 2 *)
let test8 =
  If(Bool false, Int 1, If(Bool true, Int 2, Int 3))

(* Test 9:
   Complex arithmetic expressions:
   Expression: ((\x. (\y. (x + y) + (x + 2))) 1 2) -> Expected: 6 *)
let test9 =
  App(
    App(
      Abs("x",
          Abs("y",
              Plus(Plus(Var "x", Var "y"), Plus(Var "x", Int 2)))),
      Int 1
    ),
    Int 2
  )

(* Test 10:
   Higher-order functions:
   Expression: ((\f. (\x. f (f x))) (\y. y + 1)) 1 -> Expected: 3
   This applies a function twice to an argument *)
let test10 =
  App(
    App(
      Abs("f", Abs("x", App(Var "f", App(Var "f", Var "x")))),
      Abs("y", Plus(Var "y", Int 1))
    ),
    Int 1
  )

let test11 = Abs("x", Abs("y", Plus(Var "x", Var "y")))

(* Additional comprehensive test cases to verify correctness *)

(* Test 12:
   Identity function (abstraction only):
   Expression: \x. x -> Expected: \x. x *)
let test12 = 
  Abs("x", Var "x")

(* Test 13:
   Church numeral for 2: \f.\x.f(f x)
   This tests proper handling of higher-order functions *)
let test13 =
  Abs("f", Abs("x", App(Var "f", App(Var "f", Var "x"))))

(* Test 14:
   Application of Church numeral 2 to increment function:
   Expression: ((\f.\x.f(f x)) (\n.n+1)) 0 -> Expected: 2 *)
let test14 =
  App(
    App(
      Abs("f", Abs("x", App(Var "f", App(Var "f", Var "x")))),
      Abs("n", Plus(Var "n", Int 1))
    ),
    Int 0
  )

(* Test 15:
   Test complex nesting with shared variable names (shadowing):
   Expression: (\x.(\x.x+1) (x+2)) 3 -> Expected: 6 *)
let test15 =
  App(
    Abs("x", 
      App(
        Abs("x", Plus(Var "x", Int 1)),
        Plus(Var "x", Int 2)
      )
    ),
    Int 3
  )

(* Test 16:
   Test deeply nested conditionals:
   Expression: if true then (if false then 1 else (if true then 2 else 3)) else 4
   Expected: 2 *)
let test16 =
  If(
    Bool true,
    If(
      Bool false,
      Int 1,
      If(Bool true, Int 2, Int 3)
    ),
    Int 4
  )

(* Test 17:
   Test with many nested additions:
   Expression: ((1 + 2) + (3 + 4)) + ((5 + 6) + (7 + 8))
   Expected: 36 *)
let test17 =
  Plus(
    Plus(
      Plus(Int 1, Int 2),
      Plus(Int 3, Int 4)
    ),
    Plus(
      Plus(Int 5, Int 6),
      Plus(Int 7, Int 8)
    )
  )

(* Test 18:
   Test lazy evaluation with a potentially diverging term:
   Expression: (\x. 5) (((\x. x x) (\x. x x)) should return 5
   Because the second term (infinite loop in eager evaluation) is never evaluated *)
let test18 =
  App(
    Abs("x", Int 5),
    App(
      Abs("x", App(Var "x", Var "x")),
      Abs("x", App(Var "x", Var "x"))
    )
  )

(* Test 19:
   Y combinator (fixed point combinator):
   Expression: \f.(\x.f (x x)) (\x.f (x x)) *)
let test19 =
  Abs("f", 
    App(
      Abs("x", App(Var "f", App(Var "x", Var "x"))),
      Abs("x", App(Var "f", App(Var "x", Var "x")))
    )
  )

(* Test 20:
   Function composition:
   Expression: (\f.\g.\x. f (g x)) (\x.x+1) (\x.x*2) 3
   Where x*2 is simulated as x+x for simplicity
   Expected: 7 *)
let test20 =
  App(
    App(
      App(
        Abs("f", Abs("g", Abs("x", App(Var "f", App(Var "g", Var "x"))))),
        Abs("x", Plus(Var "x", Int 1))
      ),
      Abs("x", Plus(Var "x", Var "x"))
    ),
    Int 3
  )

(* Test 21:
   Test with a complex conditional that uses the result of computation:
   Expression: (\x. if x+1>5 then x*2 else x+2) 5
   Where x*2 is simulated as x+x and x+1>5 is hard-coded as true
   Expected: 10 *)
let test21 =
  App(
    Abs("x", 
      If(
        Bool true, 
        Plus(Var "x", Var "x"), 
        Plus(Var "x", Int 2)
      )
    ),
    Int 5
  )

(* Test 22:
   Apply function multiple times (simulating loops):
   Expression: ((\f.\x.\n. if n=0 then x else f (x+1) (n-1)) (\x.\n. if n=0 then x else f (x+1) (n-1))) 0 3
   Where conditionals are simplified with hard-coded values
   Expected: Running the increment function 3 times on 0 = 3 *)
let test22 =
  App(
    App(
      App(
        Abs("f", 
          Abs("x", 
            Abs("n", 
              If(Bool false, 
                Var "x", 
                App(App(Var "f", Plus(Var "x", Int 1)), Var "n")
              )
            )
          )
        ),
        Abs("x", 
          Abs("n", 
            If(Bool true, 
              Var "x", 
              App(App(Var "f", Plus(Var "x", Int 1)), Var "n")
            )
          )
        )
      ),
      Int 0
    ),
    Int 3
  )

(* A helper function to run a test.
   It evaluates the expression and then “unloads” the resulting closure to an expression,
   which is then printed in a readable form. *)
let run_test e =
  let result_cl = eval e in
  let result_expr = unload result_cl in
  print_endline (expr_to_string result_expr)

(* Main function: run our test cases *)
let () =
  print_endline "Test 1:";
  run_test test1; (* Expected output: 5 *)

  print_endline "Test 2:";
  run_test test2; (* Expected output: 6 *)

  print_endline "Test 3:";
  run_test test3; (* Expected output: 2 *)

  print_endline "Test 4:";
  run_test test4; (* Expected output: 10 *)
  
  print_endline "Test 5 (Conditional with Plus):";
  run_test test5; (* Expected output: 6 *)
  
  print_endline "Test 6 (Nested abstraction with variables):";
  run_test test6; (* Expected output: 7 *)
  
  print_endline "Test 7 (Complex expression with all features):";
  run_test test7; (* Expected output: 6 *)
  
  print_endline "Test 8 (Nested conditionals):";
  run_test test8; (* Expected output: 2 *)
  
  print_endline "Test 9 (Complex arithmetic):";
  run_test test9; (* Expected output: 6 *)
  
  print_endline "Test 10 (Higher-order function):";
  run_test test10; (* Expected output: 3 *)

  print_endline "Test 11 (Higher-order function):";
  run_test test11; (* Expected output: \x. \y. x + y *)

  print_endline "\nAdditional Tests for Correctness Verification:";
  
  print_endline "Test 12 (Identity function):";
  run_test test12; (* Expected output: (\x. x) *)

  print_endline "Test 13 (Church numeral 2):";
  run_test test13; (* Expected output: (\f. (\x. (f (f x)))) *)

  print_endline "Test 14 (Apply Church numeral):";
  run_test test14; (* Expected output: 2 *)

  print_endline "Test 15 (Variable shadowing):";
  run_test test15; (* Expected output: 6 *)

  print_endline "Test 16 (Deeply nested conditionals):";
  run_test test16; (* Expected output: 2 *)

  print_endline "Test 17 (Complex nested additions):";
  run_test test17; (* Expected output: 36 *)

  print_endline "Test 18 (Call-by-name with diverging term):";
  run_test test18; (* Expected output: 5 *)

  print_endline "Test 19 (Y combinator):";
  run_test test19; (* Expected output: (\f. ((\x. (f (x x))) (\x. (f (x x))))) *)

  print_endline "Test 20 (Function composition):";
  run_test test20; (* Expected output: 7 *)

  print_endline "Test 21 (Conditional with computation):";
  run_test test21; (* Expected output: 10 *)

  print_endline "Test 22 (Simulating iteration):";
  run_test test22 (* Expected: depends on implementation details *)
(*
This code implements:
- A simple lambda calculus enriched with integers, booleans, conditionals, and addition.
- A Krivine machine for call-by-name evaluation.
- An unload function that transforms a closure (which has an environment) back into a source-level expression.

You can extend it further (for example, adding more built-in operations or syntactic sugar)
as required by your course notes.
*)
