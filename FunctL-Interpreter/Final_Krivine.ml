(* Define our abstract syntax for expressions.
   We include variables, lambda-abstractions, applications,
   integer and boolean constants, an if-then-else, and a plus operation. *)
(* Extended abstract syntax for expressions *)
type expr =
  | Var of string
  | N of int
  | B of bool
  | Abs of string * expr          (* lambda abstraction *)
  | App of expr * expr            (* application *)
  | Sub of expr * expr
  | Div of expr * expr
  | Rem of expr * expr
  | Add of expr * expr
  | Mult of expr * expr
  | GreaterT of expr * expr
  | GreaterTE of expr * expr
  | LessT of expr * expr
  | LessTE of expr * expr
  | Equals of expr * expr
  | Not of expr
  | Absolute of expr
  | Negative of expr
  | And of expr * expr
  | Or of expr * expr
  | IfThenElse of expr * expr * expr

 
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
    | Var y -> if y = x then v else Var y
    | Abs(y, e1) -> if y = x then Abs(y, e1) else Abs(y, subst e1 x v)
    | App(e1, e2) -> App(subst e1 x v, subst e2 x v)
    | N n -> N n
    | B b -> B b
    | Sub(e1, e2) -> Sub(subst e1 x v, subst e2 x v)
    | Div(e1, e2) -> Div(subst e1 x v, subst e2 x v)
    | Rem(e1, e2) -> Rem(subst e1 x v, subst e2 x v)
    | Add(e1, e2) -> Add(subst e1 x v, subst e2 x v)
    | Mult(e1, e2) -> Mult(subst e1 x v, subst e2 x v)
    | GreaterT(e1,e2) -> GreaterT(subst e1 x v, subst e2 x v)
    | GreaterTE(e1,e2) -> GreaterTE(subst e1 x v, subst e2 x v)
    | LessT(e1,e2) -> LessT(subst e1 x v, subst e2 x v)
    | LessTE(e1,e2) -> LessTE(subst e1 x v, subst e2 x v)
    | Equals(e1,e2) -> Equals(subst e1 x v, subst e2 x v)
    | Not e1 -> Not(subst e1 x v)
    | Absolute e1 -> Absolute(subst e1 x v)
    | Negative e1 -> Negative(subst e1 x v)
    | And(e1,e2) -> And(subst e1 x v, subst e2 x v)
    | Or(e1,e2) -> Or(subst e1 x v, subst e2 x v)
    | IfThenElse(e1,e2,e3) -> IfThenElse(subst e1 x v, subst e2 x v, subst e3 x v)

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
    (5) The If and Add nodes are handled by forcing evaluation (with an empty stack)
        of the condition and operands, respectively. *)
let rec krivine (cl : closure) (stack : closure list) : closure =
  match cl with
  | Closure(Var x, env) ->
    (try
      let cl' = List.assoc x env in
      krivine cl' stack
    with Not_found ->
      failwith ("Unbound variable: " ^ x))
  | Closure(App(e1, e2), env) ->
      krivine (Closure(e1, env)) (Closure(e2, env) :: stack)
  | Closure(Abs(x, e), env) ->
      (match stack with
        | arg_cl :: rest ->
            let new_env = (x, arg_cl) :: env in
            krivine (Closure(e, new_env)) rest
        | [] -> cl)
  | Closure(N n, _) ->
      (match stack with [] -> cl | _ -> failwith "Attempt to apply an integer as a function")
  | Closure(B b, _) ->
      (match stack with [] -> cl | _ -> failwith "Attempt to apply a boolean as a function")
  | Closure(Sub(e1, e2), env) ->
      let cl1 = krivine (Closure(e1, env)) [] in
      let cl2 = krivine (Closure(e2, env)) [] in
      (match (cl1, cl2) with
        | (Closure(N n1, _), Closure(N n2, _)) ->
            krivine (Closure(N (n1 - n2), env)) stack
        | _ -> failwith "Subtraction applied to non-integer values")
  | Closure(Div(e1, e2), env) ->
      let cl1 = krivine (Closure(e1, env)) [] in
      let cl2 = krivine (Closure(e2, env)) [] in
      (match (cl1, cl2) with
        | (Closure(N n1, _), Closure(N n2, _)) when n2 <> 0 ->
            krivine (Closure(N (n1 / n2), env)) stack
        | _ -> failwith "Division applied to non-integers or division by zero")
  | Closure(Rem(e1, e2), env) ->
      let cl1 = krivine (Closure(e1, env)) [] in
      let cl2 = krivine (Closure(e2, env)) [] in
      (match (cl1, cl2) with
        | (Closure(N n1, _), Closure(N n2, _)) when n2 <> 0 ->
            krivine (Closure(N (n1 mod n2), env)) stack
        | _ -> failwith "Remainder applied to non-integers or division by zero")
  | Closure(Add(e1, e2), env) ->
      let cl1 = krivine (Closure(e1, env)) [] in
      let cl2 = krivine (Closure(e2, env)) [] in
      (match (cl1, cl2) with
        | (Closure(N n1, _), Closure(N n2, _)) ->
            krivine (Closure(N (n1 + n2), env)) stack
        | _ -> failwith "Addition applied to non-integer values")
  | Closure(Mult(e1, e2), env) ->
      let cl1 = krivine (Closure(e1, env)) [] in
      let cl2 = krivine (Closure(e2, env)) [] in
      (match (cl1, cl2) with
        | (Closure(N n1, _), Closure(N n2, _)) ->
            krivine (Closure(N (n1 * n2), env)) stack
        | _ -> failwith "Multiplication applied to non-integer values")
  | Closure(GreaterT(e1, e2), env) ->
      let cl1 = krivine (Closure(e1, env)) [] in
      let cl2 = krivine (Closure(e2, env)) [] in
      (match (cl1, cl2) with
        | (Closure(N n1, _), Closure(N n2, _)) ->
            krivine (Closure(B (n1 > n2), env)) stack
        | _ -> failwith "Greater-than applied to non-integer values")
  | Closure(GreaterTE(e1, e2), env) ->
      let cl1 = krivine (Closure(e1, env)) [] in
      let cl2 = krivine (Closure(e2, env)) [] in
      (match (cl1, cl2) with
        | (Closure(N n1, _), Closure(N n2, _)) ->
            krivine (Closure(B (n1 >= n2), env)) stack
        | _ -> failwith "Greater-than-or-equal applied to non-integer values")
  | Closure(LessT(e1, e2), env) ->
      let cl1 = krivine (Closure(e1, env)) [] in
      let cl2 = krivine (Closure(e2, env)) [] in
      (match (cl1, cl2) with
        | (Closure(N n1, _), Closure(N n2, _)) ->
            krivine (Closure(B (n1 < n2), env)) stack
        | _ -> failwith "Less-than applied to non-integer values")
  | Closure(LessTE(e1, e2), env) ->
      let cl1 = krivine (Closure(e1, env)) [] in
      let cl2 = krivine (Closure(e2, env)) [] in
      (match (cl1, cl2) with
        | (Closure(N n1, _), Closure(N n2, _)) ->
            krivine (Closure(B (n1 <= n2), env)) stack
        | _ -> failwith "Less-than-or-equal applied to non-integer values")
  | Closure(Equals(e1, e2), env) ->
      let cl1 = krivine (Closure(e1, env)) [] in
      let cl2 = krivine (Closure(e2, env)) [] in
      (match (cl1, cl2) with
        | (Closure(N n1, _), Closure(N n2, _)) ->
            krivine (Closure(B (n1 = n2), env)) stack
        | (Closure(B b1, _), Closure(B b2, _)) ->
            krivine (Closure(B (b1 = b2), env)) stack
        | _ -> failwith "Equality applied to incomparable values")
  | Closure(Not e, env) ->
      let cl1 = krivine (Closure(e, env)) [] in
      (match cl1 with
        | Closure(B b, _) -> krivine (Closure(B (not b), env)) stack
        | _ -> failwith "Not applied to a non-boolean value")
  | Closure(Absolute e, env) ->
      let cl1 = krivine (Closure(e, env)) [] in
      (match cl1 with
        | Closure(N n, _) -> krivine (Closure(N (abs n), env)) stack
        | _ -> failwith "Absolute applied to a non-integer value")
  | Closure(Negative e, env) ->
      let cl1 = krivine (Closure(e, env)) [] in
      (match cl1 with
        | Closure(N n, _) -> krivine (Closure(N (-n), env)) stack
        | _ -> failwith "Negative applied to a non-integer value")
  | Closure(And(e1, e2), env) ->
      let cl1 = krivine (Closure(e1, env)) [] in
      let cl2 = krivine (Closure(e2, env)) [] in
      (match (cl1, cl2) with
        | (Closure(B b1, _), Closure(B b2, _)) ->
            krivine (Closure(B (b1 && b2), env)) stack
        | _ -> failwith "And applied to non-boolean values")
  | Closure(Or(e1, e2), env) ->
      let cl1 = krivine (Closure(e1, env)) [] in
      let cl2 = krivine (Closure(e2, env)) [] in
      (match (cl1, cl2) with
        | (Closure(B b1, _), Closure(B b2, _)) ->
            krivine (Closure(B (b1 || b2), env)) stack
        | _ -> failwith "Or applied to non-boolean values")
  | Closure(IfThenElse(e1, e2, e3), env) ->
      let cond_cl = krivine (Closure(e1, env)) [] in
      (match cond_cl with
        | Closure(B true, _) -> krivine (Closure(e2, env)) stack
        | Closure(B false, _) -> krivine (Closure(e3, env)) stack
        | _ -> failwith "IfThenElse condition did not evaluate to a boolean")
 
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
   | N n -> string_of_int n
   | B b -> string_of_bool b
   | Sub(e1, e2) -> "(" ^ (expr_to_string e1) ^ " - " ^ (expr_to_string e2) ^ ")"
   | Div(e1, e2) -> "(" ^ (expr_to_string e1) ^ " / " ^ (expr_to_string e2) ^ ")"
   | Rem(e1, e2) -> "(" ^ (expr_to_string e1) ^ " % " ^ (expr_to_string e2) ^ ")"
   | Add(e1, e2) -> "(" ^ (expr_to_string e1) ^ " + " ^ (expr_to_string e2) ^ ")"
   | Mult(e1, e2) -> "(" ^ (expr_to_string e1) ^ " * " ^ (expr_to_string e2) ^ ")"
   | GreaterT(e1, e2) -> "(" ^ (expr_to_string e1) ^ " > " ^ (expr_to_string e2) ^ ")"
   | GreaterTE(e1, e2) -> "(" ^ (expr_to_string e1) ^ " >= " ^ (expr_to_string e2) ^ ")"
   | LessT(e1, e2) -> "(" ^ (expr_to_string e1) ^ " < " ^ (expr_to_string e2) ^ ")"
   | LessTE(e1, e2) -> "(" ^ (expr_to_string e1) ^ " <= " ^ (expr_to_string e2) ^ ")"
   | Equals(e1, e2) -> "(" ^ (expr_to_string e1) ^ " = " ^ (expr_to_string e2) ^ ")"
   | Not e1 -> "not " ^ (expr_to_string e1)
   | Absolute e1 -> "abs " ^ (expr_to_string e1)
   | Negative e1 -> "-" ^ (expr_to_string e1)
   | And(e1, e2) -> "(" ^ (expr_to_string e1) ^ " && " ^ (expr_to_string e2) ^ ")"
   | Or(e1, e2) -> "(" ^ (expr_to_string e1) ^ " || " ^ (expr_to_string e2) ^ ")"
   | IfThenElse(e1, e2, e3) ->
      "if " ^ (expr_to_string e1) ^ " then " ^ (expr_to_string e2) ^
      " else " ^ (expr_to_string e3)

(* A function to print the result of evaluation. *)
let run (e : expr) =
  let result_cl = eval e in
  let result_expr = unload result_cl in
  match result_expr with
  | N n -> Printf.printf "Result: %d\n" n
  | B b -> Printf.printf "Result: %b\n" b
  | _ -> Printf.printf "Result: %s\n" (expr_to_string result_expr)

(* Helper function to display results with more detail *)
let display_result name expr =
  let result_cl = eval expr in
  let result_expr = unload result_cl in
  Printf.printf "\n=== Test: %s ===\n" name;
  Printf.printf "Result: %s\n" (expr_to_string result_expr)

(* Test 1: Identity function - λx.x *)
let identity = Abs("x", Var "x")
let () = display_result "Identity function" identity
let () = display_result "Identity function applied to 42" (App(identity, N 42))

(* Test 2: Constant function - λx.λy.x *)
let const = Abs("x", Abs("y", Var "x"))
let () = display_result "Constant function" const
let () = display_result "Constant function applied to true" (App(const, B true))
let () = display_result "Constant function applied to true and then false" 
  (App(App(const, B true), B false))

(* Test 3: Church encoding for pairs *)
let pair = Abs("x", Abs("y", Abs("f", App(App(Var "f", Var "x"), Var "y"))))
let first = Abs("p", App(Var "p", Abs("x", Abs("y", Var "x"))))
let second = Abs("p", App(Var "p", Abs("x", Abs("y", Var "y"))))

let () = display_result "Church pair constructor" pair
let () = display_result "First selector" first
let () = display_result "Second selector" second

(* Test 4: Pair usage - create a pair (3,5) and extract components *)
let pair_3_5 = App(App(pair, N 3), N 5)
let () = display_result "Pair (3,5)" pair_3_5
let () = display_result "First of (3,5)" (App(first, pair_3_5))
let () = display_result "Second of (3,5)" (App(second, pair_3_5))

(* Test 5: Church booleans *)
let church_true = Abs("x", Abs("y", Var "x"))  (* Same as const *)
let church_false = Abs("x", Abs("y", Var "y"))
let church_and = Abs("p", Abs("q", App(App(Var "p", Var "q"), Var "p")))
let church_or = Abs("p", Abs("q", App(App(Var "p", Var "p"), Var "q")))
let church_not = Abs("p", Abs("x", Abs("y", App(App(Var "p", Var "y"), Var "x"))))

let () = display_result "Church encoding of true" church_true
let () = display_result "Church encoding of false" church_false

(* Test 6: Church boolean operations *)
let test_and_tt = App(App(church_and, church_true), church_true)
let test_and_tf = App(App(church_and, church_true), church_false)
let test_or_tf = App(App(church_or, church_true), church_false)
let test_not_t = App(church_not, church_true)

(* Apply to arguments to see the boolean result *)
let () = display_result "true AND true" (App(App(test_and_tt, B true), B false))  (* Should select first arg *)
let () = display_result "true AND false" (App(App(test_and_tf, B true), B false))  (* Should select second arg *)
let () = display_result "true OR false" (App(App(test_or_tf, B true), B false))   (* Should select first arg *)
let () = display_result "NOT true" (App(App(test_not_t, B false), B true))        (* Should select second arg *)

(* Test 7: Church numerals *)
let church_zero = Abs("f", Abs("x", Var "x"))
let church_one = Abs("f", Abs("x", App(Var "f", Var "x")))
let church_two = Abs("f", Abs("x", App(Var "f", App(Var "f", Var "x"))))
let church_succ = Abs("n", Abs("f", Abs("x", App(Var "f", App(App(Var "n", Var "f"), Var "x")))))
let church_add = Abs("m", Abs("n", Abs("f", Abs("x", App(App(Var "m", Var "f"), App(App(Var "n", Var "f"), Var "x"))))))

let () = display_result "Church numeral 2" church_two
let () = display_result "Successor of Church numeral 1" (App(church_succ, church_one))
let () = display_result "1 + 2 using Church numerals" (App(App(church_add, church_one), church_two))

(* Test 8: Converting Church numeral to integer *)
let church_to_int = Abs("n", App(App(Var "n", Abs("x", Add(Var "x", N 1))), N 0))
let () = display_result "Church numeral 2 as integer" (App(church_to_int, church_two))
let () = display_result "1 + 2 using Church numerals as integer" 
  (App(church_to_int, App(App(church_add, church_one), church_two)))

(* Test 9: Factorial using Y-combinator *)
let fact_fun = 
  Abs("f", Abs("n", 
    IfThenElse(
      Equals(Var "n", N 0), 
      N 1, 
      Mult(Var "n", App(Var "f", Sub(Var "n", N 1)))
    )
  ))

let ycomb =
    Abs("f",
      App(
        Abs("x", App(Var "f", Abs("v", App(App(Var "x", Var "x"), Var "v")))),
        Abs("x", App(Var "f", Abs("v", App(App(Var "x", Var "x"), Var "v"))))
      )
    )

let y_fact = App(ycomb, fact_fun)
let () = display_result "Factorial of 5 (using Y-combinator)" (App(y_fact, N 5))
let () = display_result "Factorial of 6 (using Y-combinator)" (App(y_fact, N 6))

let fib_fun =
  Abs("f", Abs("n",
    IfThenElse(
      Equals(Var "n", N 0), N 0,
      IfThenElse(
        Equals(Var "n", N 1), N 1,
        Add(
          App(Var "f", Sub(Var "n", N 1)),
          App(Var "f", Sub(Var "n", N 2))
        )
      )
    )
  ))
let fib = App(ycomb, fib_fun)
(* Test 10: Fibonacci using Y-combinator *)
(* Already defined above, just display result with our new function *)
let () = display_result "Fibonacci of 7 (using Y-combinator)" (App(fib, N 7))
let () = display_result "Fibonacci of 10 (using Y-combinator)" (App(fib, N 10))

(* Test 11: Higher-order function - map function over a pair *)
let map_pair = Abs("f", Abs("p", 
  App(App(pair, 
    App(Var "f", App(first, Var "p"))), 
    App(Var "f", App(second, Var "p"))
  )))
  
let increment = Abs("x", Add(Var "x", N 1))
let pair_2_4 = App(App(pair, N 2), N 4)

let () = display_result "Map increment over pair (2,4)" 
  (App(App(map_pair, increment), pair_2_4))
  
(* Test 12: Composition of functions *)
let compose = Abs("f", Abs("g", Abs("x", App(Var "f", App(Var "g", Var "x")))))
let square = Abs("x", Mult(Var "x", Var "x"))
let add3 = Abs("x", Add(Var "x", N 3))

let square_then_add3 = App(App(compose, add3), square)
let () = display_result "Composition (add3 ∘ square) applied to 4" 
  (App(square_then_add3, N 4))  (* (4²) + 3 = 19 *)

(* Test 13: Power function using Y-combinator *)
let pow_fun = 
  Abs("f", Abs("x", Abs("n", 
    IfThenElse(
      Equals(Var "n", N 0),
      N 1,
      Mult(Var "x", App(App(Var "f", Var "x"), Sub(Var "n", N 1)))
    )
  )))

let y_pow = App(ycomb, pow_fun)
let () = display_result "2^5 (using Y-combinator)" (App(App(y_pow, N 2), N 5))  (* 32 *)
let () = display_result "3^4 (using Y-combinator)" (App(App(y_pow, N 3), N 4))  (* 81 *)

(* Test 14: Church numeral multiplication *)
let church_mult = 
  Abs("m", Abs("n", Abs("f", Abs("x", 
    App(App(Var "m", App(Var "n", Var "f")), Var "x")
  ))))

let church_three = App(church_succ, church_two)
let () = display_result "Church numeral 3" church_three
let () = display_result "2 * 3 using Church numerals as integer" 
  (App(church_to_int, App(App(church_mult, church_two), church_three)))  (* 6 *)

(* Test 15: Church numeral predecessor (complex operation) *)
let church_pred = 
  Abs("n", Abs("f", Abs("x", 
    App(
      App(
        App(Var "n", 
          Abs("g", Abs("h", App(Var "h", App(Var "g", Var "f"))))
        ),
        Abs("u", Var "x")
      ),
      Abs("u", Var "u")
    )
  )))

let () = display_result "Predecessor of Church numeral 3 as integer" 
  (App(church_to_int, App(church_pred, church_three)))  (* 2 *)

(* Test 16: Z fixed-point combinator variant *)
let z_combinator = 
  Abs("f", 
    App(
      Abs("x", App(Var "f", Abs("y", App(App(Var "x", Var "x"), Var "y")))),
      Abs("x", App(Var "f", Abs("y", App(App(Var "x", Var "x"), Var "y"))))
    )
  )

(* Using Z-combinator for factorial *)
let z_fact = App(z_combinator, fact_fun)
let () = display_result "Factorial of 7 (using Z-combinator)" (App(z_fact, N 7))  (* 5040 *)

(* Test 17: Curried addition and partial application *)
let curried_add = Abs("x", Abs("y", Add(Var "x", Var "y")))
let add_10 = App(curried_add, N 10)
let () = display_result "Partial application: add_10 function" add_10
let () = display_result "add_10(5) = 15" (App(add_10, N 5))

(* Test 18: Simulating mutual recursion with Y-combinator *)
(* even-odd functions: even(0)=true, odd(0)=false, even(n)=odd(n-1), odd(n)=even(n-1) *)
let even_odd_fun = 
  Abs("f", 
    App(
      App(
        pair,
        (* even function *)
        Abs("n", 
          IfThenElse(
            Equals(Var "n", N 0),
            B true,
            App(App(second, Var "f"), Sub(Var "n", N 1))
          )
        )
      ),
      (* odd function *)
      Abs("n", 
        IfThenElse(
          Equals(Var "n", N 0),
          B false,
          App(App(first, Var "f"), Sub(Var "n", N 1))
        )
      )
    )
  )

let even_odd = App(ycomb, even_odd_fun)
let is_even = App(first, even_odd)
let is_odd = App(second, even_odd)

let () = display_result "is_even(4)" (App(is_even, N 4))  (* true *)
let () = display_result "is_odd(4)" (App(is_odd, N 4))    (* false *)
let () = display_result "is_even(7)" (App(is_even, N 7))  (* false *)
let () = display_result "is_odd(7)" (App(is_odd, N 7))    (* true *)

(* Test 19: Factorial with accumulator for tail recursion *)
let fact_acc_fun = 
  Abs("f", Abs("n", Abs("acc",
    IfThenElse(
      Equals(Var "n", N 0),
      Var "acc",
      App(App(Var "f", Sub(Var "n", N 1)), Mult(Var "n", Var "acc"))
    )
  )))

let fact_acc = App(ycomb, fact_acc_fun)
let () = display_result "Factorial of 6 with accumulator" (App(App(fact_acc, N 6), N 1))  (* 720 *)

(* Test 20: Church encoding of conditional expressions *)
let church_if = 
  Abs("c", Abs("t", Abs("e", 
    App(App(Var "c", Var "t"), Var "e")
  )))

(* Using Church encoded if to compute max of two numbers *)
let max_church =
  Abs("m", Abs("n",
    IfThenElse(
      GreaterT(Var "m", Var "n"),
      Var "m",
      Var "n"
    )
  ))

let () = display_result "max(8, 5) using Church encoding" (App(App(max_church, N 8), N 5))  (* 8 *)
let () = display_result "max(3, 7) using Church encoding" (App(App(max_church, N 3), N 7))  (* 7 *)