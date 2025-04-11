(* Sample tests *)
let () =
  run (App(Abs("x", Var "x"), N 5));         (* => 5 *)
  run (App(App(Abs("x", Abs("y", Var "x")), B true), B false));  (* => true *)
  run (IfThenElse(GreaterT(N 3,N 2), N 10, N 20)); (* => 10 *)
  run (Add(N 2, Mult(N 3, N 4)));                (* => 14 *)
  run (App(Abs("x", Add(Var "x", N 1)), N 41)); (* => 42 *)

let factorial =
    App (
      Abs ("f", Abs ("n",
        IfThenElse (
          Equals (Var "n", N 0),
          N 1,
          Mult (
            Var "n",
            App (App (Var "f", Var "f"), Sub (Var "n", N 1))
          )
        )
      )),
      Abs ("f", Abs ("n",
        IfThenElse (
          Equals (Var "n", N 0),
          N 1,
          Mult (
            Var "n",
            App (App (Var "f", Var "f"), Sub (Var "n", N 1))
          )
        )
      ))
    )
in

(* Run SECD *)
let compiled = compile (App(factorial, N 6)) in

let result = secd [] [] compiled [] in

match result with
| NumVal n -> Printf.printf "Factorial 6 = %d\n" n
| _ -> failwith "Unexpected result"
;;
(* Helper function to display results with more detail *)
let display_result name expr =
  let code = compile expr in
  let result = secd [] [] code [] in
  Printf.printf "\n=== Test: %s ===\n" name;
  match result with
  | NumVal n -> Printf.printf "Result: %d (Number)\n" n
  | BoolVal b -> Printf.printf "Result: %b (Boolean)\n" b
  | FuncVal (param, _, _) -> Printf.printf "Result: <function with param '%s'> (Closure)\n" param

(* Test 1: Identity function - λx.x *)
let identity = Abs("x", Var "x");;
display_result "Identity function" identity;;
display_result "Identity function applied to 42" (App(identity, N 42));;

(* Test 2: Constant function - λx.λy.x *)
let const = Abs("x", Abs("y", Var "x"));;
display_result "Constant function" const;;
display_result "Constant function applied to true" (App(const, B true));;
display_result "Constant function applied to true and then false" 
  (App(App(const, B true), B false));;

(* Test 3: Church encoding for pairs *)
let pair = Abs("x", Abs("y", Abs("f", App(App(Var "f", Var "x"), Var "y"))));;
let first = Abs("p", App(Var "p", Abs("x", Abs("y", Var "x"))));;
let second = Abs("p", App(Var "p", Abs("x", Abs("y", Var "y"))));;

display_result "Church pair constructor" pair;;
display_result "First selector" first;;
display_result "Second selector" second;;

(* Test 4: Pair usage - create a pair (3,5) and extract components *)
let pair_3_5 = App(App(pair, N 3), N 5);;
display_result "Pair (3,5)" pair_3_5;;
display_result "First of (3,5)" (App(first, pair_3_5));;
display_result "Second of (3,5)" (App(second, pair_3_5));;

(* Test 5: Church booleans *)
let church_true = Abs("x", Abs("y", Var "x"));;  (* Same as const *)
let church_false = Abs("x", Abs("y", Var "y"));;
let church_and = Abs("p", Abs("q", App(App(Var "p", Var "q"), Var "p")));;
let church_or = Abs("p", Abs("q", App(App(Var "p", Var "p"), Var "q")));;
let church_not = Abs("p", Abs("x", Abs("y", App(App(Var "p", Var "y"), Var "x"))));;

display_result "Church encoding of true" church_true;;
display_result "Church encoding of false" church_false;;

(* Test 6: Church boolean operations *)
let test_and_tt = App(App(church_and, church_true), church_true);;
let test_and_tf = App(App(church_and, church_true), church_false);;
let test_or_tf = App(App(church_or, church_true), church_false);;
let test_not_t = App(church_not, church_true);;

(* Apply to arguments to see the boolean result *)
display_result "true AND true" (App(App(test_and_tt, B true), B true));;  (* Should return 1 *)
display_result "true AND false" (App(App(test_and_tf, B true), B false));;  (* Should return 0 *)
display_result "true OR false" (App(App(test_or_tf, B true), B false));;   (* Should return 1 *)
display_result "NOT true" (App(App(test_not_t, B true), B false));;        (* Should return 0 *)

(* Test 7: Church numerals *)
let church_zero = Abs("f", Abs("x", Var "x"));;
let church_one = Abs("f", Abs("x", App(Var "f", Var "x")));;
let church_two = Abs("f", Abs("x", App(Var "f", App(Var "f", Var "x"))));;
let church_succ = Abs("n", Abs("f", Abs("x", App(Var "f", App(App(Var "n", Var "f"), Var "x")))));;
let church_add = Abs("m", Abs("n", Abs("f", Abs("x", App(App(Var "m", Var "f"), App(App(Var "n", Var "f"), Var "x"))))));;

display_result "Church numeral 2" church_two;;
display_result "Successor of Church numeral 1" (App(church_succ, church_one));;
display_result "1 + 2 using Church numerals" (App(App(church_add, church_one), church_two));;

(* Test 8: Converting Church numeral to integer *)
let church_to_int = Abs("n", App(App(Var "n", Abs("x", Add(Var "x", N 1))), N 0));;
display_result "Church numeral 2 as integer" (App(church_to_int, church_two));;
display_result "1 + 2 using Church numerals as integer" 
  (App(church_to_int, App(App(church_add, church_one), church_two)));;

(* Test 9: Factorial using Y-combinator *)
let fact_fun = 
  Abs("f", Abs("n", 
    IfThenElse(
      Equals(Var "n", N 0), 
      N 1, 
      Mult(Var "n", App(Var "f", Sub(Var "n", N 1)))
    )
  ));;

let ycomb =
    Abs("f",
      App(
        Abs("x", App(Var "f", Abs("v", App(App(Var "x", Var "x"), Var "v")))),
        Abs("x", App(Var "f", Abs("v", App(App(Var "x", Var "x"), Var "v"))))
      )
    );;
let y_fact = App(ycomb, fact_fun);;
display_result "Factorial of 5 (using Y-combinator)" (App(y_fact, N 5));;
display_result "Factorial of 6 (using Y-combinator)" (App(y_fact, N 6));;

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
  ));;
let fib = App(ycomb, fib_fun);;
(* Test 10: Fibonacci using Y-combinator *)
(* Already defined above, just display result with our new function *)
display_result "Fibonacci of 7 (using Y-combinator)" (App(fib, N 7));;
display_result "Fibonacci of 10 (using Y-combinator)" (App(fib, N 10));;

(* Test 11: Higher-order function - map function over a list *)

(* Test 11: Higher-order function - map function over a pair *)
let map_pair = Abs("f", Abs("p", 
  App(App(pair, 
    App(Var "f", App(first, Var "p"))), 
    App(Var "f", App(second, Var "p"))
  )));;
  
let increment = Abs("x", Add(Var "x", N 1));;
let pair_2_4 = App(App(pair, N 2), N 4);;

display_result "Map increment over pair (2,4)" 
  (App(App(map_pair, increment), pair_2_4));;
  
(* Test 12: Composition of functions *)
let compose = Abs("f", Abs("g", Abs("x", App(Var "f", App(Var "g", Var "x")))));;
let square = Abs("x", Mult(Var "x", Var "x"));;
let add3 = Abs("x", Add(Var "x", N 3));;

let square_then_add3 = App(App(compose, add3), square);;
display_result "Composition (add3 ∘ square) applied to 4" 
  (App(square_then_add3, N 4));;  (* (4²) + 3 = 19 *)

(* Test 13: Power function using Y-combinator *)
let pow_fun = 
  Abs("f", Abs("x", Abs("n", 
    IfThenElse(
      Equals(Var "n", N 0),
      N 1,
      Mult(Var "x", App(App(Var "f", Var "x"), Sub(Var "n", N 1)))
    )
  )));;

let y_pow = App(ycomb, pow_fun);;
display_result "2^5 (using Y-combinator)" (App(App(y_pow, N 2), N 5));;  (* 32 *)
display_result "3^4 (using Y-combinator)" (App(App(y_pow, N 3), N 4));;  (* 81 *)

(* Test 14: Church numeral multiplication *)
let church_mult = 
  Abs("m", Abs("n", Abs("f", Abs("x", 
    App(App(Var "m", App(Var "n", Var "f")), Var "x")
  ))));;

let church_three = App(church_succ, church_two);;
display_result "Church numeral 3" church_three;;
display_result "2 * 3 using Church numerals as integer" 
  (App(church_to_int, App(App(church_mult, church_two), church_three)));;  (* 6 *)

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
  )));;

display_result "Predecessor of Church numeral 3 as integer" 
  (App(church_to_int, App(church_pred, church_three)));;  (* 2 *)

(* Test 16: Z fixed-point combinator variant *)
let z_combinator = 
  Abs("f", 
    App(
      Abs("x", App(Var "f", Abs("y", App(App(Var "x", Var "x"), Var "y")))),
      Abs("x", App(Var "f", Abs("y", App(App(Var "x", Var "x"), Var "y"))))
    )
  );;

(* Using Z-combinator for factorial *)
let z_fact = App(z_combinator, fact_fun);;
display_result "Factorial of 7 (using Z-combinator)" (App(z_fact, N 7));;  (* 5040 *)

(* Test 17: Curried addition and partial application *)
let curried_add = Abs("x", Abs("y", Add(Var "x", Var "y")));;
let add_10 = App(curried_add, N 10);;
display_result "Partial application: add_10 function" add_10;;
display_result "add_10(5) = 15" (App(add_10, N 5));;

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
  );;

let even_odd = App(ycomb, even_odd_fun);;
let is_even = App(first, even_odd);;
let is_odd = App(second, even_odd);;

display_result "is_even(4)" (App(is_even, N 4));;  (* true *)
display_result "is_odd(4)" (App(is_odd, N 4));;    (* false *)
display_result "is_even(7)" (App(is_even, N 7));;  (* false *)
display_result "is_odd(7)" (App(is_odd, N 7));;    (* true *)

(* Test 19: Factorial with accumulator for tail recursion *)
let fact_acc_fun = 
  Abs("f", Abs("n", Abs("acc",
    IfThenElse(
      Equals(Var "n", N 0),
      Var "acc",
      App(App(Var "f", Sub(Var "n", N 1)), Mult(Var "n", Var "acc"))
    )
  )));;

let fact_acc = App(ycomb, fact_acc_fun);;
display_result "Factorial of 6 with accumulator" (App(App(fact_acc, N 6), N 1));;  (* 720 *)

(* Test 20: Church encoding of conditional expressions *)
let church_if = 
  Abs("c", Abs("t", Abs("e", 
    App(App(Var "c", Var "t"), Var "e")
  )));;

(* Using Church encoded if to compute max of two numbers *)
let max_church =
  Abs("m", Abs("n",
    IfThenElse(
      GreaterT(Var "m", Var "n"),
      Var "m",
      Var "n"
    )
  ));;

display_result "max(8, 5) using Church encoding" (App(App(max_church, N 8), N 5));;  (* 8 *)
display_result "max(3, 7) using Church encoding" (App(App(max_church, N 3), N 7));;  (* 7 *)