(* Corrected SECD Machine Implementation in OCaml *)

(* Expressions *)
type expr =
  | Var of string
  | N of int
  | B of bool
  | Lam of string * expr          (* lambda abstraction *)
  | App of expr * expr            (* application *)
  | Sub of expr * expr
  | Div of expr * expr
  | Rem of expr * expr
  | Add of expr * expr
  | Mult of expr * expr
  | Absolute of expr
  | Negative of expr
  | GreaterT of expr * expr
  | GreaterTE of expr * expr
  | LessT of expr * expr
  | LessTE of expr * expr
  | Equals of expr * expr
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | IfThenElse of expr * expr * expr

(* SECD opcodes *)
type op =
  | LOOKUP of string
  | INT of int
  | BOOL of bool
  | MKCLOS of string * op list
  | FCALLOP of op list * op list
  | APP
  | RET
  | IFTEOP of op list * op list
  | PLUSOP | MINUSOP | MULTOP | DIVOP | REMOP
  | GTOP | GEQOP | LTOP | LEQOP | EQUALSOP
  | NOTOP | ABSOLUTEOP | NEGATIVEOP
  | CONJOP | DISJOP

(* Runtime values *)
type value =
  | NumVal of int
  | BoolVal of bool
  | FuncVal of string * op list * table  (* closure: param, body code, env *)

and stack_token = VClose of value * table
and table = (string * stack_token) list

exception StackError of string
exception ValueError of string
exception OpError of string

let rec lookupTable x tbl =
  match tbl with
  | [] -> raise (ValueError "Variable not found")
  | (y, v) :: rest -> if x = y then v else lookupTable x rest

let augment tbl x v =
  let rec aux acc = function
    | [] -> List.rev ((x, v) :: acc)
    | (y, w) :: rest when y = x -> List.rev_append acc ((x, v) :: rest)
    | pair :: rest -> aux (pair :: acc) rest
  in aux [] tbl

(* Compiler *)
let rec compile e =
  match e with
  | Var x -> [LOOKUP x]
  | N n -> [INT n]
  | B b -> [BOOL b]
  | Lam(x,body) -> [MKCLOS(x, compile body @ [RET])]
  | App(f,a) -> [FCALLOP(compile f, compile a)]
  | Sub (a,b) -> compile a @ compile b @ [MINUSOP]
  | Div (a,b) -> compile a @ compile b @ [DIVOP]
  | Rem (a,b) -> compile a @ compile b @ [REMOP]
  | Add (a,b) -> compile a @ compile b @ [PLUSOP]
  | Mult (a,b) -> compile a @ compile b @ [MULTOP]
  | GreaterT (a,b) -> compile a @ compile b @ [GTOP]
  | GreaterTE(a,b) -> compile a @ compile b @ [GEQOP]
  | LessT   (a,b) -> compile a @ compile b @ [LTOP]
  | LessTE  (a,b) -> compile a @ compile b @ [LEQOP]
  | Equals  (a,b) -> compile a @ compile b @ [EQUALSOP]
  | Not e1 -> compile e1 @ [NOTOP]
  | Absolute e1 -> compile e1 @ [ABSOLUTEOP]
  | Negative e1 -> compile e1 @ [NEGATIVEOP]
  | And (a,b) -> compile a @ compile b @ [CONJOP]
  | Or  (a,b) -> compile a @ compile b @ [DISJOP]
  | IfThenElse(c,t,e2) -> compile c @ [IFTEOP(compile t, compile e2)]

(* SECD interpreter *)
let rec secd stack env code dump =
  match code with
  | [] -> (
      match stack with
      | VClose(v,_) :: [] -> v
      | _ -> raise (StackError "Expected single value on stack at end")
    )
  | op :: rest ->
    (match op with
     | INT n -> secd (VClose(NumVal n, env)::stack) env rest dump
     | BOOL b -> secd (VClose(BoolVal b, env)::stack) env rest dump
     | LOOKUP x ->
         let VClose(v, tbl') = lookupTable x env in
         let tbl'' = augment tbl' x (VClose(v,tbl')) in
         let env' = augment env x (VClose(v,tbl')) in
         secd (VClose(v, tbl'')::stack) env' rest dump
     | PLUSOP ->
         (match stack with
          | VClose(NumVal n2,_) :: VClose(NumVal n1,_) :: tl ->
              secd (VClose(NumVal(n1+n2),env)::tl) env rest dump
          | _ -> raise (StackError "PLUS expects two ints"))
     | MINUSOP ->
         (match stack with
          | VClose(NumVal n2,_) :: VClose(NumVal n1,_) :: tl ->
              secd (VClose(NumVal(n1-n2),env)::tl) env rest dump
          | _ -> raise (StackError "MINUS expects two ints"))
     | MULTOP ->
         (match stack with
          | VClose(NumVal n2,_) :: VClose(NumVal n1,_) :: tl ->
              secd (VClose(NumVal(n1*n2),env)::tl) env rest dump
          | _ -> raise (StackError "MULT expects two ints"))
     | DIVOP ->
         (match stack with
          | VClose(NumVal n2,_) :: VClose(NumVal n1,_) :: tl when n2<>0 ->
              secd (VClose(NumVal(n1/n2),env)::tl) env rest dump
          | _ -> raise (StackError "DIV expects two ints and non-zero divisor"))
     | REMOP ->
         (match stack with
          | VClose(NumVal n2,_) :: VClose(NumVal n1,_) :: tl when n2<>0 ->
              secd (VClose(NumVal(n1 mod n2),env)::tl) env rest dump
          | _ -> raise (StackError "REM expects two ints and non-zero divisor"))
     | NOTOP ->
         (match stack with
          | VClose(BoolVal b,_) :: tl -> secd (VClose(BoolVal(not b),env)::tl) env rest dump
          | _ -> raise (StackError "NOT expects a bool"))
     | ABSOLUTEOP ->
         (match stack with
          | VClose(NumVal n,_) :: tl -> secd (VClose(NumVal(abs n),env)::tl) env rest dump
          | _ -> raise (StackError "ABS expects an int"))
     | NEGATIVEOP ->
         (match stack with
          | VClose(NumVal n,_) :: tl -> secd (VClose(NumVal(-n),env)::tl) env rest dump
          | _ -> raise (StackError "NEG expects an int"))
     | CONJOP ->
         (match stack with
          | VClose(BoolVal b2,_) :: VClose(BoolVal b1,_) :: tl ->
              secd (VClose(BoolVal(b1 && b2),env)::tl) env rest dump
          | _ -> raise (StackError "AND expects two bools"))
     | DISJOP ->
         (match stack with
          | VClose(BoolVal b2,_) :: VClose(BoolVal b1,_) :: tl ->
              secd (VClose(BoolVal(b1 || b2),env)::tl) env rest dump
          | _ -> raise (StackError "OR expects two bools"))
     | GTOP ->
         (match stack with
          | VClose(NumVal n2,_) :: VClose(NumVal n1,_) :: tl ->
              secd (VClose(BoolVal(n1>n2),env)::tl) env rest dump
          | _ -> raise (StackError "GT expects two ints"))
     | GEQOP ->
         (match stack with
          | VClose(NumVal n2,_) :: VClose(NumVal n1,_) :: tl ->
              secd (VClose(BoolVal(n1>=n2),env)::tl) env rest dump
          | _ -> raise (StackError "GEQ expects two ints"))
     | LTOP ->
         (match stack with
          | VClose(NumVal n2,_) :: VClose(NumVal n1,_) :: tl ->
              secd (VClose(BoolVal(n1<n2),env)::tl) env rest dump
          | _ -> raise (StackError "LT expects two ints"))
     | LEQOP ->
         (match stack with
          | VClose(NumVal n2,_) :: VClose(NumVal n1,_) :: tl ->
              secd (VClose(BoolVal(n1<=n2),env)::tl) env rest dump
          | _ -> raise (StackError "LEQ expects two ints"))
     | EQUALSOP ->
         (match stack with
          | VClose(NumVal n2,_) :: VClose(NumVal n1,_) :: tl ->
              secd (VClose(BoolVal(n1=n2),env)::tl) env rest dump
          | _ -> raise (StackError "EQUALS expects two ints"))
     | IFTEOP(c1,c2) ->
         (match stack with
          | VClose(BoolVal b,_) :: tl ->
              let next = if b then c1 @ rest else c2 @ rest in
              secd tl env next dump
          | _ -> raise (StackError "IFTE expects a bool"))
     | MKCLOS(x,body) ->
         secd (VClose(FuncVal(x,body,env),env)::stack) env rest dump
     | FCALLOP(cf,ca) ->
         secd stack env (cf @ ca @ [APP] @ rest) dump
     | APP ->
         (match stack with
          | VClose(v,_) :: VClose(FuncVal(x,body,env_clo),_) :: tl ->
              let env' = augment env_clo x (VClose(v,env_clo)) in
              secd [] env' body ((tl,env,rest)::dump)
          | _ -> raise (StackError "APP expects closure and argument"))
     | RET ->
         (match stack, dump with
          | VClose(v,_)::[], (s_old,e_old,c_old)::ds ->
              secd (VClose(v,e_old)::s_old) e_old c_old ds
          | _ -> raise (OpError "OpCode Error")) )

let rec ans_to_string = function
  | NumVal n -> string_of_int n
  | BoolVal b -> string_of_bool b
  | FuncVal(param, code, env) ->
      let rec op_list_to_string ops =
        match ops with
        | [] -> ""
        | [op] -> op_to_string op
        | op::rest -> op_to_string op ^ "; " ^ op_list_to_string rest
      and op_to_string = function
        | LOOKUP x -> "LOOKUP \"" ^ x ^ "\""
        | INT n -> "INT " ^ string_of_int n
        | BOOL b -> "BOOL " ^ string_of_bool b
        | MKCLOS(x, body) -> "\\(" ^ x ^ " -> ...)"
        | FCALLOP(_, _) -> "FCALLOP(...)"
        | APP -> "APP"
        | RET -> "RET"
        | IFTEOP(_, _) -> "IFTEOP(...)"
        | PLUSOP -> "+"
        | MINUSOP -> "-"
        | MULTOP -> "*"
        | DIVOP -> "/"
        | REMOP -> "%"
        | GTOP -> ">"
        | GEQOP -> ">="
        | LTOP -> "<"
        | LEQOP -> "<="
        | EQUALSOP -> "="
        | NOTOP -> "NOT"
        | ABSOLUTEOP -> "ABS"
        | NEGATIVEOP -> "NEG"
        | CONJOP -> "AND"
        | DISJOP -> "OR"
      in
      let env_str = 
        String.concat ", " 
          (List.map (fun (var, _) -> var) env)
      in
      "\\" ^ param ^ "." ^ 
      "[" ^ op_list_to_string code ^ "]" ^ 
      "{env: [" ^ env_str ^ "]}" 

(* Test harness *)
let run e =
  let code = compile e in
  let result = secd [] [] code [] in
  let () = Printf.printf "Result: %s\n" (ans_to_string result) in
  result

(* Helper function to display results with more detail *)
let display_result name expr =
  let code = compile expr in
  let result = secd [] [] code [] in
  Printf.printf "\n=== Test: %s ===\n" name;
  Printf.printf "Result: %s\n" (ans_to_string result)

(* Sample tests *)
(* let () =
  let _ = run (App(Lam("x", Var "x"), N 5)) in          (* => 5 *)
  let _ = run (App(App(Lam("x", Lam("y", Var "x")), B true), B false)) in   (* => true *)
  let _ = run (IfThenElse(GreaterT(N 3,N 2), N 10, N 20)) in  (* => 10 *)
  let _ = run (Add(N 2, Mult(N 3, N 4))) in                 (* => 14 *)
  let _ = run (App(Lam("x", Add(Var "x", N 1)), N 41)) in  (* => 42 *)
  
  (* Define factorial *)
  let factorial =
    App (
      Lam ("f", Lam ("n",
        IfThenElse (
          Equals (Var "n", N 0),
          N 1,
          Mult (
            Var "n",
            App (App (Var "f", Var "f"), Sub (Var "n", N 1))
          )
        )
      )),
      Lam ("f", Lam ("n",
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
  
  display_result "Factorial 6" (App(factorial, N 6));  => 720 *)

  (* Test 0: Basic arithmetic operations *)
  (* Test 1: Identity function - λx.x *)
(* Test 1: Identity function - λx.x *)
let identity = Lam("x", Var "x")
let () = display_result "Identity function" identity
let () = display_result "Identity function applied to 42" (App(identity, N 42))

(* Test 2: Constant function - λx.λy.x *)
let const = Lam("x", Lam("y", Var "x"))
let () = display_result "Constant function" const
let () = display_result "Constant function applied to true" (App(const, B true))
let () = display_result "Constant function applied to true and then false" 
  (App(App(const, B true), B false))

(* Test 3: Church encoding for pairs *)
let pair = Lam("x", Lam("y", Lam("f", App(App(Var "f", Var "x"), Var "y"))))
let first = Lam("p", App(Var "p", Lam("x", Lam("y", Var "x"))))
let second = Lam("p", App(Var "p", Lam("x", Lam("y", Var "y"))))

let () = display_result "Church pair constructor" pair
let () = display_result "First selector" first
let () = display_result "Second selector" second

(* Test 4: Pair usage - create a pair (3,5) and extract components *)
let pair_3_5 = App(App(pair, N 3), N 5)
let () = display_result "Pair (3,5)" pair_3_5
let () = display_result "First of (3,5)" (App(first, pair_3_5))
let () = display_result "Second of (3,5)" (App(second, pair_3_5))

(* Test 5: Church booleans *)
let church_true = Lam("x", Lam("y", Var "x"))  (* Same as const *)
let church_false = Lam("x", Lam("y", Var "y"))
let church_and = Lam("p", Lam("q", App(App(Var "p", Var "q"), Var "p")))
let church_or = Lam("p", Lam("q", App(App(Var "p", Var "p"), Var "q")))
let church_not = Lam("p", Lam("x", Lam("y", App(App(Var "p", Var "y"), Var "x"))))

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
let church_zero = Lam("f", Lam("x", Var "x"))
let church_one = Lam("f", Lam("x", App(Var "f", Var "x")))
let church_two = Lam("f", Lam("x", App(Var "f", App(Var "f", Var "x"))))
let church_succ = Lam("n", Lam("f", Lam("x", App(Var "f", App(App(Var "n", Var "f"), Var "x")))))
let church_add = Lam("m", Lam("n", Lam("f", Lam("x", App(App(Var "m", Var "f"), App(App(Var "n", Var "f"), Var "x"))))))

let () = display_result "Church numeral 2" church_two
let () = display_result "Successor of Church numeral 1" (App(church_succ, church_one))
let () = display_result "1 + 2 using Church numerals" (App(App(church_add, church_one), church_two))

(* Test 8: Converting Church numeral to integer *)
let church_to_int = Lam("n", App(App(Var "n", Lam("x", Add(Var "x", N 1))), N 0))
let () = display_result "Church numeral 2 as integer" (App(church_to_int, church_two))
let () = display_result "1 + 2 using Church numerals as integer" 
  (App(church_to_int, App(App(church_add, church_one), church_two)))

(* Test 9: Factorial using Y-combinator *)
let fact_fun = 
  Lam("f", Lam("n", 
    IfThenElse(
      Equals(Var "n", N 0), 
      N 1, 
      Mult(Var "n", App(Var "f", Sub(Var "n", N 1)))
    )
  ))

let ycomb =
    Lam("f",
      App(
        Lam("x", App(Var "f", Lam("v", App(App(Var "x", Var "x"), Var "v")))),
        Lam("x", App(Var "f", Lam("v", App(App(Var "x", Var "x"), Var "v"))))
      )
    )

let y_fact = App(ycomb, fact_fun)
let () = display_result "Factorial of 5 (using Y-combinator)" (App(y_fact, N 5))
let () = display_result "Factorial of 6 (using Y-combinator)" (App(y_fact, N 6))

let fib_fun =
  Lam("f", Lam("n",
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
let map_pair = Lam("f", Lam("p", 
  App(App(pair, 
    App(Var "f", App(first, Var "p"))), 
    App(Var "f", App(second, Var "p"))
  )))
  
let increment = Lam("x", Add(Var "x", N 1))
let pair_2_4 = App(App(pair, N 2), N 4)

let () = display_result "Map increment over pair (2,4)" 
  (App(App(map_pair, increment), pair_2_4))
  
(* Test 12: Composition of functions *)
let compose = Lam("f", Lam("g", Lam("x", App(Var "f", App(Var "g", Var "x")))))
let square = Lam("x", Mult(Var "x", Var "x"))
let add3 = Lam("x", Add(Var "x", N 3))

let square_then_add3 = App(App(compose, add3), square)
let () = display_result "Composition (add3 ∘ square) applied to 4" 
  (App(square_then_add3, N 4))  (* (4²) + 3 = 19 *)

(* Test 13: Power function using Y-combinator *)
let pow_fun = 
  Lam("f", Lam("x", Lam("n", 
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
  Lam("m", Lam("n", Lam("f", Lam("x", 
    App(App(Var "m", App(Var "n", Var "f")), Var "x")
  ))))

let church_three = App(church_succ, church_two)
let () = display_result "Church numeral 3" church_three
let () = display_result "2 * 3 using Church numerals as integer" 
  (App(church_to_int, App(App(church_mult, church_two), church_three)))  (* 6 *)

(* Test 15: Church numeral predecessor (complex operation) *)
let church_pred = 
  Lam("n", Lam("f", Lam("x", 
    App(
      App(
        App(Var "n", 
          Lam("g", Lam("h", App(Var "h", App(Var "g", Var "f"))))
        ),
        Lam("u", Var "x")
      ),
      Lam("u", Var "u")
    )
  )))

let () = display_result "Predecessor of Church numeral 3 as integer" 
  (App(church_to_int, App(church_pred, church_three)))  (* 2 *)

(* Test 16: Z fixed-point combinator variant *)
let z_combinator = 
  Lam("f", 
    App(
      Lam("x", App(Var "f", Lam("y", App(App(Var "x", Var "x"), Var "y")))),
      Lam("x", App(Var "f", Lam("y", App(App(Var "x", Var "x"), Var "y"))))
    )
  )

(* Using Z-combinator for factorial *)
let z_fact = App(z_combinator, fact_fun)
let () = display_result "Factorial of 7 (using Z-combinator)" (App(z_fact, N 7))  (* 5040 *)

(* Test 17: Curried addition and partial application *)
let curried_add = Lam("x", Lam("y", Add(Var "x", Var "y")))
let add_10 = App(curried_add, N 10)
let () = display_result "Partial application: add_10 function" add_10
let () = display_result "add_10(5) = 15" (App(add_10, N 5))

(* Test 18: Simulating mutual recursion with Y-combinator *)
(* even-odd functions: even(0)=true, odd(0)=false, even(n)=odd(n-1), odd(n)=even(n-1) *)
let even_odd_fun = 
  Lam("f", 
    App(
      App(
        pair,
        (* even function *)
        Lam("n", 
          IfThenElse(
            Equals(Var "n", N 0),
            B true,
            App(App(second, Var "f"), Sub(Var "n", N 1))
          )
        )
      ),
      (* odd function *)
      Lam("n", 
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
  Lam("f", Lam("n", Lam("acc",
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
  Lam("c", Lam("t", Lam("e", 
    App(App(Var "c", Var "t"), Var "e")
  )))

(* Using Church encoded if to compute max of two numbers *)
let max_church =
  Lam("m", Lam("n",
    IfThenElse(
      GreaterT(Var "m", Var "n"),
      Var "m",
      Var "n"
    )
  ))

let () = display_result "max(8, 5) using Church encoding" (App(App(max_church, N 8), N 5))  (* 8 *)
let () = display_result "max(3, 7) using Church encoding" (App(App(max_church, N 3), N 7))  (* 7 *)

(* NEW TEST CASES BELOW *)

(* Test 21: GCD using Y-combinator *)
let gcd_fun = 
  Lam("f", Lam("a", Lam("b",
    IfThenElse(
      Equals(Var "b", N 0),
      Var "a",
      App(App(Var "f", Var "b"), Rem(Var "a", Var "b"))
    )
  ))) 

let gcd = App(ycomb, gcd_fun) 
let () =  display_result "GCD of 48 and 18" (App(App(gcd, N 48), N 18))  (* 6 *)
let () =  display_result "GCD of 35 and 49" (App(App(gcd, N 35), N 49))  (* 7 *)

(* Test 22: Church numeral exponentiation *)
let church_exp = 
  Lam("m", Lam("n", App(Var "n", Var "m"))) 
  
let church_four = App(church_succ, church_three) 
let () = display_result "Church numeral 4" church_four 
let () = display_result "2^3 using Church numerals as integer" 
  (App(church_to_int, App(App(church_exp, church_two), church_three)))  (* 8 *)

(* Test 23: Fixed-point combinator for McCarthy 91 function *)
let mccarthy91_fun = 
  Lam("f", Lam("n", 
    IfThenElse(
      GreaterT(Var "n", N 100),
      Sub(Var "n", N 10),
      App(Var "f", App(Var "f", Add(Var "n", N 11)))
    )
  )) 
  
let m91 = App(ycomb, mccarthy91_fun) 
let () = display_result "McCarthy 91 function applied to 95" (App(m91, N 95))  (* 91 *)
let () = display_result "McCarthy 91 function applied to 105" (App(m91, N 105))  (* 95 *)

(* Test 24: SKKI combinator (identity function) *)
let s_comb = Lam("x", Lam("y", Lam("z", App(App(Var "x", Var "z"), App(Var "y", Var "z"))))) 
let k_comb = Lam("x", Lam("y", Var "x")) 
let i_comb = App(App(s_comb, k_comb), k_comb) 

let () = display_result "I combinator applied to 42" (App(i_comb, N 42))  (* 42 *)

(* Test 25: Church numeral subtraction *)
let church_sub = 
  Lam("m", Lam("n", 
    App(App(Var "n", church_pred), Var "m"))) 
    
let () = display_result "3 - 1 using Church numerals as integer" 
  (App(church_to_int, App(App(church_sub, church_three), church_one))) (* 2 *)

(* Test 26: Boolean operations using only lambda calculus *)
let lambda_xor = 
  Lam("p", Lam("q", 
    App(App(Var "p", App(church_not, Var "q")), Var "q"))) 
    
let test_xor_tf = App(App(lambda_xor, church_true), church_false) 
let test_xor_tt = App(App(lambda_xor, church_true), church_true) 

let () = display_result "true XOR false using Church booleans" 
  (App(App(test_xor_tf, B true), B false)) (* true *)
let () = display_result "true XOR true using Church booleans" 
  (App(App(test_xor_tt, B true), B false)) (* false *)

(* Test 27: Selection sort using Y-combinator - minFunction *)
let min_fun = 
  Lam("f", Lam("a", Lam("b",
    IfThenElse(
      LessT(Var "a", Var "b"),
      Var "a",
      Var "b")
  ))) 
  
let min = App(ycomb, min_fun) 
let () = display_result "min(8, 5)" (App(App(min, N 8), N 5))  (* 5 *)
let () = display_result "min(3, 7)" (App(App(min, N 3), N 7))  (* 3 *)