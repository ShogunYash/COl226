open Lambda_interpreter

(* Helper function to print Krivine evaluation results *)
let test_krivine name expr =
  Printf.printf "=== Krivine test: %s ===\n" name;
  Printf.printf "Expression: %s\n" (string_of_expr expr);
  let result = evaluate_krivine expr in
  Printf.printf "Result: %s\n\n" (string_of_expr result)

(* Helper function to print direct evaluation results *)
let test_direct name expr =
  Printf.printf "=== Direct evaluation test: %s ===\n" name;
  Printf.printf "Expression: %s\n" (string_of_expr expr);
  let result = evaluate_direct expr in
  Printf.printf "Result: %s\n\n" (string_of_expr result)

(* Basic arithmetic tests *)
let test_arithmetic () =
  Printf.printf "\n--- ARITHMETIC OPERATIONS ---\n\n";
  
  (* Addition *)
  test_direct "Addition" (Add(Int 3, Int 4));
  
  (* Subtraction *)
  test_direct "Subtraction" (Sub(Int 10, Int 6));
  
  (* Multiplication *)
  test_direct "Multiplication" (Mul(Int 5, Int 7));
  
  (* Division *)
  test_direct "Division" (Div(Int 20, Int 4));
  
  (* Complex arithmetic *)
  test_direct "Complex arithmetic" 
    (Add(Mul(Int 3, Int 4), Div(Int 10, Int 2)))

(* Boolean operation tests *)
let test_boolean () =
  Printf.printf "\n--- BOOLEAN OPERATIONS ---\n\n";
  
  (* Equality *)
  test_direct "Equality (true)" (Equals(Int 5, Int 5));
  test_direct "Equality (false)" (Equals(Int 5, Int 7));
  
  (* Less than *)
  test_direct "Less than (true)" (Lt(Int 3, Int 7));
  test_direct "Less than (false)" (Lt(Int 7, Int 3));
  
  (* Greater than *)
  test_direct "Greater than (true)" (Gt(Int 8, Int 2));
  test_direct "Greater than (false)" (Gt(Int 2, Int 8));
  
  (* Logical AND *)
  test_direct "Logical AND (true)" (And(Bool true, Bool true));
  test_direct "Logical AND (false)" (And(Bool true, Bool false));
  
  (* Logical OR *)
  test_direct "Logical OR (true)" (Or(Bool true, Bool false));
  test_direct "Logical OR (false)" (Or(Bool false, Bool false));

(* Conditional tests *)
let test_conditionals () =
  Printf.printf "\n--- CONDITIONAL EXPRESSIONS ---\n\n";
  
  (* If-then-else with boolean condition *)
  test_direct "If-then-else (true condition)" 
    (If(Bool true, Int 1, Int 2));
  
  test_direct "If-then-else (false condition)" 
    (If(Bool false, Int 1, Int 2));
  
  (* If-then-else with computed condition *)
  test_direct "If-then-else (computed condition)" 
    (If(Lt(Int 3, Int 5), 
        Add(Int 10, Int 20), 
        Sub(Int 10, Int 20)))

(* Lambda calculus with primitives *)
let test_lambda_with_primitives () =
  Printf.printf "\n--- LAMBDA CALCULUS WITH PRIMITIVES ---\n\n" ;
  
  (* Apply function that adds 1 to its argument *)
  let add_one = Lam("x", Add(Var "x", Int 1)); 
  test_direct "Apply function (add one)" 
    (App(add_one, Int 5));
  
  (* Function that computes max of two numbers *)
  let max_func = Lam("x", Lam("y", 
                   If(Gt(Var "x", Var "y"),
                      Var "x",
                      Var "y")));
  test_direct "Function (max of two numbers)" 
    (App(App(max_func, Int 7), Int 3));
  
  (* Factorial function (recursive via Y combinator) *)
  let y_comb = Lam("f", 
                   App(
                     Lam("x", App(Var "f", Lam("y", App(App(Var "x", Var "x"), Var "y")))),
                     Lam("x", App(Var "f", Lam("y", App(App(Var "x", Var "x"), Var "y"))))
                   )) ;

  let fact_func = Lam("f", 
                     Lam("n",
                       If(Equals(Var "n", Int 0),
                          Int 1,
                          Mul(Var "n", App(Var "f", Sub(Var "n", Int 1)))))) ;
  
  (* We only try a small factorial to avoid excessive computation *)
  test_direct "Factorial of 4" 
    (App(App(y_comb, fact_func), Int 4));

(* Main test function *)
let run_tests () =
  test_arithmetic ();
  test_boolean ();
  test_conditionals ();
  test_lambda_with_primitives ()

(* Run all tests *)
let () = run_tests ()
