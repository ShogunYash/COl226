open Lambda_interpreter

(* Helper function to print evaluation results *)
let test_krivine name (expr: Lambda_interpreter.expr) =
  Printf.printf "=== Krivine test: %s ===\n" name;
  Printf.printf "Expression: %s\n" (Lambda_interpreter.string_of_expr expr);
  let result = evaluate_krivine expr in
  Printf.printf "Result: %s\n\n" (Lambda_interpreter.string_of_expr result)

(* Identity function: λx.x *)
let identity = Lam("x", Var "x")

(* Application of identity to itself: (λx.x)(λx.x) *)
let id_app_id = App(identity, identity)

(* Booleans in lambda calculus *)
let lc_true = Lam("x", Lam("y", Var "x"))            (* λx.λy.x *)
let lc_false = Lam("x", Lam("y", Var "y"))           (* λx.λy.y *)

(* Conditional: λp.λa.λb.p a b *)
let lc_if = Lam("p", Lam("a", Lam("b", App(App(Var "p", Var "a"), Var "b"))))

(* Test if_true_a_b => a *)
let test_if_true = App(App(App(lc_if, lc_true), Var "a"), Var "b")

(* Test if_false_a_b => b *)
let test_if_false = App(App(App(lc_if, lc_false), Var "a"), Var "b")

(* Church numerals *)
let church_0 = Lam("f", Lam("x", Var "x"))                              (* λf.λx.x *)
let church_1 = Lam("f", Lam("x", App(Var "f", Var "x")))                 (* λf.λx.f x *)
let church_2 = Lam("f", Lam("x", App(Var "f", App(Var "f", Var "x"))))   (* λf.λx.f(f x) *)

(* Church successor: λn.λf.λx.f(n f x) *)
let church_succ = Lam("n", 
                      Lam("f", 
                          Lam("x", 
                              App(Var "f", 
                                  App(App(Var "n", Var "f"), Var "x")))))

(* Test succ 1 => 2 *)
let test_succ_1 = App(church_succ, church_1)

(* Main test function *)
let run_tests () =
  (* Krivine machine tests *)
  test_krivine "Identity" identity;
  test_krivine "Identity applied to itself" id_app_id;
  test_krivine "True" lc_true;
  test_krivine "False" lc_false;
  test_krivine "If true a b" test_if_true;
  test_krivine "If false a b" test_if_false;
  test_krivine "Church numeral 0" church_0;
  test_krivine "Church numeral 1" church_1;
  test_krivine "Church numeral 2" church_2;
  test_krivine "Successor of 1" test_succ_1

(* Run all tests *)
let () = run_tests ()