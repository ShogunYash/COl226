let () =
  let _ = run (App(Abs("x", Var "x"), N 5)) in          (* => 5 *)
  let _ = run (App(App(Abs("x", Abs("y", Var "x")), B true), B false)) in   (* => true *)
  let _ = run (IfThenElse(GreaterT(N 3,N 2), N 10, N 20)) in  (* => 10 *)
  let _ = run (Add(N 2, Mult(N 3, N 4))) in                 (* => 14 *)
  let _ = run (App(Abs("x", Add(Var "x", N 1)), N 41)) in  (* => 42 *)
  
  (* Define factorial *)
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
  
  display_result "Factorial 6" (App(factorial, N 6));  => 720