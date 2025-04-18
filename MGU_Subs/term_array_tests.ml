open Term_array

(* Import the Term module *)
open Term

(* Create sample signatures *)
let signature1: signature = Array.of_list [ ("+", 2); ("*", 2); ("1", 0); ("0", 0); ("/", 0) ]
let signature2: signature = Array.of_list [ ("+", 2); ("*", 2); ("1", 0); ("0", 0); ("/", 0) ]
let signature3: signature = Array.of_list [ ("-", 2); ("sin", 1); ("2", 0); ("/", 2); ("cos", 1) ]

(* Create sample terms *)
let tree1 = (* (x+1)+0 *)
  Node (("+", 2), [|
    Node (("+", 2), [| V "x"; Node (("1", 0), [||]) |]);
    Node (("0", 0), [||])
  |])

let tree2 = (* 1 + (0*x) *)
  Node (("+", 2), [|
    Node (("1", 0), [||]);
    Node (("*", 2), [| Node (("0", 0), [||]); V "x" |])
  |])

let tree3 = (* (y/sin(cos(z))) - 2 *)
  Node (("-", 2), [|
    Node (("/", 2), [|
      V "y";
      Node (("sin", 1), [|
        Node (("cos", 1), [| V "z" |])
      |])
    |]);
    Node (("2", 0), [||])
  |])

(* Test cases for MGU *)
(* Case 1: Variables, same variable, identity substitution *)
let t1 = V "x"
let t2 = V "x"

(* Case 2: Variables, different variables, substitution {x -> y} *)
let t3 = V "x"
let t4 = V "y"

(* Case 3: Variable and non-variable, substitution {x -> Node("f", [])} *)
let t5 = V "x"
let t6 = Node (("f", 0), [||])

(* Case 4: Non-variables, same structure, identity substitution *)
let t7 = Node (("g", 2), [| V "a"; V "b" |])
let t8 = Node (("g", 2), [| V "a"; V "b" |])

(* Case 5: Non-variables, different structures, not unifiable *)
let t9 = Node (("h", 2), [| V "x"; V "y" |])
let t10 = Node (("h", 2), [| V "z"; V "w" |])

(* Case 6: Nested variables, not unifiable due to occurs-check *)
let t11 = Node (("i", 1), [| V "x" |])
let t12 = Node (("i", 1), [| Node (("y", 1), [| V "x" |]) |])

(* Case 7: Nested variables, not unifiable due to different number of children*)
let t13 = Node (("i", 1), [| V "x" |])
let t14 = Node (("i", 2), [| V "y"; V "z" |])

(* Case 8: Multiple nested variables, unifiable *)
let t15 = Node (("i", 2), [| V "x"; V "y" |])
let t16 = Node (("i", 2), [| Node (("i", 1), [| V "z" |]); V "w" |])

(* Sample substitutions *)
let subst1 = [("x", V "y"); ("z", V "a")]
let subst2 = [("y", t7)]
let subst3 = [("y", tree1); ("c", tree2)]
let subst4 = [("z", tree3); ("x", t1); ("u", V "v")]
let subst5 = [("p", V "q"); ("r", t11); ("t", V "u")]

(* Invalid signatures and terms for testing *)
let sign1: signature = Array.of_list [("0", 0); ("1", 0); ("0", 1)]
let sign2: signature = Array.of_list [("0", 0); ("1", 0); ("+", 2)]

let t_wrong = Node (("+", 3), [| V "x"; V "y"; V "z" |])
let t_right = Node (("+", 2), [| V "x"; V "y" |])
let t_nested = Node (("+", 2), [| V "z"; t_right |])

(* Test functions *)
let testsignature () =
  test_sig 1 signature1;
  test_sig 2 signature2;
  test_sig 3 signature3

let testwfterm () =
  test_wfterm 1 signature1 tree1;
  test_wfterm 2 signature2 tree2;
  test_wfterm 3 signature3 tree3

let testHtSizeVars () =
  test_ht_size_vars 1 tree1;
  test_ht_size_vars 2 tree2;
  test_ht_size_vars 3 tree3;
  test_ht_size_vars 4 t1;
  test_ht_size_vars 5 t2;
  test_ht_size_vars 6 t3;
  test_ht_size_vars 7 t4;
  test_ht_size_vars 8 t5;
  test_ht_size_vars 9 t6;
  test_ht_size_vars 10 t7;
  test_ht_size_vars 11 t8;
  test_ht_size_vars 12 t9;
  test_ht_size_vars 13 t10;
  test_ht_size_vars 14 t11;
  test_ht_size_vars 15 t12;
  test_ht_size_vars 16 t13;
  test_ht_size_vars 17 t14;
  test_ht_size_vars 18 t15;
  test_ht_size_vars 19 t16

let testmgu () =
  test_mgu 1 t1 t2;
  test_mgu 2 t3 t4;
  test_mgu 3 t5 t6;
  test_mgu 4 t7 t8;
  test_mgu 5 t9 t10;
  test_mgu 6 t11 t12;
  test_mgu 7 t13 t14;
  test_mgu 8 t15 t16

let testCompoSubst () =
  test_compo 1 subst1 subst2;
  test_compo 2 subst1 subst3;
  test_compo 3 subst1 subst4;
  test_compo 4 subst1 subst5;
  test_compo 5 subst2 subst3;
  test_compo 6 subst2 subst4;
  test_compo 7 subst2 subst5;
  test_compo 8 subst3 subst4;
  test_compo 9 subst3 subst5;
  test_compo 10 subst4 subst5;
  test_compo 11 subst2 subst1;
  test_compo 12 subst3 subst1;
  test_compo 13 subst4 subst1;
  test_compo 14 subst5 subst1;
  test_compo 15 subst3 subst2;
  test_compo 16 subst4 subst2;
  test_compo 17 subst5 subst2;
  test_compo 18 subst4 subst3;
  test_compo 19 subst5 subst3;
  test_compo 20 subst5 subst4

let testSubst () =
  test_subst 1 subst1 t1;
  test_subst 2 subst1 t2;
  test_subst 3 subst1 t3;
  test_subst 4 subst1 t4;
  test_subst 5 subst1 t5;
  test_subst 6 subst1 t6;
  test_subst 7 subst1 t7;
  test_subst 8 subst1 t8;
  test_subst 9 subst1 t9;
  test_subst 10 subst1 t10;
  test_subst 11 subst1 t11;
  test_subst 12 subst1 t12;
  test_subst 13 subst1 t13;
  test_subst 14 subst1 t14;
  test_subst 15 subst1 t15;
  test_subst 16 subst1 t16;

  test_subst 17 subst2 t1;
  test_subst 18 subst2 t2;
  test_subst 19 subst2 t3;
  test_subst 20 subst2 t4;
  test_subst 21 subst2 t5;
  test_subst 22 subst2 t6;
  test_subst 23 subst2 t7;
  test_subst 24 subst2 t8;
  test_subst 25 subst2 t9;
  test_subst 26 subst2 t10;
  test_subst 27 subst2 t11;
  test_subst 28 subst2 t12;
  test_subst 29 subst2 t13;
  test_subst 30 subst2 t14;
  test_subst 31 subst2 t15;
  test_subst 32 subst2 t16;

  test_subst 33 subst3 t1;
  test_subst 34 subst3 t2;
  test_subst 35 subst3 t3;
  test_subst 36 subst3 t4;
  test_subst 37 subst3 t5;
  test_subst 38 subst3 t6;
  test_subst 39 subst3 t7;
  test_subst 40 subst3 t8;
  test_subst 41 subst3 t9;
  test_subst 42 subst3 t10;
  test_subst 43 subst3 t11;
  test_subst 44 subst3 t12;
  test_subst 45 subst3 t13;
  test_subst 46 subst3 t14;
  test_subst 47 subst3 t15;
  test_subst 48 subst3 t16;

  test_subst 49 subst4 t1;
  test_subst 50 subst4 t2;
  test_subst 51 subst4 t3;
  test_subst 52 subst4 t4;
  test_subst 53 subst4 t5;
  test_subst 54 subst4 t6;
  test_subst 55 subst4 t7;
  test_subst 56 subst4 t8;
  test_subst 57 subst4 t9;
  test_subst 58 subst4 t10;
  test_subst 59 subst4 t11;
  test_subst 60 subst4 t12;
  test_subst 61 subst4 t13;
  test_subst 62 subst4 t14;
  test_subst 63 subst4 t15;
  test_subst 64 subst4 t16;

  test_subst 65 subst5 t1;
  test_subst 66 subst5 t2;
  test_subst 67 subst5 t3;
  test_subst 68 subst5 t4;
  test_subst 69 subst5 t5;
  test_subst 70 subst5 t6;
  test_subst 71 subst5 t7;
  test_subst 72 subst5 t8;
  test_subst 73 subst5 t9;
  test_subst 74 subst5 t10;
  test_subst 75 subst5 t11;
  test_subst 76 subst5 t12;
  test_subst 77 subst5 t13;
  test_subst 78 subst5 t14;
  test_subst 79 subst5 t15;
  test_subst 80 subst5 t16

(* Main function to run all tests *)
let () =
  testsignature ();
  testwfterm ();
  testHtSizeVars ();
  testmgu ();
  testCompoSubst ();
  testSubst ();
  test_sig 1 sign1;
  test_wfterm 2 sign2 t_wrong;
  test_ht_size_vars 3 t_right;
  print_term (mirror t_nested);
  Printf.printf "\ntesting mgu\n\n";
  testmgu ()
