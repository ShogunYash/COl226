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
  Printf.printf "=== Signature Tests ===\n";
  Printf.printf "Test 1: Valid signature with unique symbols and non-negative arities\n";
  Printf.printf "Expected: Valid Signature\n";
  test_sig 1 signature1;
  
  Printf.printf "Test 2: Another valid signature\n";
  Printf.printf "Expected: Valid Signature\n";
  test_sig 2 signature2;
  
  Printf.printf "Test 3: Valid signature with different symbols\n";
  Printf.printf "Expected: Valid Signature\n";
  test_sig 3 signature3;
  Printf.printf "\n"

let testwfterm () =
  Printf.printf "=== Well-Formed Term Tests ===\n";
  Printf.printf "Test 1: Term '(x+1)+0' with signature1\n";
  Printf.printf "Expected: Valid Term\n";
  test_wfterm 1 signature1 tree1;
  
  Printf.printf "Test 2: Term '1+(0*x)' with signature2\n";
  Printf.printf "Expected: Valid Term\n";
  test_wfterm 2 signature2 tree2;
  
  Printf.printf "Test 3: Term '(y/sin(cos(z)))-2' with signature3\n";
  Printf.printf "Expected: Valid Term\n";
  test_wfterm 3 signature3 tree3;
  Printf.printf "\n"

let testHtSizeVars () =
  Printf.printf "=== Height, Size, Variables Tests ===\n";
  
  Printf.printf "Test 1: (x+1)+0\n";
  Printf.printf "Expected: Height=3, Size=5, Vars=x\n";
  test_ht_size_vars 1 tree1;
  
  Printf.printf "Test 2: 1+(0*x)\n";
  Printf.printf "Expected: Height=3, Size=5, Vars=x\n";
  test_ht_size_vars 2 tree2;
  
  Printf.printf "Test 3: (y/sin(cos(z)))-2\n";
  Printf.printf "Expected: Height=4, Size=7, Vars=y, z\n";
  test_ht_size_vars 3 tree3;
  
  Printf.printf "Test 4: x (variable)\n";
  Printf.printf "Expected: Height=0, Size=1, Vars=x\n";
  test_ht_size_vars 4 t1;
  
  Printf.printf "Test 5: x (variable, same as Test 4)\n";
  Printf.printf "Expected: Height=0, Size=1, Vars=x\n";
  test_ht_size_vars 5 t2;
  
  Printf.printf "Test 6: x (variable)\n";
  Printf.printf "Expected: Height=0, Size=1, Vars=x\n";
  test_ht_size_vars 6 t3;
  
  Printf.printf "Test 7: y (variable)\n";
  Printf.printf "Expected: Height=0, Size=1, Vars=y\n";
  test_ht_size_vars 7 t4;
  
  Printf.printf "Test 8: x (variable)\n";
  Printf.printf "Expected: Height=0, Size=1, Vars=x\n";
  test_ht_size_vars 8 t5;
  
  Printf.printf "Test 9: f (constant)\n";
  Printf.printf "Expected: Height=1, Size=1, Vars=none\n";
  test_ht_size_vars 9 t6;
  
  Printf.printf "Test 10: g(a,b)\n";
  Printf.printf "Expected: Height=1, Size=3, Vars=a, b\n";
  test_ht_size_vars 10 t7;
  
  Printf.printf "Test 11: g(a,b) (same as Test 10)\n";
  Printf.printf "Expected: Height=1, Size=3, Vars=a, b\n";
  test_ht_size_vars 11 t8;
  
  Printf.printf "Test 12: h(x,y)\n";
  Printf.printf "Expected: Height=1, Size=3, Vars=x, y\n";
  test_ht_size_vars 12 t9;
  
  Printf.printf "Test 13: h(z,w)\n";
  Printf.printf "Expected: Height=1, Size=3, Vars=z, w\n";
  test_ht_size_vars 13 t10;
  
  Printf.printf "Test 14: i(x)\n";
  Printf.printf "Expected: Height=1, Size=2, Vars=x\n";
  test_ht_size_vars 14 t11;
  
  Printf.printf "Test 15: i(y(x))\n";
  Printf.printf "Expected: Height=2, Size=3, Vars=x\n";
  test_ht_size_vars 15 t12;
  
  Printf.printf "Test 16: i(x)\n";
  Printf.printf "Expected: Height=1, Size=2, Vars=x\n";
  test_ht_size_vars 16 t13;
  
  Printf.printf "Test 17: i(y,z)\n";
  Printf.printf "Expected: Height=1, Size=3, Vars=y, z\n";
  test_ht_size_vars 17 t14;
  
  Printf.printf "Test 18: i(x,y)\n";
  Printf.printf "Expected: Height=1, Size=3, Vars=x, y\n";
  test_ht_size_vars 18 t15;
  
  Printf.printf "Test 19: i(i(z),w)\n";
  Printf.printf "Expected: Height=2, Size=4, Vars=z, w\n";
  test_ht_size_vars 19 t16;
  Printf.printf "\n"

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

let testedit () =
  Printf.printf "=== Edit Tests ===\n";
  
  (* Test 1: Replace root node *)
  Printf.printf "Test 1: Replace the entire tree\n";
  Printf.printf "Expected: The replacement tree (V \"z\")\n";
  test_edit 1 t7 [] (V "z");
  
  (* Test 2: Replace left subtree of a node *)
  Printf.printf "Test 2: Replace left subtree of g(a,b) with x\n";
  Printf.printf "Expected: g(x,b)\n";
  test_edit 2 t7 [0] (V "x");
  
  (* Test 3: Replace right subtree of a node *)
  Printf.printf "Test 3: Replace right subtree of g(a,b) with y\n";
  Printf.printf "Expected: g(a,y)\n";
  test_edit 3 t7 [1] (V "y");
  
  (* Test 4: Replace in a deeper nested structure *)
  Printf.printf "Test 4: Replace z in i(i(z),w) with f()\n";
  Printf.printf "Expected: i(i(f()),w)\n";
  test_edit 4 t16 [0, 0] (Node (("f", 0), [||]));
  
  (* Test 5: Invalid position - index out of bounds *)
  Printf.printf "Test 5: Try to access index 2 in g(a,b) which has only 2 children\n";
  Printf.printf "Expected: Edit failed: invalid position\n";
  test_edit 5 t7 [2] (V "c");
  
  (* Test 6: Invalid position - trying to navigate into a variable *)
  Printf.printf "Test 6: Try to navigate into variable x\n";
  Printf.printf "Expected: Edit failed: invalid position (encountered variable)\n";
  test_edit 6 (V "x") [0] (V "y");
  
  (* Test 7: Nested edit in a complex term *)
  Printf.printf "Test 7: Replace sin(cos(z)) with y in (y/sin(cos(z)))-2\n";
  Printf.printf "Expected: (y/y)-2\n";
  test_edit 7 tree3 [0, 1] (V "y");
  
  Printf.printf "\n";

let testsubstinplace () =
  Printf.printf "=== In-place Substitution Tests ===\n";
  
  (* Test 1: Simple variable substitution *)
  Printf.printf "Test 1: Substitute x->a in g(x,y)\n";
  Printf.printf "Expected: g(a,y)\n";
  test_subst_in_place 1 (Node (("g", 2), [| V "x"; V "y" |])) [("x", V "a")];
  
  (* Test 2: Multiple variable substitutions *)
  Printf.printf "Test 2: Substitute x->a, y->b in g(x,y)\n";
  Printf.printf "Expected: g(a,b)\n";
  test_subst_in_place 2 (Node (("g", 2), [| V "x"; V "y" |])) [("x", V "a"); ("y", V "b")];
  
  (* Test 3: Nested structure substitution *)
  Printf.printf "Test 3: Substitute z->f() in i(i(z),w)\n";
  Printf.printf "Expected: i(i(f()),w)\n";
  test_subst_in_place 3 t16 [("z", Node (("f", 0), [||]))];
  
  (* Test 4: Replace with complex structure *)
  Printf.printf "Test 4: Substitute x->g(a,b) in h(x,y)\n";
  Printf.printf "Expected: h(g(a,b),y)\n";
  test_subst_in_place 4 t9 [("x", t7)];
  
  (* Test 5: No matching variables *)
  Printf.printf "Test 5: Substitute z->a in g(x,y) where z doesn't appear\n";
  Printf.printf "Expected: g(x,y) (unchanged)\n";
  test_subst_in_place 5 (Node (("g", 2), [| V "x"; V "y" |])) [("z", V "a")];
  
  (* Test 6: Complex tree with multiple substitutions *)
  Printf.printf "Test 6: Multiple substitutions in (y/sin(cos(z)))-2\n";
  Printf.printf "Expected: (k/sin(cos(a)))-2\n";
  test_subst_in_place 6 tree3 [("y", V "k"); ("z", V "a")];
  
  Printf.printf "\n";

(* Main function to run all tests *)
let () =
  testsignature ();
  testwfterm ();
  testHtSizeVars ();
  testmgu ();
  testCompoSubst ();
  testSubst ();
  testedit ();           (* Add the edit test function *)
  testsubstinplace ();   (* Add the in-place substitution test function *)
  test_sig 1 sign1;
  test_wfterm 2 sign2 t_wrong;
  test_ht_size_vars 3 t_right;
  Printf.printf "\n=============== Testing MGU ===============\n\n";
  testmgu ()
