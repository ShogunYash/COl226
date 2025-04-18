(* Additional test cases for the array-based term implementation *)

open Term_array
open Term

(* Advanced test signatures *)
let complex_sig = Array.of_list [
  ("f", 1); ("g", 2); ("h", 3); ("cons", 2); ("nil", 0); 
  ("+", 2); ("-", 2); ("*", 2); ("/", 2); ("pow", 2);
  ("0", 0); ("1", 0); ("2", 0); ("3", 0)
]

(* Complex terms for testing *)
let complex_term1 = (* f(g(x,y), h(z,1,g(2,3)), nil) *)
  Node (("f", 1), [|
    Node (("g", 2), [|
      V "x";
      V "y"
    |]);
    Node (("h", 3), [|
      V "z";
      Node (("1", 0), [||]);
      Node (("g", 2), [|
        Node (("2", 0), [||]);
        Node (("3", 0), [||])
      |])
    |]);
    Node (("nil", 0), [||])
  |])

let complex_term2 = (* f(g(a,b), h(c,1,g(2,3)), nil) *)
  Node (("f", 1), [|
    Node (("g", 2), [|
      V "a";
      V "b"
    |]);
    Node (("h", 3), [|
      V "c";
      Node (("1", 0), [||]);
      Node (("g", 2), [|
        Node (("2", 0), [||]);
        Node (("3", 0), [||])
      |])
    |]);
    Node (("nil", 0), [||])
  |])

(* Test case for circular unification (should fail) *)
let circular1 = V "x"
let circular2 = Node (("f", 1), [| V "x" |])

(* Test case for deeply nested term (deep occurs check) *)
let deep_nested1 = V "x"
let deep_nested2 = 
  Node (("f", 1), [|
    Node (("f", 1), [|
      Node (("f", 1), [|
        Node (("f", 1), [|
          Node (("f", 1), [| V "x" |])
        |])
      |])
    |])
  |])

(* Test case for complex substitution *)
let complex_subst = [
  ("x", Node (("f", 1), [| V "y" |]));
  ("y", Node (("g", 2), [| V "z"; Node (("1", 0), [||]) |]));
  ("z", Node (("h", 3), [| V "a"; V "b"; V "c" |]))
]

(* Chainable substitutions to test composition *)
let chain_subst1 = [("x", V "y")]
let chain_subst2 = [("y", V "z")]
let chain_subst3 = [("z", V "w")]
let chain_subst4 = [("w", Node (("1", 0), [||]))]

(* Test well-formedness with edge cases *)
let test_advanced_wfterm () =
  (* Test correct arities *)
  let valid_term = Node (("g", 2), [| V "x"; V "y" |]) in
  let invalid_term = Node (("g", 2), [| V "x" |]) in
  let invalid_term2 = Node (("g", 2), [| V "x"; V "y"; V "z" |]) in

  (* Test unknown symbols *)
  let unknown_sym_term = Node (("unknown", 1), [| V "x" |]) in

  Printf.printf "=== Advanced Well-Formedness Tests ===\n";
  Printf.printf "Test 101: Valid term with correct arity\n";
  Printf.printf "Expected: Valid Term\n";
  test_wfterm 101 complex_sig valid_term;

  Printf.printf "Test 102: Invalid term with too few children\n";
  Printf.printf "Expected: Invalid Term\n";
  test_wfterm 102 complex_sig invalid_term;

  Printf.printf "Test 103: Invalid term with too many children\n";
  Printf.printf "Expected: Invalid Term\n";
  test_wfterm 103 complex_sig invalid_term2;

  Printf.printf "Test 104: Term with unknown symbol\n";
  Printf.printf "Expected: Invalid Term\n";
  test_wfterm 104 complex_sig unknown_sym_term;
  Printf.printf "\n"

(* Test MGU with complex terms *)
let test_advanced_mgu () =
  Printf.printf "=== Advanced MGU Tests ===\n";
  
  (* Should succeed - unifiable with multiple variable mappings *)
  Printf.printf "Test 105: Complex term unification\n";
  Printf.printf "Expected: Unification successful with mappings for x→a, y→b, z→c\n";
  test_mgu 105 complex_term1 complex_term2;
  
  (* Should fail - circular unification *)
  Printf.printf "Test 106: Circular unification\n";
  Printf.printf "Expected: Not unifiable due to occurs check\n";
  test_mgu 106 circular1 circular2;
  
  (* Should fail - deep occurs check *)
  Printf.printf "Test 107: Deep occurs check\n";
  Printf.printf "Expected: Not unifiable due to deep occurs check\n";
  test_mgu 107 deep_nested1 deep_nested2;
  
  (* Complex substitution test *)
  let test_term = V "x" in
  let result = subst complex_subst test_term in
  Printf.printf "Test 108: Complex substitution result:\n";
  Printf.printf "Expected: f[g[h[a; b; c]; 1]]\n";
  Printf.printf "Actual: %s\n\n" (string_of_term result);
  
  (* Test chained substitution composition *)
  let composed = compose_subst chain_subst1 (compose_subst chain_subst2 (compose_subst chain_subst3 chain_subst4)) in
  let chain_result = subst composed (V "x") in
  Printf.printf "Test 109: Chained substitution composition result:\n";
  Printf.printf "Expected: 1 (after applying x→y→z→w→1)\n";
  Printf.printf "Actual: %s\n\n" (string_of_term chain_result);
  Printf.printf "\n"

(* Test in-place substitution *)
let test_in_place_subst () =
  Printf.printf "=== In-place Substitution Tests ===\n";
  
  (* Create a copyable term *)
  let term = Node (("+", 2), [|
    V "x";
    Node (("*", 2), [| V "y"; V "z" |])
  |]) in
  
  (* Create a copy for testing *)
  let original = string_of_term term in
  
  (* Perform in-place substitution *)
  subst_in_place [("x", Node (("1", 0), [||])); ("z", Node (("2", 0), [||]))] term;
  
  (* Show results *)
  let modified = string_of_term term in
  Printf.printf "Test 110: In-place substitution\n";
  Printf.printf "Expected: Original: (+[x; (*[y; z])]), Modified: (+[1; (*[y; 2])])\n";
  Printf.printf "Actual: Original: %s, Modified: %s\n\n" original modified;
  
  (* Double substitution test *)
  let term2 = Node (("+", 2), [|
    V "a";
    Node (("*", 2), [| V "b"; V "a" |])
  |]) in
  
  let original2 = string_of_term term2 in
  
  (* First substitution *)
  subst_in_place [("a", Node (("c", 1), [| V "d" |]))] term2;
  let after_first = string_of_term term2 in
  
  (* Second substitution *)
  subst_in_place [("d", Node (("3", 0), [||]))] term2;
  let after_second = string_of_term term2 in
  
  Printf.printf "Test 111: Double in-place substitution\n"; 
  Printf.printf "Expected: Original: (+[a; (*[b; a])])\n";
  Printf.printf "Expected: After first: (+[c[d]; (*[b; c[d]])])\n";
  Printf.printf "Expected: After second: (+[c[3]; (*[b; c[3]])])\n";
  Printf.printf "Actual: Original: %s\nAfter first: %s\nAfter second: %s\n\n" 
    original2 after_first after_second;
  Printf.printf "\n"

(* Test edit functionality *)
let test_edit () =
  Printf.printf "=== Edit Tests ===\n";
  
  (* Original term *)
  let term = Node (("+", 2), [|
    Node (("-", 2), [| V "x"; V "y" |]);
    Node (("*", 2), [| V "z"; V "w" |])
  |]) in
  
  let original = string_of_term term in
  
  (* Edit at root - replace the whole term *)
  let root_edit = edit term [] (Node (("1", 0), [||])) in
  
  (* Edit left child *)
  let left_edit = edit term [0] (Node (("*", 2), [| V "a"; V "b" |])) in
  
  (* Edit variable in right child *)
  let var_edit = edit term [1; 0] (Node (("0", 0), [||])) in
  
  (* Edit deeply nested term *)
  let complex = Node (("f", 1), [|
    Node (("g", 2), [|
      Node (("h", 3), [| V "x"; V "y"; V "z" |]);
      V "w"
    |])
  |]) in
  
  let deep_edit = edit complex [0; 0; 0] (Node (("1", 0), [||])) in
  
  Printf.printf "Test 112: Edit operations\n";
  Printf.printf "Expected: Original: (+[(-[x; y]); (*[z; w])])\n";
  Printf.printf "Expected: Root edit: 1\n";
  Printf.printf "Expected: Left child edit: (+[(*[a; b]); (*[z; w])])\n";
  Printf.printf "Expected: Variable edit: (+[(-[x; y]); (*[0; w])])\n";
  Printf.printf "Expected: Deep edit original: (f[g[h[x; y; z]; w]])\n";
  Printf.printf "Expected: Deep edit result: (f[g[h[1; y; z]; w]])\n";
  
  Printf.printf "Actual: Original: %s\n" original;
  Printf.printf "Actual: Root edit: %s\n" (string_of_term root_edit);
  Printf.printf "Actual: Left child edit: %s\n" (string_of_term left_edit);
  Printf.printf "Actual: Variable edit: %s\n" (string_of_term var_edit);
  Printf.printf "Actual: Deep edit original: %s\n" (string_of_term complex);
  Printf.printf "Actual: Deep edit result: %s\n\n" (string_of_term deep_edit);
  Printf.printf "\n"

(* Test mirroring functionality *)
let test_mirror () =
  Printf.printf "=== Mirror Tests ===\n";
  
  (* Mirror a simple term *)
  let simple = Node (("+", 2), [| V "x"; V "y" |]) in
  let simple_mirror = mirror simple in
  
  Printf.printf "Test 113: Simple mirror\n";
  Printf.printf "Expected: Original: (+[x; y]), Mirrored: (+[y; x])\n";
  Printf.printf "Actual: Original: %s, Mirrored: %s\n\n" 
    (string_of_term simple) (string_of_term simple_mirror);
  
  (* Mirror a complex term *)
  let complex = Node (("f", 3), [|
    V "x"; 
    Node (("g", 2), [| V "y"; V "z" |]);
    Node (("h", 2), [| V "a"; V "b" |])
  |]) in
  
  let complex_mirror = mirror complex in
  
  Printf.printf "Test 114: Complex mirror\n";
  Printf.printf "Expected: Original: (f[x; g[y; z]; h[a; b]]), Mirrored: (f[h[a; b]; g[y; z]; x])\n";
  Printf.printf "Expected: Also note children within g and h should be mirrored: (f[h[b; a]; g[z; y]; x])\n";
  Printf.printf "Actual: Original: %s, Mirrored: %s\n\n" 
    (string_of_term complex) (string_of_term complex_mirror);
  
  (* Mirror a term with uneven depth *)
  let uneven = Node (("f", 2), [|
    V "x";
    Node (("g", 3), [| 
      V "a"; 
      Node (("h", 1), [| V "b" |]);
      V "c"
    |])
  |]) in
  
  let uneven_mirror = mirror uneven in
  
  Printf.printf "Test 115: Uneven depth mirror\n";
  Printf.printf "Expected: Original: (f[x; g[a; h[b]; c]]), Mirrored: (f[g[c; h[b]; a]; x])\n";
  Printf.printf "Actual: Original: %s, Mirrored: %s\n\n" 
    (string_of_term uneven) (string_of_term uneven_mirror);
  Printf.printf "\n"

(* Run all additional tests *)
let () =
  Printf.printf "\n===================================\n";
  Printf.printf "RUNNING ADDITIONAL COMPREHENSIVE TESTS\n";
  Printf.printf "===================================\n\n";

  test_advanced_wfterm ();
  test_advanced_mgu ();
  test_in_place_subst ();
  test_edit ();
  test_mirror ();

  Printf.printf "===================================\n";
  Printf.printf "ALL ADDITIONAL TESTS COMPLETED\n";
  Printf.printf "===================================\n"
