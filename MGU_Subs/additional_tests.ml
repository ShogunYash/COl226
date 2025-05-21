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
    
    (* Create term_ref for testing *)
    let term_ref = { content = term } in
    let original = string_of_term term_ref.content in
    
    (* Perform in-place substitution *)
    subst_in_place [("x", Node (("1", 0), [||])); ("z", Node (("2", 0), [||]))] term_ref;
    
    (* Show results *)
    let modified = string_of_term term_ref.content in
    Printf.printf "Test 110: In-place substitution\n";
    Printf.printf "Expected: Original: (+[x; (*[y; z])]), Modified: (+[1; (*[y; 2])])\n";
    Printf.printf "Actual: Original: %s, Modified: %s\n\n" original modified;
    
    (* Double substitution test *)
    let term2 = Node (("+", 2), [|
      V "a";
      Node (("*", 2), [| V "b"; V "a" |])
    |]) in
    
    let term2_ref = { content = term2 } in
    let original2 = string_of_term term2_ref.content in
    
    (* First substitution *)
    subst_in_place [("a", Node (("c", 1), [| V "d" |]))] term2_ref;
    let after_first = string_of_term term2_ref.content in
    
    (* Second substitution *)
    subst_in_place [("d", Node (("3", 0), [||]))] term2_ref;
    let after_second = string_of_term term2_ref.content in
    
    Printf.printf "Test 111: Double in-place substitution\n"; 
    Printf.printf "Expected: Original: (+[a; (*[b; a])])\n";
    Printf.printf "Expected: After first: (+[c[d]; (*[b; c[d]])])\n";
    Printf.printf "Expected: After second: (+[c[3]; (*[b; c[3]])])\n";
    Printf.printf "Actual: Original: %s\nAfter first: %s\nAfter second: %s\n\n" original2 after_first after_second;
    
    (* Test variable-only term substitution *)
    let variable_only = V "v" in
    let var_ref = { content = variable_only } in
    let var_original = string_of_term var_ref.content in

    (* Attempt in-place substitution on a variable-only term *)
    subst_in_place [("v", Node (("42", 0), [||]))] var_ref;
    let var_result = string_of_term var_ref.content in
    
    (* Show results *)
    Printf.printf "Test 112: Variable-only in-place substitution\n";
    Printf.printf "Expected: Original: v, Result: 42\n";
    Printf.printf "Actual: Original: %s, After substitution: %s\n" var_original var_result;

    (* Check if the result is what we'd expect from regular substitution *)
    let regular_result = subst [("v", Node (("42", 0), [||]))] variable_only in
    Printf.printf "Regular substitution result: %s\n" (string_of_term regular_result);
    Printf.printf "Results are %s\n\n" 
      (if var_result = string_of_term regular_result 
      then "consistent with regular substitution" 
      else "different from regular substitution");
    Printf.printf "\n"

  (* More comprehensive in-place substitution tests *)
  let test_more_in_place_subst () =
    Printf.printf "=== More In-place Substitution Tests ===\n";
    
    (* Test 1: Nested variables substitution *)
    let term1 = Node (("f", 1), [|
      Node (("g", 2), [| V "x"; V "y" |])
    |]) in
    let term1_ref = { content = term1 } in
    let subst1 = [("x", V "z"); ("y", V "x"); ("z", Node (("h", 1), [| V "w" |]))] in
    
    let expected1 = subst subst1 term1 in
    Printf.printf "Test 113: Nested variable substitution\n";
    Printf.printf "Original: %s\n" (string_of_term term1);
    
    (* Apply in-place substitution *)
    subst_in_place subst1 term1_ref;
    
    Printf.printf "Expected (from regular subst): %s\n" (string_of_term expected1);
    Printf.printf "Actual (from in-place subst): %s\n" (string_of_term term1_ref.content);
    Printf.printf "Results %s\n\n" 
      (if string_of_term expected1 = string_of_term term1_ref.content 
      then "match ✓" 
      else "don't match ✗");
    
    (* Test 2: Substitution chain *)
    let term2 = Node (("cons", 2), [| V "head"; V "tail" |]) in
    let term2_ref = { content = term2 } in
    let subst2 = [
      ("head", Node (("f", 1), [| V "x" |]));
      ("tail", Node (("cons", 2), [| V "y"; V "z" |]));
      ("x", Node (("0", 0), [||]))
    ] in
    
    let expected2 = subst subst2 term2 in
    Printf.printf "Test 114: Substitution chain\n";
    Printf.printf "Original: %s\n" (string_of_term term2);
    
    (* Apply in-place substitution *)
    subst_in_place subst2 term2_ref;
    
    Printf.printf "Expected (from regular subst): %s\n" (string_of_term expected2);
    Printf.printf "Actual (from in-place subst): %s\n" (string_of_term term2_ref.content);
    Printf.printf "Results %s\n\n" 
      (if string_of_term expected2 = string_of_term term2_ref.content 
      then "match ✓" 
      else "don't match ✗");
    
    (* Test 3: Complex nested structure *)
    let term3 = Node (("f", 1), [|
      Node (("g", 2), [|
        Node (("h", 3), [| V "a"; V "b"; V "c" |]);
        V "d"
      |])
    |]) in
    let term3_ref = { content = term3 } in
    let subst3 = [
      ("a", Node (("1", 0), [||]));
      ("b", Node (("f", 1), [| V "b" |])); (* Recursive substitution *)
      ("c", V "d");
      ("d", Node (("2", 0), [||]))
    ] in
    
    let expected3 = subst subst3 term3 in
    Printf.printf "Test 115: Complex nested structure\n";
    Printf.printf "Original: %s\n" (string_of_term term3);
    
    (* Apply in-place substitution *)
    subst_in_place subst3 term3_ref;
    
    Printf.printf "Expected (from regular subst): %s\n" (string_of_term expected3);
    Printf.printf "Actual (from in-place subst): %s\n" (string_of_term term3_ref.content);
    Printf.printf "Results %s\n\n" 
      (if string_of_term expected3 = string_of_term term3_ref.content 
      then "match ✓" 
      else "don't match ✗");
    
    (* Test 4: Empty substitution *)
    let term4 = Node (("f", 1), [| V "x" |]) in
    let term4_ref = { content = term4 } in
    let subst4 = [] in
    
    let expected4 = subst subst4 term4 in
    Printf.printf "Test 116: Empty substitution\n";
    Printf.printf "Original: %s\n" (string_of_term term4);
    
    (* Apply in-place substitution *)
    subst_in_place subst4 term4_ref;
    
    Printf.printf "Expected (from regular subst): %s\n" (string_of_term expected4);
    Printf.printf "Actual (from in-place subst): %s\n" (string_of_term term4_ref.content);
    Printf.printf "Results %s\n\n" 
      (if string_of_term expected4 = string_of_term term4_ref.content 
      then "match ✓" 
      else "don't match ✗");
    
    (* Test 5: Multiple variable occurrences *)
    let term5 = Node (("+", 2), [| V "x"; V "x" |]) in
    let term5_ref = { content = term5 } in
    let subst5 = [("x", Node (("y", 0), [||]))] in
    
    let expected5 = subst subst5 term5 in
    Printf.printf "Test 117: Multiple variable occurrences\n";
    Printf.printf "Original: %s\n" (string_of_term term5);
    
    (* Apply in-place substitution *)
    subst_in_place subst5 term5_ref;
    
    Printf.printf "Expected (from regular subst): %s\n" (string_of_term expected5);
    Printf.printf "Actual (from in-place subst): %s\n" (string_of_term term5_ref.content);
    Printf.printf "Results %s\n\n" 
      (if string_of_term expected5 = string_of_term term5_ref.content 
      then "match ✓" 
      else "don't match ✗");
    
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
  Printf.printf "Original: %s\n" original;
  Printf.printf "Expected: Root edit: 1\n";
  Printf.printf "Actual: Root edit result: %s\n" (string_of_term root_edit);
  Printf.printf "Expected: Left child edit: (+[(*[a; b]); (*[z; w])])\n";
  Printf.printf "Actual: Left child edit result: %s\n" (string_of_term left_edit);
  Printf.printf "Expected: Variable edit: (+[(-[x; y]); (*[0; w])])\n";
  Printf.printf "Actual: Variable edit result: %s\n" (string_of_term var_edit);
  Printf.printf "Expected: Deep edit: (f[g[h[x; y; z]; w]])\n";
  Printf.printf "Actual: Deep edit result: %s\n" (string_of_term complex);
  Printf.printf "Expected: Deep edit : (f[g[h[1; y; z]; w]])\n";
  Printf.printf "Actual: Deep edit result: %s\n\n" (string_of_term deep_edit);
  
  Printf.printf "\n"

  (* Test edit functionality *)
  let test_more_edit () =
    Printf.printf "=== Edit Function Tests ===\n";
    
    (* Test 1: Basic edit at root *)
    let term1 = Node (("f", 1), [| V "x" |]) in
    let new_sub1 = Node (("g", 1), [| V "y" |]) in
    let result1 = edit term1 [] new_sub1 in
    
    Printf.printf "Test 201: Edit at root\n";
    Printf.printf "Original: %s\n" (string_of_term term1);
    Printf.printf "New subtree: %s\n" (string_of_term new_sub1);
    Printf.printf "Result: %s\n" (string_of_term result1);
    Printf.printf "Expected: %s\n\n" (string_of_term new_sub1);
    
    (* Test 2: Edit at first child *)
    let term2 = Node (("f", 2), [| V "x"; V "y" |]) in
    let new_sub2 = Node (("g", 1), [| V "z" |]) in
    let result2 = edit term2 [0] new_sub2 in
    
    Printf.printf "Test 202: Edit first child\n";
    Printf.printf "Original: %s\n" (string_of_term term2);
    Printf.printf "New subtree at [0]: %s\n" (string_of_term new_sub2);
    Printf.printf "Result: %s\n" (string_of_term result2);
    Printf.printf "Expected: (f[g[z]; y])\n\n";
    
    (* Test 3: Edit at nested position *)
    let term3 = Node (("f", 1), [| 
      Node (("g", 2), [|
        V "x"; 
        Node (("h", 1), [| V "y" |])
      |])
    |]) in
    let new_sub3 = V "z" in
    let result3 = edit term3 [0; 1] new_sub3 in
    
    Printf.printf "Test 203: Edit at nested position\n";
    Printf.printf "Original: %s\n" (string_of_term term3);
    Printf.printf "New subtree at [0, 1]: %s\n" (string_of_term new_sub3);
    Printf.printf "Result: %s\n" (string_of_term result3);
    Printf.printf "Expected: (f[g[x; z]])\n\n";
    
    (* Test 4: Edit deep nested position *)
    let term4 = Node (("f", 1), [|
      Node (("g", 1), [|
        Node (("h", 1), [|
          Node (("i", 1), [| V "x" |])
        |])
      |])
    |]) in
    let new_sub4 = Node (("const", 0), [||]) in
    let result4 = edit term4 [0; 0; 0] new_sub4 in
    
    Printf.printf "Test 204: Edit deep nested position\n";
    Printf.printf "Original: %s\n" (string_of_term term4);
    Printf.printf "New subtree at [0, 0, 0]: %s\n" (string_of_term new_sub4);
    Printf.printf "Result: %s\n" (string_of_term result4);
    Printf.printf "Expected: (f[g[h[const]]])\n\n";
    
    (* Test 5: Replace variable with complex term *)
    let term5 = Node (("f", 2), [| V "x"; V "y" |]) in
    let new_sub5 = Node (("+", 2), [| 
      Node (("1", 0), [||]);
      Node (("2", 0), [||])
    |]) in
    let result5 = edit term5 [1] new_sub5 in
    
    Printf.printf "Test 205: Replace variable with complex term\n";
    Printf.printf "Original: %s\n" (string_of_term term5);
    Printf.printf "New subtree at [1]: %s\n" (string_of_term new_sub5);
    Printf.printf "Result: %s\n" (string_of_term result5);
    Printf.printf "Expected: (f[x; (+[1; 2])])\n\n";
    
    (* Test 6: Test invalid position errors *)
    Printf.printf "Test 206: Invalid positions (should show error messages)\n";
    
    (* Out of bounds index *)
    begin try
      let _result = edit term5 [5] new_sub5 in
      Printf.printf "Error: Should have failed with index out of bounds\n"
    with Failure msg ->
      Printf.printf "Correctly failed with: %s\n" msg
    end;
    
    (* Position in a variable *)
    begin try
      let _result = edit term5 [0; 0] new_sub5 in
      Printf.printf "Error: Should have failed with invalid position in variable\n"
    with Failure msg ->
      Printf.printf "Correctly failed with: %s\n" msg
    end;
    
    (* Test 7: Replace entire complex structure *)
    let term7 = complex_term1 in (* Using a complex term defined earlier *)
    let new_sub7 = V "simple" in
    let result7 = edit term7 [] new_sub7 in
    
    Printf.printf "\nTest 207: Replace entire complex structure\n";
    Printf.printf "Original: %s\n" (string_of_term term7);
    Printf.printf "New subtree at []: %s\n" (string_of_term new_sub7);
    Printf.printf "Result: %s\n" (string_of_term result7);
    Printf.printf "Expected: simple\n\n";
    
    (* Test 8: Edit and then substitute *)
    let term8 = Node (("f", 1), [| V "x" |]) in
    let new_sub8 = Node (("g", 1), [| V "y" |]) in
    let result8 = edit term8 [] new_sub8 in
    let subst8 = [("y", Node (("0", 0), [||]))] in
    let final8 = subst subst8 result8 in
    
    Printf.printf "Test 208: Edit then substitute\n";
    Printf.printf "Original: %s\n" (string_of_term term8);
    Printf.printf "After edit: %s\n" (string_of_term result8);
    Printf.printf "After substitution: %s\n" (string_of_term final8);
    Printf.printf "Expected: (g[0])\n\n";
    
    (* Test 9: Edit and create a circular reference (valid if not unifying) *)
    let term9 = Node (("f", 1), [| V "x" |]) in
    let new_sub9 = Node (("g", 1), [| term9 |]) in (* new_sub9 references term9 *)
    let result9 = edit term9 [] new_sub9 in
    
    Printf.printf "Test 209: Edit with circular reference effect\n";
    Printf.printf "Original: %s\n" (string_of_term term9);
    Printf.printf "New subtree: g[...circular reference...]\n";
    Printf.printf "Result first level: (g[%s])\n\n" (string_of_term result9);
    
    Printf.printf "\n"

(* Run all additional tests *)
let () =
  Printf.printf "\n===================================\n";
  Printf.printf "RUNNING ADDITIONAL COMPREHENSIVE TESTS\n";
  Printf.printf "===================================\n\n";

  test_advanced_wfterm ();
  test_advanced_mgu ();
  test_in_place_subst ();
  test_more_in_place_subst ();
  test_edit ();
  test_more_edit ();

  Printf.printf "===================================\n";
  Printf.printf "ALL ADDITIONAL TESTS COMPLETED\n";
  Printf.printf "===================================\n"
