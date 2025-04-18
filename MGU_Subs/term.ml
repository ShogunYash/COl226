(* term_test.ml *)

(* Use OUnit2 for unit testing *)
open OUnit2

module Term = struct
  (* Type definitions *)
  type variable = string
  type symbol = string * int
  type term =
    | V of variable
    | Node of symbol * (term array)

  (* A signature is represented as a list of symbols (with their arities). *)
  type signature = symbol list

  (* Check that a signature is valid:
     - every arity is nonnegative,
     - no two symbols share the same string.
  *)
  let check_sig (sigs: signature) : bool =
    let arity_ok = List.for_all (fun (_, arity) -> arity >= 0) sigs in
    let names = List.map fst sigs in
    let rec no_duplicates seen = function
      | [] -> true
      | x :: xs -> if List.mem x seen then false else no_duplicates (x :: seen) xs
    in
    arity_ok && no_duplicates [] names

  (* Check that a term is well-formed with respect to a valid signature.
     Each Node (sym, children) must have exactly as many children as given by its arity
     and the symbol must appear in the signature.
  *)
  let rec wfterm (sig_list: signature) (t: term) : bool =
    match t with
    | V _ -> true
    | Node ((s, arity), children) ->
        let symbol_ok =
          List.exists (fun (s', arity') -> s = s' && arity = arity') sig_list
        in
        Array.length children = arity &&
        symbol_ok &&
        Array.for_all (wfterm sig_list) children

  (* Return the height of term t:
     Height of a variable is 0;
     A Node’s height is 1 plus the maximum height of its children.
  *)
  let rec ht (t: term) : int =
    match t with
    | V _ -> 0
    | Node (_, children) ->
        let child_heights = Array.map ht children in
        let max_child =
          if Array.length child_heights = 0 then 0
          else Array.fold_left max 0 child_heights
        in
        1 + max_child

  (* Return the number of nodes in term t. *)
  let rec size (t: term) : int =
    match t with
    | V _ -> 1
    | Node (_, children) -> 1 + Array.fold_left (fun acc t -> acc + size t) 0 children

  (* A set of variables (as strings). *)
  module VarSet = Set.Make(String)
  let rec vars (t: term) : VarSet.t =
    match t with
    | V v -> VarSet.singleton v
    | Node (_, children) ->
        Array.fold_left (fun acc t -> VarSet.union acc (vars t))
          VarSet.empty children

  (* A substitution is represented as an association list from variables to terms.
     In the unique homomorphic extension, only those variables changed (nonidentity)
     appear.
  *)
  type substitution = (variable * term) list

  (* Apply substitution sigma to term t (the unique homomorphic extension of sigma). *)
  let rec subst (sigma: substitution) (t: term) : term =
    match t with
    | V v -> (try List.assoc v sigma with Not_found -> V v)
    | Node (sym, children) ->
        Node (sym, Array.map (subst sigma) children)

  (* Compose substitutions: for s1 and s2, compose_subst s1 s2 is the substitution s
     such that for any term t, subst s t = subst s2 (subst s1 t). *)
  let compose_subst (s1: substitution) (s2: substitution) : substitution =
    let s1' = List.map (fun (v, t) -> (v, subst s2 t)) s1 in
    let s2_filtered =
      List.filter (fun (v, _) -> not (List.exists (fun (v', _) -> v = v') s1)) s2
    in
    s1' @ s2_filtered

  (* Exception raised when no unifier exists. *)
  exception NOT_UNIFIABLE

  (* Check whether variable v appears anywhere in term t. *)
  let rec occurs (v: variable) (t: term) : bool =
    match t with
    | V v' -> v = v'
    | Node (_, children) -> Array.exists (occurs v) children

  (* Compute the most general unifier of t1 and t2.
     If unification fails, raise NOT_UNIFIABLE.
  *)
  let rec mgu (t1: term) (t2: term) : substitution =
    match t1, t2 with
    | V v1, V v2 ->
        if v1 = v2 then [] else [ (v1, V v2) ]
    | V v, t | t, V v ->
        if occurs v t then raise NOT_UNIFIABLE else [ (v, t) ]
    | Node (sym1, args1), Node (sym2, args2) ->
        if sym1 <> sym2 then raise NOT_UNIFIABLE;
        if Array.length args1 <> Array.length args2 then raise NOT_UNIFIABLE;
        let s = ref [] in
        for i = 0 to Array.length args1 - 1 do
          let a = subst !s args1.(i) in
          let b = subst !s args2.(i) in
          let s_i = mgu a b in
          s := compose_subst !s s_i
        done;
        !s

  (* Edit a term: Given a term t, a legal position pos (a list of indices) and a replacement term new_sub,
     replace the subtree at that position with new_sub.
     An empty position replaces the whole term.
  *)
  let rec edit (t: term) (pos: int list) (new_sub: term) : term =
    match pos, t with
    | [], _ -> new_sub
    | i :: rest, Node (sym, children) ->
        if i < 0 || i >= Array.length children then
          failwith "edit: invalid position"
        else
          let new_children = Array.copy children in
          new_children.(i) <- edit new_children.(i) rest new_sub;
          Node (sym, new_children)
    | _ -> failwith "edit: invalid position (encountered variable)"

  (* In-place substitution: update the term t by replacing free occurrences of variables
     according to sigma by modifying the children arrays.
  *)
  let rec subst_in_place (sigma: substitution) (t: term) : unit =
    match t with
    | V _ -> ()  (* leaf; nothing to change in place *)
    | Node (_, children) ->
        for i = 0 to Array.length children - 1 do
          match children.(i) with
          | V v ->
              (try
                 let new_t = List.assoc v sigma in
                 children.(i) <- new_t
               with Not_found -> ())
          | Node _ as t' -> subst_in_place sigma t'
        done
end

(* === Test cases === *)
open Term
module VS = VarSet

(* Some sample signatures *)
let sig1 = [ ("0", 0); ("1", 0); ("+", 2); ("*", 2) ]

(* Some sample terms used in tests *)
let zero = Node (("0", 0), [||])
let one = Node (("1", 0), [||])
let x = V "x"
let y = V "y"
let z = V "z"
let plus_term = Node (("+", 2), [| zero; one |])
let times_term = Node (("*", 2), [| one; x |])
let wrong_arity = Node (("0", 0), [| x |])
let unknown_sym = Node (("non", 1), [| x |])

(* For testing nested terms *)
let nested_term =
  Node (("+",2), [| x;
                    Node (("*",2), [| y; x |]) |])

(* === check_sig tests === *)
let test_check_sig_valid _ =
  assert_bool "Signature sig1 should be valid" (check_sig sig1)

let test_check_sig_dup _ =
  let dup_sig = [ ("0", 0); ("0", 1) ] in
  assert_bool "Duplicate symbol in signature" (not (check_sig dup_sig))

let test_check_sig_negative _ =
  let neg_sig = [ ("a", -1) ] in
  assert_bool "Negative arity signature" (not (check_sig neg_sig))

(* === wfterm tests === *)
let test_wfterm_valid_zero _ =
  assert_bool "zero is well-formed" (wfterm sig1 zero)

let test_wfterm_valid_plus _ =
  assert_bool "plus_term is well-formed" (wfterm sig1 plus_term)

let test_wfterm_valid_times _ =
  assert_bool "times_term is well-formed" (wfterm sig1 times_term)

let test_wfterm_invalid_wrong_arity _ =
  assert_bool "wrong arity term should fail" (not (wfterm sig1 wrong_arity))

let test_wfterm_invalid_unknown_sym _ =
  assert_bool "term with unknown symbol" (not (wfterm sig1 unknown_sym))

(* === ht tests === *)
let test_ht_variable _ =
  assert_equal 0 (ht x)

let test_ht_const _ =
  (* Node with no children: height = 1 *)
  assert_equal 1 (ht zero)

let test_ht_plus _ =
  (* plus_term: each child height = 1, so ht = 1 + max(1,1) = 2 *)
  assert_equal 2 (ht plus_term)

let test_ht_nested _ =
  (* nested_term: height = 1 + max(0, 1+max(0,0)) = 2 *)
  assert_equal 2 (ht nested_term)

(* === size tests === *)
let test_size_variable _ =
  assert_equal 1 (size x)

let test_size_const _ =
  assert_equal 1 (size zero)

let test_size_plus _ =
  (* plus_term: 1 (root) + 1 (zero) + 1 (one) = 3 *)
  assert_equal 3 (size plus_term)

let test_size_nested _ =
  (* nested_term: root + size(x)=1 + size(Node("*",...))=1+ (1+1)= 1+1+2 =4 *)
  let expected = 4 in
  assert_equal expected (size nested_term)

(* === vars tests === *)
let test_vars_variable _ =
  assert_bool "vars of x" (VS.equal (vars x) (VS.singleton "x"))

let test_vars_const _ =
  assert_bool "vars of constant" (VS.equal (vars zero) VS.empty)

let test_vars_plus _ =
  let term = Node (("+", 2), [| x; y |]) in
  assert_bool "vars plus" (VS.equal (vars term) (VS.of_list ["x"; "y"]))

let test_vars_nested _ =
  let term = Node (("+", 2), [| x;
                                 Node (("*",2), [| y; x |]) |])
  in
  assert_bool "vars nested" (VS.equal (vars term) (VS.of_list ["x"; "y"]))

(* === subst tests === *)
let test_subst_identity _ =
  let sigma = [] in
  let t = nested_term in
  assert_equal t (subst sigma t)

let test_subst_simple _ =
  let sigma = [ ("x", one); ("y", zero) ] in
  (* substituting x in x yields one *)
  assert_equal one (subst sigma x);
  (* In a term where only x appears, substitution should change it *)
  let t = Node (("+", 2), [| x; z |]) in
  let t' = Node (("+", 2), [| one; z |]) in
  assert_equal t' (subst sigma t)

let test_subst_nonexistent _ =
  let sigma = [ ("x", one) ] in
  (* substituting y when not in sigma returns original variable *)
  assert_equal y (subst sigma y)

let test_subst_nested _ =
  let sigma = [ ("x", zero) ] in
  let t = Node (("+", 2), [| x; Node (("*",2), [| x; y |]) |]) in
  let t_expected = Node (("+", 2), [| zero; Node (("*",2), [| zero; y |]) |]) in
  assert_equal t_expected (subst sigma t)

(* === compose_subst tests === *)
let test_compose_subst_basic _ =
  let s1 = [ ("x", V "y") ] in
  let s2 = [ ("y", one) ] in
  let composed = compose_subst s1 s2 in
  (* Applying composed substitution to V "x" should yield one *)
  assert_equal one (subst composed (V "x"))

(* === mgu tests === *)
let test_mgu_same_variable _ =
  (* Unify two occurrences of the same variable: mgu should be identity *)
  assert_equal [] (mgu x x)

let test_mgu_diff_variables _ =
  (* For distinct variables, our implementation returns [("x", V "y")] *)
  assert_equal [("x", V "y")] (mgu x y)

let test_mgu_nodes _ =
  (* Unify: Node("+",[| V "x"; one|]) and Node("+",[| zero; V "y" |]) *)
  let t1 = Node (("+",2), [| x; one |]) in
  let t2 = Node (("+",2), [| zero; y |]) in
  let sigma = mgu t1 t2 in
  (* Check that applying sigma makes t1 and t2 equal *)
  assert_equal (subst sigma t1) (subst sigma t2);
  (* We also expect sigma to map "x" to zero and "y" to one *)
  assert_equal [("x", zero); ("y", one)] sigma

let test_mgu_failure _ =
  (* Unification fails when the occurs-check is triggered:
     Unifying V "x" with Node(("f",1), [| V "x" |]) should fail. *)
  let f_term = Node (("f", 1), [| x |]) in
  assert_raises NOT_UNIFIABLE (fun () -> mgu x f_term)

(* === edit tests === *)
let test_edit_root _ =
  (* Replace the whole term with one *)
  let t = plus_term in
  let t' = one in
  assert_equal t' (edit t [] one)

let test_edit_child _ =
  (* Replace left child of Node(("+",[| x; y |])) with one *)
  let t = Node (("+", 2), [| x; y |]) in
  let expected = Node (("+", 2), [| one; y |]) in
  assert_equal expected (edit t [0] one)

let test_edit_deep _ =
  (* For a term with two levels:
       let t = Node (("*",2), [| z; Node (("+",2), [| x; y |]) |])
     update the left child of the second child.
  *)
  let inner = Node (("+",2), [| x; y |]) in
  let t = Node (("*",2), [| z; inner |]) in
  let inner_expected = Node (("+",2), [| zero; y |]) in
  let t_expected = Node (("*",2), [| z; inner_expected |]) in
  assert_equal t_expected (edit t [1; 0] zero)

(* === subst_in_place tests === *)
let test_subst_in_place_simple _ =
  (* Create a term with a Node whose children are variables.
     Then perform an in-place update for "x". *)
  let t = Node (("+",2), [| x; y |]) in
  subst_in_place [("x", one)] t;
  let expected = Node (("+",2), [| one; y |]) in
  assert_equal expected t

let test_subst_in_place_nested _ =
  (* Create a nested term and perform in-place substitution in a subnode. *)
  let inner = Node (("*",2), [| x; y |]) in
  let t = Node (("+",2), [| x; inner |]) in
  subst_in_place [("x", zero)] t;
  let inner_expected = Node (("*",2), [| zero; y |]) in
  let expected = Node (("+",2), [| zero; inner_expected |]) in
  assert_equal expected t

(* === Collect all tests into a test suite === *)
let suite =
  "Term Module Test Suite" >::: [
    (* check_sig: 3 tests *)
    "check_sig_valid" >:: test_check_sig_valid;
    "check_sig_dup" >:: test_check_sig_dup;
    "check_sig_negative" >:: test_check_sig_negative;
    (* wfterm: 5 tests *)
    "wfterm_zero" >:: test_wfterm_valid_zero;
    "wfterm_plus" >:: test_wfterm_valid_plus;
    "wfterm_times" >:: test_wfterm_valid_times;
    "wfterm_wrong_arity" >:: test_wfterm_invalid_wrong_arity;
    "wfterm_unknown_sym" >:: test_wfterm_invalid_unknown_sym;
    (* ht: 4 tests *)
    "ht_variable" >:: test_ht_variable;
    "ht_const" >:: test_ht_const;
    "ht_plus" >:: test_ht_plus;
    "ht_nested" >:: test_ht_nested;
    (* size: 4 tests *)
    "size_variable" >:: test_size_variable;
    "size_const" >:: test_size_const;
    "size_plus" >:: test_size_plus;
    "size_nested" >:: test_size_nested;
    (* vars: 4 tests *)
    "vars_variable" >:: test_vars_variable;
    "vars_const" >:: test_vars_const;
    "vars_plus" >:: test_vars_plus;
    "vars_nested" >:: test_vars_nested;
    (* subst: 4 tests *)
    "subst_identity" >:: test_subst_identity;
    "subst_simple" >:: test_subst_simple;
    "subst_nonexistent" >:: test_subst_nonexistent;
    "subst_nested" >:: test_subst_nested;
    (* compose_subst: 1 test *)
    "compose_subst_basic" >:: test_compose_subst_basic;
    (* mgu: 4 tests *)
    "mgu_same_variable" >:: test_mgu_same_variable;
    "mgu_diff_variables" >:: test_mgu_diff_variables;
    "mgu_nodes" >:: test_mgu_nodes;
    "mgu_failure" >:: test_mgu_failure;
    (* edit: 3 tests *)
    "edit_root" >:: test_edit_root;
    "edit_child" >:: test_edit_child;
    "edit_deep" >:: test_edit_deep;
    (* subst_in_place: 2 tests *)
    "subst_in_place_simple" >:: test_subst_in_place_simple;
    "subst_in_place_nested" >:: test_subst_in_place_nested;
  ]

(* Run all tests *)
let () =
  run_test_tt_main suite
