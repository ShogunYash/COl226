(* term_test.ml *)

(* ---------------- Module Definition ---------------- *)

module Term = struct
  (* Type definitions *)
  type variable = string
  type symbol = string * int
  type term = V of variable | Node of symbol * (term array)

  (* A signature is represented as a list of symbols (with their arities). *)
  type signature = symbol list

  (* Check that a signature is valid:
     - every arity is nonnegative,
     - no two symbols share the same string.
  *)
  let check_sig (sigs: signature) : bool =
    let arity_ok = List.for_all (fun (_, arity) -> arity >= 0) sigs in
    let names = List.map (fun (s, _) -> s) sigs in
    let rec no_duplicates seen = function
      | [] -> true
      | x :: xs -> if List.mem x seen then false else no_duplicates (x :: seen) xs
    in
    arity_ok && no_duplicates [] names

  (* Checks that a preterm is well-formed with respect to a valid signature.
     Each Node ((s, arity), children) must have exactly the number of children given by its arity,
     the symbol must appear in the signature, and every subtree must be well-formed.
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

  (* Computes the height of a term:
     - A variable has height 0.
     - A Node has height 1 plus the maximum height among its children.
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

  (* Computes the size (number of nodes) of a term. *)
  let rec size (t: term) : int =
    match t with
    | V _ -> 1
    | Node (_, children) ->
        1 + Array.fold_left (fun acc t -> acc + size t) 0 children

  (* A set of variables represented using OCaml’s Set functor. *)
  module VarSet = Set.Make(String)
  let rec vars (t: term) : VarSet.t =
    match t with
    | V v -> VarSet.singleton v
    | Node (_, children) ->
        Array.fold_left (fun acc t -> VarSet.union acc (vars t))
          VarSet.empty children

  (* A substitution is represented as an association list mapping variables to terms.
     Only those variables that are not identity appear in the list.
  *)
  type substitution = (variable * term) list

  (* The unique homomorphic extension of a substitution sigma to the term t *)
  let rec subst (sigma: substitution) (t: term) : term =
    match t with
    | V v -> (try List.assoc v sigma with Not_found -> V v)
    | Node (sym, children) ->
        Node (sym, Array.map (subst sigma) children)

  (* Composition of substitutions s1 and s2, defined such that
       subst (compose_subst s1 s2) t = subst s2 (subst s1 t)
  *)
  let compose_subst (s1: substitution) (s2: substitution) : substitution =
    let s1' = List.map (fun (v, t) -> (v, subst s2 t)) s1 in
    let s2_filtered =
      List.filter (fun (v, _) -> not (List.exists (fun (v', _) -> v = v') s1)) s2
    in
    s1' @ s2_filtered

  (* Exception raised when no unifier exists *)
  exception NOT_UNIFIABLE

  (* Occurs-check: returns true if variable v occurs in term t *)
  let rec occurs (v: variable) (t: term) : bool =
    match t with
    | V v' -> v = v'
    | Node (_, children) -> Array.exists (occurs v) children

  (* Compute the most general unifier (mgu) of two terms t1 and t2.
     If unification is impossible, raise NOT_UNIFIABLE.
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

  (* Edit function: given a term, a legal position (list of child indices),
     and a replacement subtree, return a new term where the subtree at that position is replaced.
     The empty list [] means the whole term is replaced.
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

  (* In-place substitution: modifies the term t by replacing free occurrences
     of variables according to the substitution sigma.
  *)
  let rec subst_in_place (sigma: substitution) (t: term) : unit =
    match t with
    | V _ -> ()  (* nothing to modify at a leaf *)
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

(* ---------------- Test Runner (without dune) ---------------- *)

(* A simple runner that takes a test name and a function; if the function
   raises no exception then the test is considered passed. *)
let run_test name f =
  try
    f ();
    Printf.printf "[PASS] %s\n%!" name
  with ex ->
    Printf.printf "[FAIL] %s: %s\n%!" name (Printexc.to_string ex)

open Term
module VS = VarSet

(* ----- Define sample signatures and test terms ----- *)
let sig1 = [ ("0", 0); ("1", 0); ("+", 2); ("*", 2); ("f", 1) ]

let zero = Node (("0", 0), [||])
let one = Node (("1", 0), [||])
let x = V "x"
let y = V "y"
let z = V "z"
let plus_term = Node (("+", 2), [| zero; one |])
let times_term = Node (("*", 2), [| one; x |])
let wrong_arity = Node (("0", 0), [| x |])
let unknown_sym = Node (("non", 1), [| x |])
let nested_term = Node (("+", 2), [| x; Node (("*", 2), [| y; x |]) |])

(* ------------------- Test Cases ------------------- *)

(* check_sig tests *)
let test_check_sig_valid () =
  assert (check_sig sig1)

let test_check_sig_dup () =
  let dup_sig = [ ("0", 0); ("0", 1) ] in
  assert (not (check_sig dup_sig))

let test_check_sig_negative () =
  let neg_sig = [ ("a", -1) ] in
  assert (not (check_sig neg_sig))

(* wfterm tests *)
let test_wfterm_valid_zero () =
  assert (wfterm sig1 zero)

let test_wfterm_valid_plus () =
  assert (wfterm sig1 plus_term)

let test_wfterm_valid_times () =
  assert (wfterm sig1 times_term)

let test_wfterm_invalid_wrong_arity () =
  assert (not (wfterm sig1 wrong_arity))

let test_wfterm_invalid_unknown_sym () =
  assert (not (wfterm sig1 unknown_sym))

(* ht tests *)
let test_ht_variable () =
  assert (ht x = 0)

let test_ht_const () =
  assert (ht zero = 1)

let test_ht_plus () =
  assert (ht plus_term = 2)

let test_ht_nested () =
  assert (ht nested_term = 2)

(* size tests *)
let test_size_variable () =
  assert (size x = 1)

let test_size_const () =
  assert (size zero = 1)

let test_size_plus () =
  assert (size plus_term = 3)

let test_size_nested () =
  let expected = 4 in
  assert (size nested_term = expected)

(* vars tests *)
let test_vars_variable () =
  assert (VS.equal (vars x) (VS.singleton "x"))

let test_vars_const () =
  assert (VS.equal (vars zero) VS.empty)

let test_vars_plus () =
  let term = Node (("+", 2), [| x; y |]) in
  assert (VS.equal (vars term) (VS.of_list ["x"; "y"]))

let test_vars_nested () =
  let term = Node (("+", 2), [| x; Node (("*", 2), [| y; x |]) |]) in
  assert (VS.equal (vars term) (VS.of_list ["x"; "y"]))

(* subst tests *)
let test_subst_identity () =
  let sigma = [] in
  let t = nested_term in
  assert (t = subst sigma t)

let test_subst_simple () =
  let sigma = [ ("x", one); ("y", zero) ] in
  assert (subst sigma x = one);
  let t = Node (("+", 2), [| x; z |]) in
  let t' = Node (("+", 2), [| one; z |]) in
  assert (subst sigma t = t')

let test_subst_nonexistent () =
  let sigma = [ ("x", one) ] in
  assert (subst sigma y = y)

let test_subst_nested () =
  let sigma = [ ("x", zero) ] in
  let t = Node (("+", 2), [| x; Node (("*", 2), [| x; y |]) |]) in
  let t_expected = Node (("+", 2), [| zero; Node (("*", 2), [| zero; y |]) |]) in
  assert (subst sigma t = t_expected)

(* compose_subst tests *)
let test_compose_subst_basic () =
  let s1 = [ ("x", V "y") ] in
  let s2 = [ ("y", one) ] in
  let composed = compose_subst s1 s2 in
  assert (subst composed (V "x") = one)

(* mgu tests *)
let test_mgu_same_variable () =
  assert (mgu x x = [])

let test_mgu_diff_variables () =
  assert (mgu x y = [("x", V "y")])

let test_mgu_nodes () =
  let t1 = Node (("+", 2), [| x; one |]) in
  let t2 = Node (("+", 2), [| zero; y |]) in
  let sigma = mgu t1 t2 in
  (* After applying sigma, t1 and t2 should be identical *)
  assert (subst sigma t1 = subst sigma t2);
  (* Expect substitution mapping "x" to zero and "y" to one (order may vary) *)
  assert (sigma = [("x", zero); ("y", one)] || sigma = [("y", one); ("x", zero)])

let test_mgu_failure () =
  let f_term = Node (("f", 1), [| x |]) in
  try
    let _ = mgu x f_term in
    assert false
  with NOT_UNIFIABLE -> ()

(* edit tests *)
let test_edit_root () =
  let t = plus_term in
  let t' = one in
  assert (edit t [] one = t')

let test_edit_child () =
  let t = Node (("+", 2), [| x; y |]) in
  let expected = Node (("+", 2), [| one; y |]) in
  assert (edit t [0] one = expected)

let test_edit_deep () =
  let inner = Node (("+", 2), [| x; y |]) in
  let t = Node (("*", 2), [| z; inner |]) in
  let inner_expected = Node (("+", 2), [| zero; y |]) in
  let t_expected = Node (("*", 2), [| z; inner_expected |]) in
  assert (edit t [1; 0] zero = t_expected)

(* subst_in_place tests *)
let test_subst_in_place_simple () =
  let t = Node (("+", 2), [| x; y |]) in
  subst_in_place [("x", one)] t;
  let expected = Node (("+", 2), [| one; y |]) in
  assert (t = expected)

let test_subst_in_place_nested () =
  let inner = Node (("*", 2), [| x; y |]) in
  let t = Node (("+", 2), [| x; inner |]) in
  subst_in_place [("x", zero)] t;
  let inner_expected = Node (("*", 2), [| zero; y |]) in
  let expected = Node (("+", 2), [| zero; inner_expected |]) in
  assert (t = expected)

(* ----------------- Collect and Run Tests ----------------- *)

let tests = [
  ("check_sig_valid", test_check_sig_valid);
  ("check_sig_dup", test_check_sig_dup);
  ("check_sig_negative", test_check_sig_negative);
  ("wfterm_valid_zero", test_wfterm_valid_zero);
  ("wfterm_valid_plus", test_wfterm_valid_plus);
  ("wfterm_valid_times", test_wfterm_valid_times);
  ("wfterm_invalid_wrong_arity", test_wfterm_invalid_wrong_arity);
  ("wfterm_invalid_unknown_sym", test_wfterm_invalid_unknown_sym);
  ("ht_variable", test_ht_variable);
  ("ht_const", test_ht_const);
  ("ht_plus", test_ht_plus);
  ("ht_nested", test_ht_nested);
  ("size_variable", test_size_variable);
  ("size_const", test_size_const);
  ("size_plus", test_size_plus);
  ("size_nested", test_size_nested);
  ("vars_variable", test_vars_variable);
  ("vars_const", test_vars_const);
  ("vars_plus", test_vars_plus);
  ("vars_nested", test_vars_nested);
  ("subst_identity", test_subst_identity);
  ("subst_simple", test_subst_simple);
  ("subst_nonexistent", test_subst_nonexistent);
  ("subst_nested", test_subst_nested);
  ("compose_subst_basic", test_compose_subst_basic);
  ("mgu_same_variable", test_mgu_same_variable);
  ("mgu_diff_variables", test_mgu_diff_variables);
  ("mgu_nodes", test_mgu_nodes);
  ("mgu_failure", test_mgu_failure);
  ("edit_root", test_edit_root);
  ("edit_child", test_edit_child);
  ("edit_deep", test_edit_deep);
  ("subst_in_place_simple", test_subst_in_place_simple);
  ("subst_in_place_nested", test_subst_in_place_nested);
]

let () =
  List.iter (fun (name, test_fn) -> run_test name test_fn) tests;
  Printf.printf "All tests completed.\n%!"
