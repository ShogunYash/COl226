open Term_array
open Term

(* ---------- TESTING UTILITIES ---------- *)

let test_case description f =
  try
    f ();
    ()
  with
  | Assert_failure _ -> Printf.printf "%s failed\n" description


(* ---------- SIGNATURE TESTS ---------- *)
let sig_valid = Array.of_list [("f", 2); ("g", 1); ("h", 0); ("i", 3)] 

let () = test_case "check_sig sig_valid" (fun () -> 
  assert (check_sig sig_valid)
)
let () = test_case "check_sig sig_invalid_arity" (fun () -> 
  let sig_invalid_arity = Array.of_list [("f", -1); ("g", 2)] in
  assert (not (check_sig sig_invalid_arity))
)
let () = test_case "check_sig sig_invalid_duplicate" (fun () -> 
  let sig_invalid_duplicate = Array.of_list [("f", 2); ("f", 3)] in
  assert (not (check_sig sig_invalid_duplicate))
)
let () = test_case "check_sig sig_empty" (fun () -> 
  let sig_empty = Array.of_list [] in
  assert (check_sig sig_empty)
)

(* ---------- WELL-FORMED TERM TESTS ---------- *)

let () = test_case "wfterm t_valid1" (fun () -> 
  let t_valid1 = Node(("f", 2), [| V "x"; Node(("g", 1), [| V "y" |]) |]) in
  assert (wfterm sig_valid t_valid1)
)
let () = test_case "wfterm t_valid2" (fun () -> 
  let t_valid2 = Node(("i", 3), [| Node(("h", 0), [||]); V "z"; Node(("h", 0), [||]) |]) in
  assert (wfterm sig_valid t_valid2)
)
let () = test_case "wfterm t_invalid1" (fun () -> 
  let t_invalid1 = Node(("f", 2), [| V "x" |])  (* arity mismatch *) in
  assert (not (wfterm sig_valid t_invalid1))
)
let () = test_case "wfterm t_invalid2" (fun () -> 
  let t_invalid2 = Node(("unknown", 1), [| V "x" |])  (* symbol missing *) in
  assert (not (wfterm sig_valid t_invalid2))
)

(* ---------- TERM PROPERTIES TESTS ---------- *)
let t_complex = Node(("f", 2), [|
      Node(("g", 1), [| Node(("g", 1), [| V "x" |]) |]);
    Node(("g", 1), [| Node(("g", 1), [| Node(("g", 1), [| V "y" |]) |]) |])
  |]) 
(* Define substitutions as association lists to match the implementation in term_array.ml *)
let sub1 = [("x", Node(("h", 0), [||]))]
let sub2 = [("y", Node(("h", 0), [||]))]

(* Apply substitution to complex term *)
let t_subbed = subst sub1 t_complex

(* Define compose using the compose_subst function from the module *)
let compose = compose_subst

(* Create the composite substitution *)
let comp = compose sub1 sub2

let () = test_case "ht t_complex" (fun () -> 
  assert (ht t_complex = 4)
)
let () = test_case "size t_complex" (fun () -> 
  assert (size t_complex = 8)
)
let vars_list = VarSet.elements (vars t_complex) 
let () = test_case "vars t_complex" (fun () -> 
  assert ((vars_list) = ["x"; "y"])
)

(* ---------- SUBSTITUTION AND COMPOSITION TESTS ---------- *)

let () = test_case "subst sub1 size" (fun () -> 
  assert (size t_subbed = 8)
)
let () = test_case "compose sub1 sub2 on x" (fun () -> 
  match subst comp (V "x") with Node(("h", 0), [||]) -> () | _ -> assert false
)
let () = test_case "compose sub1 sub2 on y" (fun () -> 
  match subst comp (V "y") with Node(("h", 0), [||]) -> () | _ -> assert false
)
let () = test_case "compose sub1 sub2 on z" (fun () -> 
  match subst comp (V "z") with V "z" -> () | _ -> assert false
)

(* ---------- MGU TESTS ---------- *)

let () = test_case "mgu trivial unification" (fun () ->
  let t1 = V "x" and t2 = V "x" in
  let u = mgu t1 t2 in
  assert (subst u t1 = subst u t2)
)

let () = test_case "mgu unify variable with node" (fun () ->
  let t1 = V "x" in
  let t2 = Node(("h", 0), [||]) in
  let u = mgu t1 t2 in
  assert (subst u t1 = subst u t2)
)

let () = test_case "mgu failure due to occurs check" (fun () ->
  let t1 = V "x" in
  let t2 = Node(("g", 1), [| V "x" |]) in
  (try let _ = mgu t1 t2 in assert false with NOT_UNIFIABLE -> ())
)

let () = test_case "mgu non-trivial unification" (fun () ->
  let t1 = Node(("f", 2), [| V "x"; Node(("h", 0), [||]) |]) in
  let t2 = Node(("f", 2), [| Node(("g", 1), [| V "z" |]); V "y" |]) in
  let u = mgu t1 t2 in
  assert (subst u t1 = subst u t2)
)

let () = test_case "mgu failure due to symbol mismatch" (fun () ->
  let t1 = Node(("f", 2), [| V "x"; V "y" |]) in
  let t2 = Node(("g", 2), [| V "x"; V "y" |]) in
  (try let _ = mgu t1 t2 in assert false with NOT_UNIFIABLE -> ())
)

(* ---------- EDIT TESTS ---------- *)

let () = test_case "edit test" (fun () ->
  let t_editable = Node(("f", 2), [| V "x"; V "y" |]) in
  let edited = edit t_editable [0] (Node(("h", 0), [||])) in
  match edited with
  | Node(("f", 2), [| Node(("h", 0), [||]); V "y" |]) -> ()
  | _ -> assert false
)

(* ---------- IN-PLACE SUBSTITUTION TESTS ---------- *)
(* ---------- IN-PLACE SUBSTITUTION TESTS ---------- *)

let () = test_case "inplace_subst test" (fun () ->
  let t_content = Node(("f", 2), [| V "x"; Node(("g", 1), [| V "y" |]) |]) in
  let t_ref = { Term.content = t_content } in
  let sub = [("x", Node(("h", 0), [||])); ("y", Node(("h", 0), [||]))] in
  Term.subst_in_place sub t_ref;
  match t_ref.content with
  | Node(("f", 2), [| Node(("h", 0), [||]); Node(("g", 1), [| Node(("h", 0), [||]) |]) |]) -> ()
  | _ -> assert false
)
(* let () = test_case "inplace_subst test" (fun () ->
  let t = Node(("f", 2), [| V "x"; Node(("g", 1), [| V "y" |]) |]) in
  let sub x = if x = "x" then Node(("h", 0), [||]) else if x = "y" then Node(("h", 0), [||]) else V x in
  subst_in_place sub t;
  match t with
  | Node(("f", 2), [| Node(("h", 0), [||]); Node(("g", 1), [| Node(("h", 0), [||]) |]) |]) -> ()
  | _ -> assert false
)  *)