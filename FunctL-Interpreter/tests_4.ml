

(* ---------- SECD MACHINE TESTS ---------- *)

open Secd  (* Assuming your secd.ml exposes SECD module *)

let test_secd_var_lookup () =
  let env = [ENTRY ("x", VCLOS ("x", [], []))] in
  let s, e, c, d = ([], env, [LOOKUP "x"], []) in
  let result = secd_execute s e c d in
  match result with
  | VCLOS ("x", [], _)  -> ()  (* Checking for the closure *)
  | _ -> assert false

let test_secd_simple_function_application () =
  let lam = MKCLOS ("x", [LOOKUP "x"; RET]) in
  let prog = [lam; lam; APP] in
  let result = secd_execute [] [] prog [] in
  match result with
  | VCLOS ("x", [LOOKUP "x"; RET], _) -> ()  (* Checking for the closure *)
  | _ -> assert false

let test_secd_nested_functions () =
  let inner = MKCLOS ("y", [LOOKUP "y"; RET]) in
  let outer = MKCLOS ("x", [inner; RET]) in
  let prog = [outer; outer; APP] in
  let result = secd_execute [] [] prog [] in
  match result with
  | VCLOS ("y", [LOOKUP "y"; RET], _) -> ()  (* Checking for the closure *)
  | _ -> assert false

let test_secd_application_ret () =
  let lam = MKCLOS ("x", [LOOKUP "x"; RET]) in
  let prog = [lam; lam; APP] in
  let result = secd_execute [] [] prog [] in
  match result with
  | VCLOS ("x", [LOOKUP "x"; RET], _) -> ()  (* Checking for the closure *)
  | _ -> assert false

  let test_secd_closure_environment () =
    let lam = MKCLOS ("x", [LOOKUP "x"; RET]) in
    let env = [ENTRY ("y", VCLOS ("y", [], []))] in
    let s, e, c, d = ([], env, [lam], []) in
    let result = secd_execute s e c d in
    match result with
    | VCLOS ("x", [LOOKUP "x"; RET], closure_env) ->
        (* Check that closure_env contains an ENTRY for "y" *)
        let rec contains_y = function
          | ENTRY ("y", _) :: _ -> true
          | _ :: rest -> contains_y rest
          | [] -> false
        in
        assert (contains_y closure_env)  (* Check for closure environment *)
    | _ -> assert false
  
(* ---------- RUN ALL TESTS ---------- *)
 

let () =
  (* SECD Tests *)
  test_case "SECD Var Lookup" test_secd_var_lookup;
  test_case "SECD Simple Function Application" test_secd_simple_function_application;
  test_case "SECD Nested Functions" test_secd_nested_functions;
  test_case "SECD Application RET" test_secd_application_ret;
  test_case "SECD Closure Environment" test_secd_closure_environment 
