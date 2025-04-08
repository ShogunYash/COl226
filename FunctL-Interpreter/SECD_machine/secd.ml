(* SECD Machine Implementation in OCaml *)

(* Opcodes *)
type opcode =
  | NUM of int
  | BOOL of bool
  | LOOKUP of string
  | MkCLOS of string * opcode list
  | APP
  | RET
  | ASSIGN of string  (* New opcode for assignment *)

(* Values: integers, booleans, closures, or references *)
type value =
  | IntV of int
  | BoolV of bool
  | CloV of string * opcode list * env
  | RefV of value ref  (* Mutable reference *)

and env = (string * value) list

(* Pretty-printing values *)
let rec string_of_value = function
  | IntV n -> string_of_int n
  | BoolV b -> string_of_bool b
  | CloV (x, code, _) -> Printf.sprintf "<<%s, code(%d), env>>" x (List.length code)
  | RefV r -> Printf.sprintf "ref(%s)" (string_of_value !r)

(* AST for lambda calculus terms plus ints, bools, and assignment *)
type term =
  | Var of string
  | Lam of string * term
  | App of term * term
  | Int of int
  | Bool of bool
  | Assign of string * term  (* New term for variable assignment *)

(* Compiler from term to SECD code *)
let rec compile = function
  | Var x -> [LOOKUP x]
  | Int n -> [NUM n]  (* Fixed: should use NUM instead of LOOKUP *)
  | Bool b -> [BOOL b]  (* Fixed: should use BOOL instead of LOOKUP *)
  | App (e1, e2) -> compile e1 @ compile e2 @ [APP]
  | Lam (x, body) ->
      let body_code = compile body @ [RET] in
      [MkCLOS (x, body_code)]
  | Assign (x, e) ->
      compile e @ [ASSIGN x]  (* Compile the expression and then assign it to x *)

(* SECD machine interpreter *)
let secd code env0 =
  let rec exec s env code d =
    match code with
    | [] -> failwith "Empty code without return"
    | op :: rest -> (
        match op with
        | NUM n ->
            let v = IntV n in
            exec (v :: s) env rest d
        | BOOL b ->
            let v = BoolV b in
            exec (v :: s) env rest d
        | LOOKUP x ->
            let v = try 
                     let found_val = List.assoc x env in
                     (* Automatically dereference if it's a reference *)
                     match found_val with
                     | RefV r -> !r
                     | _ -> found_val
                   with Not_found -> failwith ("Unbound variable: " ^ x)
            in exec (v :: s) env rest d
        | MkCLOS (x, c1) ->
            let clo = CloV (x, c1, env) in
            exec (clo :: s) env rest d
        | APP -> (
            match s with
            | v2 :: CloV (x, c1, env1) :: s' ->
                (* Save context *)
                let d' = (s', env, rest) :: d in
                (* New context *)
                let env_new = (x, v2) :: env1 in
                exec [] env_new c1 d'
            | _ -> failwith "Stack underflow or not a closure on APP"
          )
        | RET -> (
            match s, d with
            | v :: _, (s', env', code') :: d' ->
                exec (v :: s') env' code' d'
            | _ -> failwith "RET with empty dump"
          )
        | ASSIGN x -> (
            match s with
            | v :: s' ->
                (* Try to find the variable in the environment *)
                let new_env = 
                  try
                    let found = List.mem_assoc x env in
                    if found then
                      (* If variable exists, update it *)
                      let updated_env = List.map (fun (name, value) ->
                        if name = x then 
                          match value with
                          | RefV r -> (r := v; (name, RefV r))
                          | _ -> (name, RefV (ref v))
                        else (name, value)
                      ) env in
                      updated_env
                    else
                      (* If variable doesn't exist, add it as a new reference *)
                      (x, RefV (ref v)) :: env
                  with Not_found ->
                    (* If an error occurs, just add a new binding *)
                    (x, RefV (ref v)) :: env
                in
                exec (v :: s') new_env rest d
            | [] -> failwith "Stack underflow on ASSIGN"
          )
      )
  in
  match exec [] env0 code [] with
  | v :: _ -> v
  | _ -> failwith "Empty result stack"

(* Examples and test harness *)
let _ =
  (* Helper to build initial env for literals *)
  let base_env = [] in

  (* Example 1: ((\x. x) 5) => 5 *)
  let term1 = App (Lam ("x", Var "x"), Int 5) in
  let code1 = compile term1 in
  let res1 = secd code1 base_env in
  Printf.printf "Result1: %s\n" (string_of_value res1);

  (* Example 2: ((\x. \y. x) True) False => True *)
  let term2 = App (App (Lam ("x", Lam ("y", Var "x")), Bool true), Bool false) in
  let code2 = compile term2 in
  let res2 = secd code2 base_env in
  Printf.printf "Result2: %s\n" (string_of_value res2);
  
  (* Example 3: Variable assignment - let x = 10, then x := 20, return x *)
  let term3 = App (Lam ("x", 
                       App (Lam ("_", Var "x"),
                            Assign ("x", Int 20))
                      ), Int 10) in
  let code3 = compile term3 in
  let res3 = secd code3 base_env in
  Printf.printf "Result3 (assignment): %s\n" (string_of_value res3)
