(* Define the opcodes for our SECD machine *)
type opcode =
  | LOOKUP of string              (* Look up a variable in the environment *)
  | APP                          (* Function application *)
  | MKCLOS of string * opcode list  (* Build a closure; holds formal parameter and code *)
  | RET                          (* Return from function call *)

(* Closures and environments are defined mutually recursively.
   A closure pairs a formal parameter, a code list (the compiled body), and an environment.
   The environment is a list mapping variable names to closures. *)
type closure = Closure of string * opcode list * env    (* Opcode list contains the body of the function *)
and env = (string * closure) list                       (* Environment: a list of pairs (variable name, closure) *)
(* The environment is a mapping from variable names to closures. *)

(* The SECD machine's state is represented as a tuple of four components:
   - Stack (S): a list of closures (our runtime “values”)
   - Environment (γ): a mapping from variable names to closures
   - Control (C): a list of opcodes to execute
   - Dump (D): a list of triples (stack, environment, control) that record calling contexts. *)

(* The runtime configuration of the SECD machine is a quadruple:
   (stack, environment, control, dump)
   - Stack (S): a list of closures (our runtime “values”)
   - Environment (γ): a mapping from variable names to closures
   - Control (C): a list of opcodes to execute
   - Dump (D): a list of triples (stack, environment, control) that record calling contexts.
*)
type state = closure list * env * opcode list * ((closure list) * env * (opcode list)) list

(* Step function: This performs one transition of the SECD machine. *)
let rec step (st : state) : state option =
  match st with
  | (s, env, c, d) ->
      match c with
      | [] ->
          (* No more instructions. If the dump is nonempty, restore the saved context.
             The rule is:
             (a :: s, env, [], (s', env', c') :: d) ⇒ (a :: s', env', c', d) *)
          (match d with
           | [] -> None  (* Termination: no instructions and no saved context *)
           | (s_rest, env_rest, c_rest) :: d_rest ->
               (match s with
                | a :: _ -> Some (a :: s_rest, env_rest, c_rest, d_rest)
                | [] -> failwith "Empty stack when restoring dump"))
      | instr :: c' ->
          begin match instr with
          | LOOKUP x ->
              (* (LOOKUP) Look up variable x in the current environment
                 and push its associated closure onto the stack *)
              let v =
                try List.assoc x env
                with Not_found -> failwith ("Unbound variable: " ^ x)
              in
              Some (v :: s, env, c', d)
          | MKCLOS (x, c_body) ->
              (* (MKCLOS) Create a closure with parameter x, code c_body,
                 and the CURRENT environment, then push it onto the stack.
                 
                 This implements:
                     (S, γ, MKCLOS(x, c_body)::C, D) ⇒ (⟨x, c_body, γ⟩ :: S, γ, C, D)
              *)
              Some (Closure (x, c_body, env) :: s, env, c', d)
          | APP ->
              (* (APP) For application, the top of the stack should contain:
                 (actual argument) :: (Closure(x, c_body, env_closure)) :: s_rest.
                 
                 We then build a new environment by adding the binding [x ↦ actual]
                 to the saved closure’s environment (env_closure), and set up the new
                 state to execute the code c_body with an empty stack. The current state
                 (s_rest, env, c') is saved on the dump.
                 
                 Thus, the transition is:
                     (a :: Closure(x, c_body, env_closure) :: s, γ, APP:: c', D)
                     ⇒ ([], env_closure[x ↦ a], c_body, ((s, γ, c') :: D))
              *)
              (match s with
               | a :: Closure (x, c_body, env_closure) :: s_rest ->
                   let new_env = (x, a) :: env_closure in
                   Some ([], new_env, c_body, (s_rest, env, c') :: d)
               | _ ->
                   failwith "APP opcode expects a closure and its argument on the stack")
          | RET ->
              (* (RET) Return from a function call.
                 The rule expects that the stack has the result a at the top and
                 there is a context saved on the dump.
                 
                 (a :: s, env, RET :: c, (s_rest, env_rest, c_rest)::d)
                     ⇒ (a :: s_rest, env_rest, c_rest, d)
              *)
              (match s, d with
               | a :: _, (s_rest, env_rest, c_rest) :: d_rest ->
                   Some (a :: s_rest, env_rest, c_rest, d_rest)
               | _ -> failwith "RET encountered with an empty dump")
          end

(* Run function: repeatedly performs steps until no more transitions are possible.
   It returns the final state. *)
let rec run (st : state) : state =
  match step st with
  | None -> st
  | Some st' -> run st'

(* A helper to extract the result from the final state.
   We assume that when the control list and dump are empty,
   the top of the stack is the final answer. *)
let get_result (st : state) =
  let (s, _, c, d) = st in
  match s with
  | v :: _ -> v
  | [] -> failwith "No result on the stack"

(* For testing purposes, an initial state is created with:
   - an empty stack,
   - a given environment,
   - a control list containing the compiled opcodes,
   - an empty dump.
*)
let initial_state (code : opcode list) (env : env) : state =
  ([], env, code, [])

(*
  Example:
  
  Let us simulate the evaluation of the expression:
      ((λx. x) v)
  
  Under a SECD compilation, this might compile to:
      [ MKCLOS("x", [LOOKUP "x"; RET]); LOOKUP "v"; APP ]
  
  We also assume that the variable "v" is bound in the environment to some closure.
  For simplicity, we represent the value of v as a closure with no body and empty env.
*)

let v_value = Closure ("dummy", [], [])  (* a dummy closure representing v *)

let example_code = [ MKCLOS ("x", [LOOKUP "x"; RET]);
                     LOOKUP "v";
                     APP ]
let example_env = [("v", v_value)]

(* Run the example and print a result message *)
let () =
  let final_state = run (initial_state example_code example_env) in
  let _result = get_result final_state in
  (* In this simple testing scenario, we merely print that evaluation finished. *)
  print_endline "Evaluation finished with a result on the top of the stack."
