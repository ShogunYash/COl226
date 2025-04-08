(* Lambda calculus expressions, same as in Krivine machine *)
type expr =
  | Var of string
  | App of expr * expr
  | Lam of string * expr

(* SECD Machine Implementation *)

(* Opcodes for SECD machine *)
type opcode =
  | LOOKUP of string
  | MkCLOS of string * opcode list
  | APP
  | RET

(* Environment maps variables to values *)
type env = (string * value) list
and value = 
  | Closure of string * opcode list * env

(* SECD machine state components *)
type stack = value list
type control = opcode list
type dump = (stack * env * control) list

(* SECD machine state *)
type secd_state = {
  stack: stack;
  env: env;
  control: control;
  dump: dump;
}

(* Extend environment with new binding *)
let extend_env x v env = (x, v) :: env

(* Lookup a variable in the environment *)
let rec lookup x = function
  | [] -> failwith ("Unbound variable: " ^ x)
  | (y, v) :: rest -> if x = y then v else lookup x rest

(* Compiler: translate lambda expression to opcodes *)
let rec compile = function
  | Var x -> [LOOKUP x]
  | App(e1, e2) -> compile e1 @ compile e2 @ [APP]
  | Lam(x, e) -> [MkCLOS(x, compile e @ [RET])]

(* Single step of SECD machine *)
let secd_step state =
  match state.control, state.stack, state.env, state.dump with
  | LOOKUP(x) :: c', s, env, d ->
      (* Var rule: s, gamma, LOOKUP(x)::c', d => gamma(x)::s, gamma, c', d *)
      let v = lookup x env in
      { state with stack = v :: s; control = c' }
  
  | MkCLOS(x, c1) :: c', s, env, d ->
      (* Clos rule: s, gamma, MkCLOS(x,c1)::c', d => <<x,c1,gamma>>::s, gamma, c', d *)
      { state with stack = Closure(x, c1, env) :: s; control = c' }
  
  | APP :: c', v2 :: (Closure(x, c1, env1)) :: s, env, d ->
      (* App rule: v2::<<x,c1,gamma1>>::s, gamma, APP::c', d => [], gamma1[x|->v2], c1, (s,gamma,c')::d *)
      { stack = [];
        env = extend_env x v2 env1;
        control = c1;
        dump = (s, env, c') :: d }
  
  | RET :: _, v :: _, env', (s, env, c') :: d ->
      (* Ret rule: v::s', gamma', RET::_, (s,gamma,c')::d => v::s, gamma, c', d *)
      { stack = v :: s;
        env = env;
        control = c';
        dump = d }
  
  | [], [v], [], [] ->
      (* Final state *)
      { state with control = [] }  (* Mark as done by emptying control *)
  
  | _ -> failwith "Invalid SECD state"

(* Run the machine until it cannot make further steps *)
let rec secd_eval state =
  if state.control = [] then
    state
  else
    try
      let next_state = secd_step state in
      secd_eval next_state
    with
    | Failure msg ->
        Printf.printf "Evaluation stopped: %s\n" msg;
        state

(* Initialize SECD machine with compiled expression *)
let init_secd e =
  { stack = [];
    env = [];
    control = compile e;
    dump = [] }

(* Evaluate an expression using the SECD machine *)
let evaluate_secd e =
  let final_state = secd_eval (init_secd e) in
  match final_state.stack with
  | [v] -> v
  | _ -> failwith "SECD machine ended in unexpected state"

(* String representation of values *)
let rec string_of_value = function
  | Closure(x, _, _) -> "<closure: λ" ^ x ^ "...>"

(* String representation of expressions, same as in Krivine machine *)
let rec string_of_expr = function
  | Var x -> x
  | App(e1, e2) -> "(" ^ string_of_expr e1 ^ " " ^ string_of_expr e2 ^ ")"
  | Lam(x, e) -> "(λ" ^ x ^ "." ^ string_of_expr e ^ ")"
