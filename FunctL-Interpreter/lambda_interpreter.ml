(* Lambda calculus expressions *)
type expr =
  | Var of string
  | App of expr * expr
  | Abs of string * expr
  | Int of int               (* Integer literals *)
  | Bool of bool             (* Boolean literals *)
  | Add of expr * expr       (* Addition *)
  | Sub of expr * expr       (* Subtraction *)
  | Mul of expr * expr       (* Multiplication *)
  | Div of expr * expr       (* Division *)
  | Equals of expr * expr    (* Equality test *)
  | Lt of expr * expr        (* Less than *)
  | Gt of expr * expr        (* Greater than *)
  | And of expr * expr       (* Logical AND *)
  | Or of expr * expr        (* Logical OR *)
  | If of expr * expr * expr (* Conditional expression *)

(* Values that can result from evaluation *)
type value =
  | VClosure of string * expr * env
  | VInt of int
  | VBool of bool
and env = (string * value) list

(* Krivine Machine Implementation *)

(* Environment maps variables to closures *)
type krivine_env = (string * krivine_closure) list
and krivine_closure = Closure of expr * krivine_env

(* Stack of closures *)
type stack = krivine_closure list

(* Krivine machine state *)
type krivine_state = {
  term: expr;
  env: krivine_env;
  stack: stack
}

(* Extend or update environment with new binding *)
let extend_env x v env = (x, v) :: env

(* Lookup a variable in the environment *)
let rec lookup x = function
  | [] -> failwith ("Unbound variable: " ^ x)
  | (y, v) :: rest -> if x = y then v else lookup x rest

(* Evaluate primitive operations *)
let eval_primitive op e1 e2 env =
  match op, e1, e2 with
  | `Add, Int v1, Int v2 -> Int (v1 + v2)
  | `Sub, Int v1, Int v2 -> Int (v1 - v2)
  | `Mul, Int v1, Int v2 -> Int (v1 * v2)
  | `Div, Int v1, Int v2 -> 
      if v2 = 0 then failwith "Division by zero" else Int (v1 / v2)
  | `Equals, Int v1, Int v2 -> Bool (v1 = v2)
  | `Equals, Bool v1, Bool v2 -> Bool (v1 = v2)
  | `Lt, Int v1, Int v2 -> Bool (v1 < v2)
  | `Gt, Int v1, Int v2 -> Bool (v1 > v2)
  | `And, Bool v1, Bool v2 -> Bool (v1 && v2)
  | `Or, Bool v1, Bool v2 -> Bool (v1 || v2)
  | _ -> failwith "Type error in primitive operation"

(* Single step of Krivine machine - extended for primitives *)
let krivine_step state =
  match state.term, state.env, state.stack with
  | App(e1, e2), env, stack ->
      (* Op rule: <<(e1 e2), gamma>>, S => <<e1, gamma>>, <<e2, gamma>> :: S *)
      { term = e1;
        env = env;
        stack = Closure(e2, env) :: stack }
  
  | Var(x), env, stack ->
      (* Var rule: <<x, gamma>>, S => gamma(x), S *)
      let Closure(e, env') = lookup x env in
      { term = e;
        env = env';
        stack = stack }
  
  | Abs(x, e), env, cl :: stack' ->
      (* Abs rule: <<\x.e, gamma>>, cl :: S => <<e, gamma[x |-> cl]>>, S *)
      { term = e;
        env = extend_env x cl env;
        stack = stack' }

  (* Integer and Boolean literals evaluate to themselves *)
  | Int(_), _, _ -> state
  | Bool(b), env, cl1 :: cl2 :: stack' ->
      (* Special case for booleans with at least two items on stack (like conditional) *)
      if b then
        (* If true, use the first closure *)
        let Closure(e, env') = cl1 in
        { term = e;
          env = env';
          stack = stack' }
      else
        (* If false, use the second closure *)
        let Closure(e, env') = cl2 in
        { term = e;
          env = env';
          stack = stack' }
  | Bool(_), _, _ -> state

  (* Primitive operations *)
  | Add(e1, e2), env, stack ->
      { term = App(App(Abs("x", Abs("y", Add(Var "x", Var "y"))), e1), e2);
        env = env;
        stack = stack }

  | Sub(e1, e2), env, stack ->
      { term = App(App(Abs("x", Abs("y", Sub(Var "x", Var "y"))), e1), e2);
        env = env;
        stack = stack }

  | Mul(e1, e2), env, stack ->
      { term = App(App(Abs("x", Abs("y", Mul(Var "x", Var "y"))), e1), e2);
        env = env;
        stack = stack }

  | Div(e1, e2), env, stack ->
      { term = App(App(Abs("x", Abs("y", Div(Var "x", Var "y"))), e1), e2);
        env = env;
        stack = stack }

  | Equals(e1, e2), env, stack ->
      { term = App(App(Abs("x", Abs("y", Equals(Var "x", Var "y"))), e1), e2);
        env = env;
        stack = stack }

  | Lt(e1, e2), env, stack ->
      { term = App(App(Abs("x", Abs("y", Lt(Var "x", Var "y"))), e1), e2);
        env = env;
        stack = stack }

  | Gt(e1, e2), env, stack ->
      { term = App(App(Abs("x", Abs("y", Gt(Var "x", Var "y"))), e1), e2);
        env = env;
        stack = stack }

  | And(e1, e2), env, stack ->
      { term = App(App(Abs("x", Abs("y", And(Var "x", Var "y"))), e1), e2);
        env = env;
        stack = stack }

  | Or(e1, e2), env, stack ->
      { term = App(App(Abs("x", Abs("y", Or(Var "x", Var "y"))), e1), e2);
        env = env;
        stack = stack }

  | If(cond, then_branch, else_branch), env, stack ->
      (* Translate If to application: if e1 e2 e3 => ((λc.λt.λf.c t f) e1 e2 e3) *)
      let if_expr = App(App(App(
          Abs("c", Abs("t", Abs("f", 
            App(App(Var "c", Var "t"), Var "f")))),
          cond), then_branch), else_branch) in
      { term = if_expr;
        env = env;
        stack = stack }
  
  | _, _, [] -> failwith "Stack is empty, cannot reduce further"

(* Run the machine until it cannot make further steps *)
let rec krivine_eval ?(max_steps = 10000) state =
  let rec eval_with_counter state count =
    if count <= 0 then
      (* Safety limit reached *)
      (Printf.printf "Warning: Evaluation limit reached, terminating\n"; state)
    else
      match state.term with
      | Abs(_, _) when state.stack = [] ->
          (* Final state, lambda with empty stack *)
          state
      | Int(_) when state.stack = [] ->
          (* Final state, integer with empty stack *)
          state
      | Bool(_) when state.stack = [] ->
          (* Final state, boolean with empty stack *)
          state
      | _ ->
          try
            let next_state = krivine_step state in
            eval_with_counter next_state (count - 1)
          with
          | Failure msg ->
              Printf.printf "Evaluation stopped: %s\n" msg;
              state
  in
  eval_with_counter state max_steps

(* Direct evaluation of primitive operations *)
let rec eval_direct expr env =
  match expr with
  | Var x -> lookup x env
  | Abs(x, body) -> VClosure(x, body, env)
  | App(e1, e2) ->
      (match eval_direct e1 env with
       | VClosure(x, body, env') ->
           let v2 = eval_direct e2 env in
           eval_direct body (extend_env x v2 env')
       | _ -> failwith "Application of non-function")
  | Int n -> VInt n
  | Bool b -> VBool b
  | Add(e1, e2) ->
      (match eval_direct e1 env, eval_direct e2 env with
       | VInt n1, VInt n2 -> VInt (n1 + n2)
       | _ -> failwith "Type error in addition")
  | Sub(e1, e2) ->
      (match eval_direct e1 env, eval_direct e2 env with
       | VInt n1, VInt n2 -> VInt (n1 - n2)
       | _ -> failwith "Type error in subtraction")
  | Mul(e1, e2) ->
      (match eval_direct e1 env, eval_direct e2 env with
       | VInt n1, VInt n2 -> VInt (n1 * n2)
       | _ -> failwith "Type error in multiplication")
  | Div(e1, e2) ->
      (match eval_direct e1 env, eval_direct e2 env with
       | VInt n1, VInt n2 -> 
           if n2 = 0 then failwith "Division by zero" else VInt (n1 / n2)
       | _ -> failwith "Type error in division")
  | Equals(e1, e2) ->
      (match eval_direct e1 env, eval_direct e2 env with
       | VInt n1, VInt n2 -> VBool (n1 = n2)
       | VBool b1, VBool b2 -> VBool (b1 = b2)
       | _ -> failwith "Type error in equals")
  | Lt(e1, e2) ->
      (match eval_direct e1 env, eval_direct e2 env with
       | VInt n1, VInt n2 -> VBool (n1 < n2)
       | _ -> failwith "Type error in less than")
  | Gt(e1, e2) ->
      (match eval_direct e1 env, eval_direct e2 env with
       | VInt n1, VInt n2 -> VBool (n1 > n2)
       | _ -> failwith "Type error in greater than")
  | And(e1, e2) ->
      (match eval_direct e1 env, eval_direct e2 env with
       | VBool b1, VBool b2 -> VBool (b1 && b2)
       | _ -> failwith "Type error in logical AND")
  | Or(e1, e2) ->
      (match eval_direct e1 env, eval_direct e2 env with
       | VBool b1, VBool b2 -> VBool (b1 || b2)
       | _ -> failwith "Type error in logical OR")
  | If(cond, then_branch, else_branch) ->
      (match eval_direct cond env with
       | VBool true -> eval_direct then_branch env
       | VBool false -> eval_direct else_branch env
       | _ -> failwith "Condition must evaluate to boolean in if-expression")

(* Unload function: Convert closure back to lambda term *)
let rec unload (Closure(e, env)) =
  match e with
  | Var(x) ->
      (try
         let cl = lookup x env in
         unload cl
       with Failure _ -> Var(x))
  | App(e1, e2) ->
      App(unload (Closure(e1, env)), unload (Closure(e2, env)))
  | Abs(x, e1) ->
      Abs(x, unload (Closure(e1, env)))
  | Int n -> Int n
  | Bool b -> Bool b
  | Add(e1, e2) -> Add(unload (Closure(e1, env)), unload (Closure(e2, env)))
  | Sub(e1, e2) -> Sub(unload (Closure(e1, env)), unload (Closure(e2, env)))
  | Mul(e1, e2) -> Mul(unload (Closure(e1, env)), unload (Closure(e2, env)))
  | Div(e1, e2) -> Div(unload (Closure(e1, env)), unload (Closure(e2, env)))
  | Equals(e1, e2) -> Equals(unload (Closure(e1, env)), unload (Closure(e2, env)))
  | Lt(e1, e2) -> Lt(unload (Closure(e1, env)), unload (Closure(e2, env)))
  | Gt(e1, e2) -> Gt(unload (Closure(e1, env)), unload (Closure(e2, env)))
  | And(e1, e2) -> And(unload (Closure(e1, env)), unload (Closure(e2, env)))
  | Or(e1, e2) -> Or(unload (Closure(e1, env)), unload (Closure(e2, env)))
  | If(c, t, e) -> 
      If(unload (Closure(c, env)), 
         unload (Closure(t, env)), 
         unload (Closure(e, env)))

(* Initialize Krivine machine with a term *)
let init_krivine e =
  { term = e; env = []; stack = [] }

(* Evaluate a term using the Krivine machine *)
let evaluate_krivine e =
  let final_state = krivine_eval (init_krivine e) in
  unload (Closure(final_state.term, final_state.env))

(* String representation of expressions *)
let rec string_of_expr = function
  | Var x -> x
  | App(e1, e2) -> "(" ^ string_of_expr e1 ^ " " ^ string_of_expr e2 ^ ")"
  | Abs(x, e) -> "(λ" ^ x ^ "." ^ string_of_expr e ^ ")"
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | Add(e1, e2) -> "(" ^ string_of_expr e1 ^ " + " ^ string_of_expr e2 ^ ")"
  | Sub(e1, e2) -> "(" ^ string_of_expr e1 ^ " - " ^ string_of_expr e2 ^ ")"
  | Mul(e1, e2) -> "(" ^ string_of_expr e1 ^ " * " ^ string_of_expr e2 ^ ")"
  | Div(e1, e2) -> "(" ^ string_of_expr e1 ^ " / " ^ string_of_expr e2 ^ ")"
  | Equals(e1, e2) -> "(" ^ string_of_expr e1 ^ " = " ^ string_of_expr e2 ^ ")"
  | Lt(e1, e2) -> "(" ^ string_of_expr e1 ^ " < " ^ string_of_expr e2 ^ ")"
  | Gt(e1, e2) -> "(" ^ string_of_expr e1 ^ " > " ^ string_of_expr e2 ^ ")"
  | And(e1, e2) -> "(" ^ string_of_expr e1 ^ " && " ^ string_of_expr e2 ^ ")"
  | Or(e1, e2) -> "(" ^ string_of_expr e1 ^ " || " ^ string_of_expr e2 ^ ")"
  | If(c, t, e) -> 
      "if " ^ string_of_expr c ^ " then " ^ 
      string_of_expr t ^ " else " ^ string_of_expr e

(* String representation of values *)
let rec string_of_value = function
  | VClosure(x, _, _) -> "<closure: λ" ^ x ^ "...>"
  | VInt n -> string_of_int n
  | VBool b -> string_of_bool b

(* Direct evaluation interface *)
let evaluate_direct e =
  eval_direct e []

(* Examples to demonstrate Krivine machine execution *)

(* Identity function: λx.x *)
let identity = Abs("x", Var "x")

(* Apply function: λf.λx.(f x) *)
let apply = Abs("f", Abs("x", App(Var "f", Var "x")))

(* Church booleans *)
let true_church = Abs("t", Abs("f", Var "t"))
let false_church = Abs("t", Abs("f", Var "f"))

(* Church numerals *)
let zero = Abs("f", Abs("x", Var "x"))
let one = Abs("f", Abs("x", App(Var "f", Var "x")))
let two = Abs("f", Abs("x", App(Var "f", App(Var "f", Var "x"))))

(* Successor function: λn.λf.λx.f (n f x) *)
let succ = Abs("n", Abs("f", Abs("x", App(Var "f", App(App(Var "n", Var "f"), Var "x")))))

(* Addition function: λm.λn.λf.λx.m f (n f x) *)
let add = Abs("m", Abs("n", Abs("f", Abs("x", 
  App(App(Var "m", Var "f"), App(App(Var "n", Var "f"), Var "x"))))))

(* Multiplication function: λm.λn.λf.m (n f) *)
let mult = Abs("m", Abs("n", Abs("f", App(Var "m", App(Var "n", Var "f")))))

(* Church pairs *)
let pair = Abs("x", Abs("y", Abs("f", App(App(Var "f", Var "x"), Var "y"))))
let first = Abs("p", App(Var "p", true_church))
let second = Abs("p", App(Var "p", false_church))

(* Conditional *)
let cond = Abs("c", Abs("t", Abs("f", App(App(Var "c", Var "t"), Var "f"))))

(* Y combinator (for recursion): λf.(λx.f (x x)) (λx.f (x x)) *)
let y_comb = Abs("f", 
  App(Abs("x", App(Var "f", App(Var "x", Var "x"))),
      Abs("x", App(Var "f", App(Var "x", Var "x")))))

(* Function to run a test case and print results *)
let run_test name expr =
  Printf.printf "\n=== Testing: %s ===\n" name;
  Printf.printf "Expression: %s\n" (string_of_expr expr);
  
  (* For potentially recursive or complex expressions, we add a safety mechanism *)
  try
    let result = evaluate_krivine expr in
    Printf.printf "Krivine Result: %s\n" (string_of_expr result);
    
    (* For comparison, also evaluate with direct evaluator when possible *)
    try
      let direct_result = evaluate_direct expr in
      Printf.printf "Direct Result: %s\n" (string_of_value direct_result)
    with exn -> 
      Printf.printf "Direct evaluation error: %s\n" (Printexc.to_string exn)
  with exn ->
    Printf.printf "Krivine evaluation error: %s\n" (Printexc.to_string exn)

(* A safer approach to test Church numerals *)
(* Instead of using the Church encoding directly with our interpreter's primitives,
   we'll define specific test cases for Church numerals *)

(* Test Church numeral applications directly *)
let test_church_numeral n name =
  Printf.printf "\n=== Testing Church numeral %s ===\n" name;
  Printf.printf "Original expression: %s\n" (string_of_expr n);
  
  (* Test applying the numeral to simple functions *)
  let simple_apply = App(App(n, Abs("x", Var "x")), Int 42) in
  Printf.printf "Applying to identity: %s\n" (string_of_expr simple_apply);
  
  try 
    let result = evaluate_krivine simple_apply in
    Printf.printf "Result: %s\n" (string_of_expr result)
  with exn ->
    Printf.printf "Evaluation error: %s\n" (Printexc.to_string exn)

(* Run tests *)
let () =
  (* Test identity function *)
  run_test "Identity function" (App(identity, Int 42));

  (* Test apply function *)
  run_test "Apply function" (App(App(apply, identity), Int 42));

  (* Test Church numerals directly instead of converting them *)
  test_church_numeral zero "zero";
  test_church_numeral one "one";
  test_church_numeral two "two";
  
  (* Test Church numerals with simple increment to demonstrate their behavior *)
  let inc_fn = Abs("n", Add(Var "n", Int 1)) in
  run_test "Apply Church zero to increment" (App(App(zero, inc_fn), Int 0));
  run_test "Apply Church one to increment" (App(App(one, inc_fn), Int 0));
  run_test "Apply Church two to increment" (App(App(two, inc_fn), Int 0));

  (* Test pairs *)
  let create_pair a b = App(App(pair, a), b) in
  let test_pair = create_pair (Int 1) (Int 2) in
  run_test "First of pair(1,2)" (App(first, test_pair));
  run_test "Second of pair(1,2)" (App(second, test_pair));

  (* Test conditional *)
  let true_cond = App(App(App(cond, Bool true), Int 1), Int 0) in
  let false_cond = App(App(App(cond, Bool false), Int 1), Int 0) in
  run_test "if true then 1 else 0" true_cond;
  run_test "if false then 1 else 0" false_cond;

  (* Test the embedded conditional syntax *)
  run_test "Embedded if true then 1 else 0" (If(Bool true, Int 1, Int 0));
  run_test "Embedded if false then 1 else 0" (If(Bool false, Int 1, Int 0));
  
  (* Alternative way to test conditionals using direct boolean values *)
  run_test "Direct boolean conditional (true)" (App(App(true_church, Int 1), Int 0));
  run_test "Direct boolean conditional (false)" (App(App(false_church, Int 1), Int 0));

  (* Test with arithmetic operations *)
  run_test "3 + 4" (Add(Int 3, Int 4));
  run_test "(1 + 2) * 3" (Mul(Add(Int 1, Int 2), Int 3));
  
  (* Test with conditional expression using operations *)
  run_test "if 3 > 2 then 10 else 20" (If(Gt(Int 3, Int 2), Int 10, Int 20))
