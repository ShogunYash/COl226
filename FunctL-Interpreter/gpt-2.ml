(* --- Type Definitions --- *)

(* Types for our simple language *)
type typeExp =
  | IntT
  | BoolT
  | PairT of typeExp * typeExp

(* Abstract syntax for expressions *)
type expr =
  | Var of string
  | Add of expr * expr            (* Addition *)
  | Sub of expr * expr            (* Subtraction *)
  | And of expr * expr            (* Boolean and *)
  | Or of expr * expr             (* Boolean or *)
  | IfThenElse of expr * expr * expr  (* if cond then e1 else e2 *)
  | Pair of expr * expr           (* A pair of expressions *)
  | N of int                    (* Integer constant *)
  | B of bool                   (* Boolean constant *)

(* Values produced by evaluation (used only at runtime) *)
type value =
  | NumVal of int
  | BoolVal of bool
  | PairVal of value * value

(* Closure: an expression paired with an environment (a table mapping variable names to closures) *)
type closure = Clos of expr * table
and table = (string * closure) list

(* --- Stack Tokens --- *)

(* Tokens used for pending operations *)
type stack_token =
  | ADDTOK of closure         (* Waiting to add with a number *)
  | SUBTOK of closure         (* Waiting to subtract with a number *)
  | ANDTOK of closure         (* Waiting to perform and *)
  | ORTOK of closure          (* Waiting to perform or *)
  | IFTETOK of closure * closure  (* Waiting for if: then and else branches *)

(* --- Exceptions --- *)

exception StackError of string
exception TableError
exception UnknownError

(* --- Environment Helpers --- *)

(* Augment the table: if key exists, replace it; otherwise add the new binding *)
let rec augment t key cl =
  match t with
  | [] -> [(key, cl)]
  | (k, cl_val) :: rest ->
      if key = k then (key, cl) :: rest else (k, cl_val) :: (augment rest key cl)

(* Lookup a key in the table *)
let rec lookupTable key t =
  match t with
  | [] -> raise TableError
  | (k, cl) :: rest -> if key = k then cl else lookupTable key rest

(* Join two tables; later bindings override earlier ones *)
let rec join t1 t2 =
  match t1 with
  | [] -> t2
  | (k, cl) :: t1_rem -> join t1_rem (augment t2 k cl)

(* --- Substitution and Unload (Unpack) Functions --- *)

(* Substitute free occurrences of variable x in expression e with expression v *)
let rec subst (e : expr) (x : string) (v : expr) : expr =
  match e with
  | Var y -> if y = x then v else Var y
  | Add(e1, e2) -> Add(subst e1 x v, subst e2 x v)
  | Sub(e1, e2) -> Sub(subst e1 x v, subst e2 x v)
  | And(e1, e2) -> And(subst e1 x v, subst e2 x v)
  | Or(e1, e2) -> Or(subst e1 x v, subst e2 x v)
  | IfThenElse(e1, e2, e3) ->
      IfThenElse(subst e1 x v, subst e2 x v, subst e3 x v)
  | Pair(e1, e2) -> Pair(subst e1 x v, subst e2 x v)
  | N n -> N n
  | B b -> B b

(* Unload: remove the environment from a closure by substituting each binding *)
let rec unload (Clos(e, t)) : expr =
  let rec aux e t =
    match t with
    | [] -> e
    | (s, cl) :: rest ->
        let e' = aux e rest in
        let v = unload cl in
        subst e' s v
  in
  aux e t

(* --- Krivine Machine --- *)

(* The Krivine function takes a closure and a stack of tokens, and returns a value *)
let rec krivine (cl: closure) (s: stack_token list) : value =
  match cl with
  | Clos(e, t) ->
      (match e with
       | N(x) ->
           (match s with
            | [] -> NumVal(x)
            | ADDTOK(c) :: s_rem ->
                let v1 = krivine c [] in
                (match v1 with
                 | NumVal(x1) -> krivine (Clos(N(x + x1), t)) s_rem
                 | _ -> raise UnknownError)
            | SUBTOK(c) :: s_rem ->
                let v1 = krivine c [] in
                (match v1 with
                 | NumVal(x1) -> krivine (Clos(N(x - x1), t)) s_rem
                 | _ -> raise UnknownError)
            | _ -> raise (StackError "Incorrect token for integer constant"))
       | B(b) ->
           (match s with
            | [] -> BoolVal(b)
            | ANDTOK(c) :: s_rem ->
                let v1 = krivine c [] in
                (match v1 with
                 | BoolVal(b1) -> krivine (Clos(B(b && b1), t)) s_rem
                 | _ -> raise UnknownError)
            | ORTOK(c) :: s_rem ->
                let v1 = krivine c [] in
                (match v1 with
                 | BoolVal(b1) -> krivine (Clos(B(b || b1), t)) s_rem
                 | _ -> raise UnknownError)
            | IFTETOK(c1, c2) :: s_rem ->
                if b then krivine c1 s_rem else krivine c2 s_rem
            | _ -> raise (StackError "Incorrect token for boolean constant"))
       | Add(e1, e2) ->
           krivine (Clos(e2, t)) (ADDTOK(Clos(e1, t)) :: s)
       | Sub(e1, e2) ->
           krivine (Clos(e2, t)) (SUBTOK(Clos(e1, t)) :: s)
       | And(e1, e2) ->
           krivine (Clos(e2, t)) (ANDTOK(Clos(e1, t)) :: s)
       | Or(e1, e2) ->
           krivine (Clos(e2, t)) (ORTOK(Clos(e1, t)) :: s)
       | IfThenElse(e1, e2, e3) ->
           krivine (Clos(e1, t)) (IFTETOK(Clos(e2, t), Clos(e3, t)) :: s)
       | Pair(e1, e2) ->
           let v1 = krivine (Clos(e1, t)) [] in
           let v2 = krivine (Clos(e2, t)) [] in
           (match s with
            | [] -> PairVal(v1, v2)
            | _ -> raise (StackError "No tokens should be pending when constructing a pair"))
       | Var(x) ->
           krivine (lookupTable x t) s)

(* --- Pretty-printer for Values --- *)

let rec value_to_string v =
  match v with
  | NumVal(x) -> string_of_int x
  | BoolVal(b) -> string_of_bool b
  | PairVal(v1, v2) -> "(" ^ value_to_string v1 ^ ", " ^ value_to_string v2 ^ ")"

(* --- Test Helpers --- *)

let run_eval_test (description: string) (e: closure) =
  try
    let v = krivine e [] in
    Printf.printf "%s => %s\n" description (value_to_string v)
  with
  | StackError s -> Printf.printf "%s => StackError: %s\n" description s
  | TableError -> Printf.printf "%s => TableError\n" description
  | UnknownError -> Printf.printf "%s => UnknownError\n" description

let run_unload_test (description: string) (e: closure) =
  try
    let unloaded = unload e in
    (* Print the unloaded expression using a simple pretty-printer *)
    let rec expr_to_string e =
      match e with
      | Var x -> x
      | N n -> string_of_int n
      | B b -> string_of_bool b
      | Add(e1, e2) -> "(" ^ expr_to_string e1 ^ " + " ^ expr_to_string e2 ^ ")"
      | Sub(e1, e2) -> "(" ^ expr_to_string e1 ^ " - " ^ expr_to_string e2 ^ ")"
      | And(e1, e2) -> "(" ^ expr_to_string e1 ^ " and " ^ expr_to_string e2 ^ ")"
      | Or(e1, e2) -> "(" ^ expr_to_string e1 ^ " or " ^ expr_to_string e2 ^ ")"
      | IfThenElse(e1, e2, e3) ->
          "if " ^ expr_to_string e1 ^ " then " ^ expr_to_string e2 ^ " else " ^ expr_to_string e3
      | Pair(e1, e2) -> "(" ^ expr_to_string e1 ^ ", " ^ expr_to_string e2 ^ ")"
    in
    Printf.printf "%s => %s\n" description (expr_to_string unloaded)
  with
  | StackError s -> Printf.printf "%s => StackError: %s\n" description s
  | TableError -> Printf.printf "%s => TableError\n" description
  | UnknownError -> Printf.printf "%s => UnknownError\n" description

(* --- Test Cases for Evaluation --- *)

(* Empty environment *)
let empty_table = []

(* Sample table for variable lookup *)
let sample_table = [("x", Clos(N(7), empty_table));
                    ("flag", Clos(B(true), empty_table))]

(* Test 1: 2 + 3 => 5 *)
let test1 = Clos(Add(N(2), N(3)), empty_table)

(* Test 2: 10 - 4 => 6 *)
let test2 = Clos(Sub(N(10), N(4)), empty_table)

(* Test 3: true and false => false *)
let test3 = Clos(And(B(true), B(false)), empty_table)

(* Test 4: true or false => true *)
let test4 = Clos(Or(B(true), B(false)), empty_table)

(* Test 5: if true then 1 else 0 => 1 *)
let test5 = Clos(IfThenElse(B(true), N(1), N(0)), empty_table)

(* Test 6: if false then 1 else 0 => 0 *)
let test6 = Clos(IfThenElse(B(false), N(1), N(0)), empty_table)

(* Test 7: Pair (1, true) => (1, true) *)
let test7 = Clos(Pair(N(1), B(true)), empty_table)

(* Test 8: Variable lookup: x => 7 *)
let test8 = Clos(Var("x"), sample_table)

(* Test 9: Nested arithmetic: 2 + (5 - 3) => 4 *)
let test9 = Clos(Add(N(2), Sub(N(5), N(3))), empty_table)

(* --- Test Cases for Unload --- *)

(* Test U1: Unload a closure with no environment *)
let testU1 = Clos(Add(N(2), N(3)), empty_table)

(* Test U2: Unload a closure that contains a variable binding *)
let testU2 =
  let env = [("y", Clos(Sub(N(10), N(4)), empty_table))] in
  (* Expression: x + y, where y is bound in the environment to (10 - 4) *)
  Clos(Add(Var "y", N(5)), env)

(* Test U3: Unload a closure with a pair and variable lookup *)
let testU3 =
  let env = [("p", Clos(Pair(N(3), B(false)), empty_table))] in
  (* Expression: p, which should be substituted by the pair (3, false) *)
  Clos(Var "p", env)

(* --- Run All Tests --- *)

let () =
  (* Evaluation Tests *)
  run_eval_test "Eval Test 1 (2 + 3)" test1;
  run_eval_test "Eval Test 2 (10 - 4)" test2;
  run_eval_test "Eval Test 3 (true and false)" test3;
  run_eval_test "Eval Test 4 (true or false)" test4;
  run_eval_test "Eval Test 5 (if true then 1 else 0)" test5;
  run_eval_test "Eval Test 6 (if false then 1 else 0)" test6;
  run_eval_test "Eval Test 7 (Pair(1, true))" test7;
  run_eval_test "Eval Test 8 (Var x)" test8;
  run_eval_test "Eval Test 9 (2 + (5 - 3))" test9;

  print_endline "\n--- Unload Tests ---";
  run_unload_test "Unload Test U1 (Add(2, 3))" testU1;
  run_unload_test "Unload Test U2 (Add(Var y, 5) with y = 10-4)" testU2;
  run_unload_test "Unload Test U3 (Var p with p = Pair(3, false))" testU3
