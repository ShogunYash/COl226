(* Types for terms *)
type term =
  | Var of string
  | Abs of string * term
  | App of term * term
  | N of int
  | B of bool
  | Plus of term * term
  | Mult of term * term
  | And of term * term
  | Or of term * term
  | Not of term
  | Ifthenelse of term * term * term
  | Cmp of term * term

(* Types for values *)
type value =
  | closure of term * environment
  | Nval of int
  | Bval of bool

(* Environment is association list: variable name -> value *)
and environment = (string * value) list

(* Krivine Machine *)

let rec krivine_machine (t : term) (env : environment) : value =
  match t with
  | Var x -> List.assoc x env
  | Abs (x, t1) -> closure (Abs (x, t1), env)
  | App (t1, t2) ->
      let v1 = krivine_machine t1 env in
      let v2 = krivine_machine t2 env in
      (match v1 with
       | closure (Abs (x, tbody), env_closure) ->
           krivine_machine tbody ((x, v2) :: env_closure)
       | _ -> failwith "Application to non-closure")
  | N n -> Nval n
  | B b -> Bval b
  | Plus (t1, t2) ->
      let Nval n1 = krivine_machine t1 env in
      let Nval n2 = krivine_machine t2 env in
      Nval (n1 + n2)
  | Mult (t1, t2) ->
      let Nval n1 = krivine_machine t1 env in
      let Nval n2 = krivine_machine t2 env in
      Nval (n1 * n2)
  | And (t1, t2) ->
      let Bval b1 = krivine_machine t1 env in
      let Bval b2 = krivine_machine t2 env in
      Bval (b1 && b2)
  | Or (t1, t2) ->
      let Bval b1 = krivine_machine t1 env in
      let Bval b2 = krivine_machine t2 env in
      Bval (b1 || b2)
  | Not t1 ->
      let Bval b1 = krivine_machine t1 env in
      Bval (not b1)
  | Ifthenelse (tcond, tthen, telse) ->
      let Bval cond = krivine_machine tcond env in
      if cond then krivine_machine tthen env else krivine_machine telse env
  | Cmp (t1, t2) ->
      let Nval n1 = krivine_machine t1 env in
      let Nval n2 = krivine_machine t2 env in
      Bval (n1 = n2)

(* Top level call *)
let eval t = krivine_machine t []
