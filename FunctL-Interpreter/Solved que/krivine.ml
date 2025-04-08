(* Define the abstract syntax for expressions *)
type exp =
  | V of string           (* Variable *)
  | Abs of string * exp   (* Abstraction: λx.e *)
  | App of exp * exp      (* Application: (e1 e2) *)

(* Define the opcodes for the SECD machine *)
type opcode =
  | LOOKUP of string      (* Lookup a variable *)
  | APP                   (* Function application *)
  | MKCLOS of string * opcode list  (* Create a closure with formal parameter and code *)
  | RET                   (* Return from a function call *)

(* The compile function converts an expression to its opcode sequence *)
let rec compile (e : exp) : opcode list =
  match e with
  | V x ->
      [LOOKUP x]  (* compile variable x as a LOOKUP instruction *)
  | Abs (x, e_body) ->
      (* compile a function abstraction:
         - compile the body (e_body), then append RET to mark the end,
         - and finally pack it with the parameter x using MKCLOS *)
      [MKCLOS (x, (compile e_body) @ [RET])]
  | App (e1, e2) ->
      (* compile an application by:
         - compiling the operator e1,
         - then the operand e2,
         - followed by the APP opcode *)
      (compile e1) @ (compile e2) @ [APP]

(* Example usage: *)
(*
   The expression ( (λx. x) y ) is represented as:
     App (Abs ("x", V "x"), V "y")
   Its compiled opcode sequence should reflect:
     compile (Abs ("x", V "x")) @ compile (V "y") @ [APP]
   i.e.,
     [MKCLOS("x", [LOOKUP "x"; RET])] @ [LOOKUP "y"] @ [APP]
*)

(* To test the compiler, you can print or inspect the compiled opcode list.
   For example:
   
   let test_expr = App (Abs ("x", V "x"), V "y") ;;
   let compiled = compile test_expr ;;
   (* Now inspect `compiled` in the OCaml toplevel *)
*)
