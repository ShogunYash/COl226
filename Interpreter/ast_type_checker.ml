open Ast

(* Define the Wrong exception that carries the problematic expression *)
exception Wrong of expr

(* Type definitions *)
type typ =
  | TInt
  | TFloat
  | TBool
  | TString
  | TIVector of int
  | TFVector of int
  | TIMatrix of int * int
  | TFMatrix of int * int
  | TUnit

(* Type environment to track variable types and declarations *)
module StringMap = Map.Make(String)
type env = {
  types: typ StringMap.t;
  declared: bool StringMap.t; (* Track whether a variable has been declared *)
}

(* Initial empty environment *)
let empty_env = {
  types = StringMap.empty;
  declared = StringMap.empty;
}

(* Environment operations *)
let lookup_type env var =
  match StringMap.find_opt var env.types with
  | Some t -> t
  | None -> raise (Wrong (Var var))

let declare_var env var typ =
  (* Check if variable is already declared *)
  if StringMap.mem var env.declared then
    raise (Wrong (Var var))
  else
    { types = StringMap.add var typ env.types;
      declared = StringMap.add var true env.declared }

let remove_var env var =
  { types = StringMap.remove var env.types;
    declared = StringMap.remove var env.declared }

(* Pretty-print types for error reporting *)
let string_of_type = function
  | TInt -> "int"
  | TFloat -> "float"
  | TBool -> "bool"
  | TString -> "string"
  | TIVector n -> Printf.sprintf "int vector[%d]" n
  | TFVector n -> Printf.sprintf "float vector[%d]" n
  | TIMatrix(r, c) -> Printf.sprintf "int matrix[%d,%d]" r c
  | TFMatrix(r, c) -> Printf.sprintf "float matrix[%d,%d]" r c
  | TUnit -> "unit"

(* Type-check an expression with exception-based error handling *)
let rec type_check_expr env = function
  | Empty -> TUnit
  | Var name -> lookup_type env name
  
  (* Literals *)
  | IntLit _ -> TInt
  | FloatLit _ -> TFloat
  | BoolLit _ -> TBool
  | StringLit _ -> TString
  | IVectorLit(n, _) -> TIVector n
  | FVectorLit(n, _) -> TFVector n
  | IMatrixLit(n, m, _) -> TIMatrix(n, m)
  | FMatrixLit(n, m, _) -> TFMatrix(n, m)
  
  (* Binary operations *)
  | BinOp(e1, op, e2) as e ->(
      let t1 = type_check_expr env e1 in
      let t2 = type_check_expr env e2 in
      match op, t1, t2 with
        (* Integer arithmetic *)
        | IAdd, TInt, TInt -> TInt
        | ISub, TInt, TInt -> TInt
        | IMul, TInt, TInt -> TInt
        | IDiv, TInt, TInt -> TInt
        | IMod, TInt, TInt -> TInt
        | Power, TInt, TInt -> TInt

        (* Float arithmetic *)
        | FAdd, TFloat, TFloat -> TFloat
        | FSub, TFloat, TFloat -> TFloat
        | FMul, TFloat, TFloat -> TFloat
        | FDiv, TFloat, TFloat -> TFloat
        | FMod, TFloat, TFloat -> TFloat
        | Power, TFloat, TFloat -> TFloat

        (* Hybrid for power *)
        | Power, TInt, TFloat -> TFloat
        | Power, TFloat, TInt -> TFloat
        
        (* Integer vector operations with dimension checks *)
        | IAdd, TIVector n1, TIVector n2 when n1 = n2 -> TIVector n1
        | ISub, TIVector n1, TIVector n2 when n1 = n2 -> TIVector n1
        | IMul, TIVector n1, TIVector n2 when n1 = n2 -> TInt  (* Dot product *)
        | IMul, TIVector n, TInt -> TIVector n  (* Scalar multiplication *)
        | IMul, TInt, TIVector n -> TIVector n
        
        (* Float vector operations with dimension checks *)
        | FAdd, TFVector n1, TFVector n2 when n1 = n2 -> TFVector n1
        | FSub, TFVector n1, TFVector n2 when n1 = n2 -> TFVector n1
        | FMul, TFVector n1, TFVector n2 when n1 = n2 -> TFloat  (* Dot product *)
        | FMul, TFVector n, TFloat -> TFVector n  (* Scalar multiplication *)
        | FMul, TFloat, TFVector n -> TFVector n
        
        (* Integer Matrix operations with dimension checks *)
        | IAdd, TIMatrix(r1, c1), TIMatrix(r2, c2) when r1 = r2 && c1 = c2 -> TIMatrix(r1, c1)
        | ISub, TIMatrix(r1, c1), TIMatrix(r2, c2) when r1 = r2 && c1 = c2 -> TIMatrix(r1, c1)
        | IMul, TIMatrix(r1, c1), TIMatrix(r2, c2) when c1 = r2 -> TIMatrix(r1, c2)
        | IMul, TIMatrix(r, c), TInt -> TIMatrix(r, c)  (* Scalar multiplication *)
        | IMul, TInt, TIMatrix(r, c) -> TIMatrix(r, c)
        
        (* Float Matrix operations with dimension checks *)
        | FAdd, TFMatrix(r1, c1), TFMatrix(r2, c2) when r1 = r2 && c1 = c2 -> TFMatrix(r1, c1)
        | FSub, TFMatrix(r1, c1), TFMatrix(r2, c2) when r1 = r2 && c1 = c2 -> TFMatrix(r1, c1)
        | FMul, TFMatrix(r1, c1), TFMatrix(r2, c2) when c1 = r2 -> TFMatrix(r1, c2)
        | FMul, TFMatrix(r, c), TFloat -> TFMatrix(r, c)  (* Scalar multiplication *)
        | FMul, TFloat, TFMatrix(r, c) -> TFMatrix(r, c)

        (* Vector Matrix operations with dimension checks *)
        (*  Vector are treated as n cross 1 matrix and changed to matrix as final answer *)
        | IMul, TIMatrix(r, c), TIVector n when c = n -> TIMatrix (r,1) (* Matrix-vector multiplication *)
        | IMul, TIVector n, TIMatrix(r, c) when 1 = r -> TIMatrix (n,c)
        | FMul, TFMatrix(r, c), TFVector n when c = n -> TFMatrix (r,1)  (* Matrix-vector multiplication *)
        | FMul, TFVector n, TFMatrix(r, c) when 1 = r -> TFMatrix (n,c)
        
        (* Comparison operators *)
        | Equal, t1, t2 when t1 = t2 -> TBool
        | NotEqual, t1, t2 when t1 = t2 -> TBool
        | Less, TInt, TInt | Greater, TInt, TInt 
        | LessEq, TInt, TInt | GreaterEq, TInt, TInt -> TBool
        | Less, TFloat, TFloat | Greater, TFloat, TFloat
        | LessEq, TFloat, TFloat | GreaterEq, TFloat, TFloat -> TBool
        
        (* Logical operators *)
        | And, TBool, TBool | Or, TBool, TBool | Xor, TBool, TBool -> TBool
        
        (* Special vector operations with dimension checks *)
        | Angle, TIVector n1, TIVector n2 when n1 = n2 -> TFloat
        | Angle, TFVector n1, TFVector n2 when n1 = n2 -> TFloat
      
        | _ -> raise (Wrong e)
  )
  (* Unary operations *)
  | UnOp(op, expr) as e ->( 
      let t = type_check_expr env expr in
      match op, t with
      | INeg, TInt -> TInt
      | FNeg, TFloat -> TFloat
      | Not, TBool -> TBool
      
      (* Type casting from int to float *)
      | I2F, TInt -> TFloat
      
      | Abs, TInt -> TInt
      | Abs, TFloat -> TFloat
      
      | Transpose, TIMatrix(r, c) -> TIMatrix(c, r)
      | Transpose, TFMatrix(r, c) -> TFMatrix(c, r)
      
      | Det, TIMatrix(r, c) when r = c -> TInt  (* Determinant requires square matrix *)
      | Det, TFMatrix(r, c) when r = c -> TFloat
      
      | Dimension, TIVector n -> TInt
      | Dimension, TFVector n -> TInt
      | Dimension, TIMatrix(r, c) -> TIVector 2 
      | Dimension, TFMatrix(r, c) -> TIVector 2
      
      | Magnitude, TIVector _ -> TFloat
      | Magnitude, TFVector _ -> TFloat
      
      | _ -> raise (Wrong e)
  )
  (* Indexing with dimension checks *)
  | VectorIndex(vec, idx) as e ->(
      let tvec = type_check_expr env vec in
      let tidx = type_check_expr env idx in
      match tvec, tidx with
      | TIVector n, TInt -> TInt
      | TFVector n, TInt -> TFloat
      | _ -> raise (Wrong e)
    )
  
  | MatrixIndex(mat, row, col) as e ->(
      let tmat = type_check_expr env mat in
      let trow = type_check_expr env row in
      let tcol = type_check_expr env col in
      match tmat, trow, tcol with
      | TIMatrix(r, c), TInt, TInt -> TInt
      | TFMatrix(r, c), TInt, TInt -> TFloat
      | _ -> raise (Wrong e)
    )
  | RowAccess(mat, row) as e -> (
    let tmat = type_check_expr env mat in
    let trow = type_check_expr env row in
    match tmat, trow with 
    | TIMatrix(r, c), TInt -> TIVector c
    | TFMatrix(r, c), TInt -> TFVector c
    | _ -> raise (Wrong e)
  )

(* Type-check a statement *)
let rec type_check_stmt env = function
  | ExprStmt e ->
      ignore (type_check_expr env e);
      env
  
  | DeclareStmt(var, e) ->
      (* For let declaration, check if variable exists *)
      let typ = type_check_expr env e in
      if StringMap.mem var env.declared then
        raise (Wrong (Var var)) (* Variable already declared - error *)
      else
        declare_var env var typ
  
  | AssignStmt(var, e) ->
      (* For assignment, check that variable exists and types match *)
      let new_type = type_check_expr env e in
      if not (StringMap.mem var env.declared) then
        raise (Wrong (Var var)) (* Variable not declared *)
      else
        let current_type = lookup_type env var in
        if current_type = new_type then
          env (* Types match, keep the environment unchanged *)
        else
          raise (Wrong e) (* Type mismatch in assignment *)
  
  | IfStmt(cond, then_branch, else_branch) ->
      let tcond = type_check_expr env cond in
      if tcond = TBool then
          let _ = type_check_stmts env then_branch in
          if else_branch = [] then env
          else type_check_stmts env else_branch
      else
        raise (Wrong cond)
  
  | ForStmt(var, start, finish, body) ->
      let tstart = type_check_expr env start in
      let tfinish = type_check_expr env finish in
      if tstart = TInt && tfinish = TInt then
        (* Check if loop variable already exists in outer scope *)
        if StringMap.mem var env.declared then
          raise (Wrong (Var var)) (* Cannot reuse an existing variable as loop counter *)
        else
          (* Create a new scope with the loop variable *)
          let loop_env = declare_var env var TInt in
          let _ = type_check_stmts loop_env body in
          (* Discard the loop variable after the loop ends *)
          remove_var env var
      else
        raise (Wrong (if tstart <> TInt then start else finish))
  
  | WhileStmt(cond, body) ->
      let tcond = type_check_expr env cond in
      if tcond = TBool then
        let _ = type_check_stmts env body in
        env
      else
        raise (Wrong cond)

  | InputStmt(e) ->
    (match e with
      | Empty -> 
          (* Input() with no args reads from stdin *)
          env
      | StringLit(_) -> 
          (* Input("filename") reads from a file *)
          env
      | Var(name) ->
          (* Input(var) where var is a variable containing a filename *)
          (match StringMap.find_opt name env.types with
          | Some TString -> env  (* Variable should contain a string (filename) *)
          | _ -> raise (Wrong e))
      | _ -> raise (Wrong e)
  )  (* Input arg must be a filename or Empty *)
  
  | PrintStmt(e) ->
    (match e with
    | Empty -> env
    | _ -> 
        (* For Print(expr), check that expr is well-typed *)
        ignore (type_check_expr env e);
        env)

(* Type-check a list of statements *)
and type_check_stmts env = function
  | [] -> env
  | s :: rest ->
      let env' = type_check_stmt env s in
      type_check_stmts env' rest

(* Type-check a program *)
let type_check (Program stmts) =
  try
    let _ = type_check_stmts empty_env stmts in
    "Program type checks successfully"
  with
  | Wrong (Var name) -> 
      (* Special case for variable errors *)
      if StringMap.mem name empty_env.declared then
        Printf.sprintf "Type error: Variable '%s' is already declared" name
      else
        Printf.sprintf "Type error: Undefined variable '%s'" name
  | Wrong e -> 
      Printf.sprintf "Type error in expression: %s" (string_of_expr e)
  | e -> 
      Printf.sprintf "Error: %s" (Printexc.to_string e)

(* Main function to run the type checker *)
let run_type_checker ast =
  let result = type_check ast in
  Printf.printf "%s\n" result