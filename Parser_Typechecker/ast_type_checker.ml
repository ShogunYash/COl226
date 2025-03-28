open Ast

(* Define the Wrong exception that carries the problematic expression *)
exception Wrong of expr

(* Type definitions *)
type typ =
  | TInt
  | TFloat
  | TBool
  | TString
  | TIVector 
  | TFVector 
  | TIMatrix
  | TFMatrix 
  | TUnit

(* Type environment to track variable types *)
module StringMap = Map.Make(String)
type env = typ StringMap.t

(* Initial empty environment *)
let empty_env = StringMap.empty

(* Environment extension function *)
let extend env var typ = StringMap.add var typ env

(* Pretty-print types for error reporting *)
let string_of_type = function
  | TInt -> "int"
  | TFloat -> "float"
  | TBool -> "bool"
  | TString -> "string"
  | TIVector -> Printf.sprintf "int vector" 
  | TFVector -> Printf.sprintf "float vector" 
  | TIMatrix -> Printf.sprintf "int matrix" 
  | TFMatrix -> Printf.sprintf "float matrix" 
  | TUnit -> "unit"

(* Type-check an expression with exception-based error handling *)
let rec type_check_expr env = function
  | Empty -> TUnit
  | Var name as e -> 
      (match StringMap.find_opt name env with
       | Some t -> t
       | None -> raise (Wrong e))
  
  (* Literals *)
  | IntLit _ -> TInt
  | FloatLit _ -> TFloat
  | BoolLit _ -> TBool
  | StringLit _ -> TString
  | IVectorLit(_ , _) -> TIVector 
  | FVectorLit(_, _) -> TFVector 
  | IMatrixLit(_, _, _) -> TIMatrix
  | FMatrixLit(_, _, _) -> TFMatrix
  
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
        
        (* Integer vector operations *)
        | IAdd, TIVector , TIVector -> (TIVector)
        | ISub, TIVector , TIVector -> (TIVector)
        | IMul, TIVector , TIVector -> TInt  (* Dot product *)
        | IMul, TIVector , TInt -> (TIVector)  (* Scalar multiplication *)
        | IMul, TInt, TIVector -> (TIVector)
        
        (* Float vector operations *)
        | FAdd, TFVector , TFVector -> (TFVector)
        | FSub, TFVector , TFVector -> (TFVector)
        | FMul, TFVector , TFVector -> TFloat  (* Dot product *)
        | FMul, TFVector , TFloat -> (TFVector)  (* Scalar multiplication *)
        | FMul, TFloat, TFVector -> (TFVector)
        
        (* Integer Matrix operations *)
        | IAdd, TIMatrix, TIMatrix  -> (TIMatrix)
        | ISub, TIMatrix, TIMatrix -> (TIMatrix)
        | IMul, TIMatrix, TIMatrix -> (TIMatrix)
        | IMul, TIMatrix, TInt -> (TIMatrix)  (* Scalar multiplication *)
        | IMul, TInt, TIMatrix -> (TIMatrix)
        
        (* Float Matrix operations *)
        | FAdd, TFMatrix, TFMatrix  -> (TFMatrix)
        | FSub, TFMatrix, TFMatrix -> (TFMatrix)
        | FMul, TFMatrix, TFMatrix -> (TFMatrix)
        | FMul, TFMatrix, TFloat -> (TFMatrix)  (* Scalar multiplication *)
        | FMul, TFloat, TFMatrix -> (TFMatrix)

        (* Vector Matrix operations *)
        | IMul, TIMatrix, TIVector  -> (TIMatrix)  (* Matrix-vector multiplication *)
        | IMul, TIVector, TIMatrix  -> (TIMatrix)
        | FMul, TFMatrix, TFVector  -> (TFMatrix)  (* Matrix-vector multiplication *)
        | FMul, TFVector, TFMatrix  -> (TFMatrix)
        
        (* Comparison operators *)
        | Equal, t1, t2 when t1 = t2 -> TBool
        | NotEqual, t1, t2 when t1 = t2 -> TBool
        | Less, TInt, TInt | Greater, TInt, TInt 
        | LessEq, TInt, TInt | GreaterEq, TInt, TInt -> TBool
        | Less, TFloat, TFloat | Greater, TFloat, TFloat
        | LessEq, TFloat, TFloat | GreaterEq, TFloat, TFloat -> TBool
        
        (* Logical operators *)
        | And, TBool, TBool | Or, TBool, TBool | Xor, TBool, TBool -> TBool
        
        (* Special vector operations *)
        | Angle, TIVector , TIVector -> TFloat
        | Angle, TFVector , TFVector -> TFloat
      
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
      
      | Transpose, TIMatrix -> TIMatrix
      | Transpose, TFMatrix -> TFMatrix
      
      | Det, TIMatrix -> TInt  (* Determinant requires square matrix *)
      | Det, TFMatrix -> TFloat
      
      | Dimension, TIVector -> TInt
      | Dimension, TFVector -> TInt
      | Dimension, TIMatrix -> TIVector 
      | Dimension, TFMatrix -> TIVector 
      
      | Magnitude, TIVector -> TFloat
      | Magnitude, TFVector -> TFloat
      
      | _ -> raise (Wrong e)
  )
  (* Indexing *)
  | VectorIndex(vec, idx) as e ->(
      let tvec = type_check_expr env vec in
      let tidx = type_check_expr env idx in
      match tvec, tidx with
      | TIVector , TInt -> TInt
      | TFVector , TInt -> TFloat
      | _ -> raise (Wrong e)
    )
  
  | MatrixIndex(mat, row, col) as e ->(
      let tmat = type_check_expr env mat in
      let trow = type_check_expr env row in
      let tcol = type_check_expr env col in
      match tmat, trow, tcol with
      | TIMatrix , TInt, TInt -> TInt
      | TFMatrix , TInt, TInt -> TFloat
      | _ -> raise (Wrong e)
    )
  | RowAccess(mat, row) as e -> (
    let tmat = type_check_expr env mat in
    let trow = type_check_expr env row in
    match tmat, trow with 
    | TIMatrix , TInt -> TInt
    | TFMatrix , TInt -> TFloat
    | _ -> raise (Wrong e)
  )
  | IcrtVectorLit(expr) as e ->(
    match type_check_expr env expr with
    | TInt -> TIVector
    | _ -> raise (Wrong e)
    )
  | FcrtVectorLit(expr) as e ->(
    match type_check_expr env expr with
    | TInt -> TFVector
    | _ -> raise (Wrong e)
    )
  | IcrtMatrixLit(rows, cols) as e ->(
    match type_check_expr env rows, type_check_expr env cols with
    | TInt, TInt -> TIMatrix
    | _ -> raise (Wrong e)
    )
  | FcrtMatrixLit(rows, cols) as e ->(
    match type_check_expr env rows, type_check_expr env cols with
    | TInt, TInt -> TFMatrix
    | _ -> raise (Wrong e)
    )
(* Type-check a statement *)
let rec type_check_stmt env = function
  | ExprStmt e ->
      ignore (type_check_expr env e);
      env
  
  | AssignStmt(var, e) ->
      let typ = type_check_expr env e in
      extend env var typ
  
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
        let loop_env = extend env var TInt in
          let _ = type_check_stmts loop_env body in
          env
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
          (match StringMap.find_opt name env with
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
  | Wrong e -> Printf.sprintf "Type error in expression: %s" (string_of_expr e)
  | e -> Printf.sprintf "Error: %s" (Printexc.to_string e)

(* Main function to run the type checker *)
let run_type_checker ast =
  let result = type_check ast in
  Printf.printf "%s\n" result




(* ast_type_checker.ml - Type checker with rho as function argument

open Ast

(* Type definitions *)
type typ =
  | TInt
  | TFloat
  | TBool
  | TString
  | TIVector of int  (* size *)
  | TFVector of int  (* size *)
  | TIMatrix of int * int  (* rows, cols *)
  | TFMatrix of int * int  (* rows, cols *)
  | TUnit
  | TError of string

(* Environment type - a function from string to typ option *)
type env = string -> typ option

(* Create initial empty environment *)
let empty_env : env = fun _ -> None

(* Environment extension function *)
let extend (rho : env) (var : string) (typ : typ) : env =
  fun x -> if x = var then Some typ else rho x

(* Pretty-print types for error messages *)
let rec string_of_type = function
  | TInt -> "int"
  | TFloat -> "float"
  | TBool -> "bool"
  | TString -> "string"
  | TIVector n -> Printf.sprintf "int vector[%d]" n
  | TFVector n -> Printf.sprintf "float vector[%d]" n
  | TIMatrix(r,c) -> Printf.sprintf "int matrix[%d,%d]" r c
  | TFMatrix(r,c) -> Printf.sprintf "float matrix[%d,%d]" r c
  | TUnit -> "unit"
  | TError msg -> Printf.sprintf "ERROR: %s" msg

(* Type-check binary operations *)
let check_binary_op op t1 t2 =
  match op, t1, t2 with
  (* Integer arithmetic *)
  | IAdd, TInt, TInt -> TInt
  | ISub, TInt, TInt -> TInt
  | IMul, TInt, TInt -> TInt
  | IDiv, TInt, TInt -> TInt
  | IMod, TInt, TInt -> TInt
  
  (* Float arithmetic *)
  | FAdd, TFloat, TFloat -> TFloat
  | FSub, TFloat, TFloat -> TFloat
  | FMul, TFloat, TFloat -> TFloat
  | FDiv, TFloat, TFloat -> TFloat
  | FMod, TFloat, TFloat -> TFloat
  
  (* Integer vector operations *)
  | IAdd, TIVector _, TIVector n2 when _ = n2 -> (TIVector _)
  | ISub, TIVector _, TIVector n2 when _ = n2 -> (TIVector _)
  | IMul, TIVector _, TIVector n2 when _ = n2 -> TInt  (* Dot product *)
  | IMul, TIVector _, TInt -> (TIVector _)  (* Scalar multiplication *)
  | IMul, TInt, TIVector _ -> (TIVector _)
  
  (* Float vector operations *)
  | FAdd, TFVector _, TFVector n2 when _ = n2 -> (TFVector _)
  | FSub, TFVector _, TFVector n2 when _ = n2 -> (TFVector _)
  | FMul, TFVector _, TFVector n2 when _ = n2 -> TFloat  (* Dot product *)
  | FMul, TFVector _, TFloat -> (TFVector _)  (* Scalar multiplication *)
  | FMul, TFloat, TFVector _ -> (TFVector _)
  
  (* Integer Matrix operations *)
  | IAdd, TIMatrix(r1,c1), TIMatrix(r2,c2) when r1=r2 && c1=c2 -> (TIMatrix(r1,c1))
  | ISub, TIMatrix(r1,c1), TIMatrix(r2,c2) when r1=r2 && c1=c2 -> (TIMatrix(r1,c1))
  | IMul, TIMatrix(r1,c1), TIMatrix(r2,c2) when c1=r2 -> (TIMatrix(r1,c2))
  | IMul, TIMatrix(r,c), TInt -> (TIMatrix(r,c))  (* Scalar multiplication *)
  | IMul, TInt, TIMatrix(r,c) -> (TIMatrix(r,c))
  
  (* Float Matrix operations *)
  | FAdd, TFMatrix(r1,c1), TFMatrix(r2,c2) when r1=r2 && c1=c2 -> (TFMatrix(r1,c1))
  | FSub, TFMatrix(r1,c1), TFMatrix(r2,c2) when r1=r2 && c1=c2 -> (TFMatrix(r1,c1))
  | FMul, TFMatrix(r1,c1), TFMatrix(r2,c2) when c1=r2 -> (TFMatrix(r1,c2))
  | FMul, TFMatrix(r,c), TFloat -> (TFMatrix(r,c))  (* Scalar multiplication *)
  | FMul, TFloat, TFMatrix(r,c) -> (TFMatrix(r,c))

  (* Vector Matrix operations *)
  | IMul, TIMatrix(r,c), TIVector n when c=n -> (TIMatrix(r,1))  (* Matrix-vector multiplication *)
  | IMul, TIVector n, TIMatrix(r,c) when n=r -> (TIMatrix(1,c))
  | FMul, TFMatrix(r,c), TFVector n when c=n -> (TFMatrix(r,1))  (* Matrix-vector multiplication *)
  | FMul, TFVector n, TFMatrix(r,c) when n=r -> (TFMatrix(1,c))
  
  (* Comparison operators *)
  | Equal, t1, t2 when t1 = t2 -> TBool
  | NotEqual, t1, t2 when t1 = t2 -> TBool
  | Less, TInt, TInt | Greater, TInt, TInt 
  | LessEq, TInt, TInt | GreaterEq, TInt, TInt -> TBool
  | Less, TFloat, TFloat | Greater, TFloat, TFloat
  | LessEq, TFloat, TFloat | GreaterEq, TFloat, TFloat -> TBool
  
  (* Logical operators *)
  | And, TBool, TBool | Or, TBool, TBool | Xor, TBool, TBool -> TBool
  
  (* Special vector operations *)
  | Angle, TIVector _, TIVector n2 when _ = n2 -> TFloat
  | Angle, TFVector _, TFVector n2 when _ = n2 -> TFloat
  
  | _ -> Error (Printf.sprintf "Type error: cannot apply operator %s to %s and %s" 
                (match op with
                 | IAdd -> "+" | ISub -> "/" | IMul -> "*" | IDiv -> "/" | IMod -> "mod"
                 | FAdd -> "+." | FSub -> "-." | FMul -> "*." | FDiv -> "/." | FMod -> "mod_float"
                 | Equal -> "=" | NotEqual -> "~=" | Less -> "<" | Greater -> ">" 
                 | LessEq -> "<=" | GreaterEq -> ">=" | And -> "&&" | Or -> "||" 
                 | Xor -> "^" | Angle -> "angle")
                (string_of_type t1) (string_of_type t2))

(* Type-check unary operations *)
let check_unary_op op t =
  match op, t with
  | INeg, TInt -> TInt
  | FNeg, TFloat -> TFloat
  | Not, TBool -> TBool
  
  | Abs, TInt -> TInt
  | Abs, TFloat -> TFloat
  
  | Transpose, TIMatrix(r,c) -> (TIMatrix(c,r))
  | Transpose, TFMatrix(r,c) -> (TFMatrix(c,r))
  
  | Det, TIMatrix(r,c) when r=c -> TInt  (* Determinant requires square matrix *)
  | Det, TFMatrix(r,c) when r=c -> TFloat
  
  | Dimension, TIVector _ | Dimension, TFVector _ -> TInt
  | Dimension, TIMatrix _ | Dimension, TFMatrix _ -> (TIVector 2)
  
  | Magnitude, TIVector _ | Magnitude, TFVector _ -> TFloat
  
  | _ -> Error (Printf.sprintf "Type error: cannot apply operator %s to %s"
                (match op with
                 | INeg -> "-" | FNeg -> "-." | Not -> "~" | Abs -> "abs"
                 | Transpose -> "'" | Det -> "det" | Dimension -> "dim" | Magnitude -> "mag")
                (string_of_type t))

(* Type-check an expression *)
let rec type_check_expr rho = function
  | Empty -> TUnit
  | Var name -> 
      (match rho name with
      | Some t -> t
      | None -> Error (Printf.sprintf "Undefined variable: %s" name))
  
  (* Literals *)
  | IntLit _ -> TInt
  | FloatLit _ -> TFloat
  | BoolLit _ -> TBool
  | StringLit _ -> TString
  | IVectorLit(size, _) -> (TIVector size)
  | FVectorLit(size, _) -> (TFVector size)
  | IMatrixLit(rows, cols, _) -> (TIMatrix(rows, cols))
  | FMatrixLit(rows, cols, _) -> (TFMatrix(rows, cols))
  
  (* Operations *)
  | BinOp(e1, op, e2) ->
      (match type_check_expr rho e1, type_check_expr rho e2 with
       | t1, t2 -> check_binary_op op t1 t2
       | Error e, _ -> Error e
       | _, Error e -> Error e)
  
  | UnOp(op, e) ->
      (match type_check_expr rho e with
       | t -> check_unary_op op t
       | Error e -> Error e)
  
  (* Indexing *)
  | VectorIndex(vec, idx) ->
      (match type_check_expr rho vec, type_check_expr rho idx with
       | (TIVector _), TInt -> TInt
       | (TFVector _), TInt -> TFloat
       | t, TInt -> Error (Printf.sprintf "Expected vector, got %s" (string_of_type t))
       | _, t -> Error (Printf.sprintf "Vector index must be int, got %s" (string_of_type t))
       | Error e, _ | _, Error e -> Error e)
  
  | MatrixIndex(mat, row, col) ->
      (match type_check_expr rho mat, type_check_expr rho row, type_check_expr rho col with
       | (TIMatrix _), TInt, TInt -> TInt
       | (TFMatrix _), TInt, TInt -> TFloat
       | t, TInt, TInt -> Error (Printf.sprintf "Expected matrix, got %s" (string_of_type t))
       | _, t, _ when t <> TInt -> Error "Matrix row index must be int"
       | _, _, t when t <> TInt -> Error "Matrix column index must be int"
       | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e)

(* Type-check a statement *)
let rec type_check_stmt rho = function
  | ExprStmt e ->
      Result.map (fun _ -> rho) (type_check_expr rho e)
  
  | AssignStmt(var, e) ->
      (match type_check_expr rho e with
       | typ -> (extend rho var typ)
       | Error e -> Error e)
  
  | IfStmt(cond, then_branch, else_branch) ->
      (match type_check_expr rho cond with
       | TBool -> 
           (match type_check_stmts rho then_branch with
            | _ when else_branch = [] -> rho
            | _ -> type_check_stmts rho else_branch
            | Error e -> Error e)
       | t -> Error (Printf.sprintf "Condition must be boolean, got %s" (string_of_type t))
       | Error e -> Error e)
  
  | ForStmt(var, start, finish, body) ->
      (match type_check_expr rho start, type_check_expr rho finish with
       | TInt, TInt ->
           let loop_rho = extend rho var TInt in
           (match type_check_stmts loop_rho body with
            | _ -> rho  (* Restore original rho *)
            | Error e -> Error e)
       | t1, TInt when t1 <> TInt -> 
           Error (Printf.sprintf "Loop start expression must be int, got %s" (string_of_type t1))
       | TInt, t2 when t2 <> TInt ->
           Error (Printf.sprintf "Loop end expression must be int, got %s" (string_of_type t2))
       | Error e, _ | _, Error e -> Error e)
  
  | WhileStmt(cond, body) ->
      (match type_check_expr rho cond with
       | TBool -> 
           (match type_check_stmts rho body with
            | _ -> rho
            | Error e -> Error e)
       | t -> Error (Printf.sprintf "Condition must be boolean, got %s" (string_of_type t))
       | Error e -> Error e)
  
  | InputStmt(var) ->
     (extend rho var TString)  (* Assume input reads strings *)
  
  | PrintStmt(e) ->
      Result.map (fun _ -> rho) (type_check_expr rho e)
  
  | Block(stmts) ->
      type_check_stmts rho stmts

(* Type-check a list of statements *)
and type_check_stmts rho = function
  | [] -> rho
  | s :: rest ->
      (match type_check_stmt rho s with
       | rho' -> type_check_stmts rho' rest
       | Error e -> Error e)

(* Type-check a program *)
let type_check (Program stmts) =
  match type_check_stmts empty_env stmts with
  | _ -> "Program type checks successfully"
  | Error msg -> Error msg

(* Test the type checker on a program *)
let run_type_checker ast =
  match type_check ast with
  | msg -> Printf.printf "Success: %s\n" msg
  | Error msg -> Printf.printf "Type Error: %s\n" msg *)
