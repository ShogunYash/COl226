open Ast
open Ast_type_checker

(* Runtime value types *)
type value =
  | VInt of int
  | VFloat of float
  | VBool of bool
  | VString of string
  | VIVector of int * int array
  | VFVector of int * float array
  | VIMatrix of int * int * int array array
  | VFMatrix of int * int * float array array
  | VUnit

(* Exception for runtime errors *)
exception Runtime_error of string

(* Environment for variable values *)
module StringMap = Map.Make(String)
type runtime_env = value StringMap.t

(* Empty environment *)
let empty_runtime_env = StringMap.empty

(* Pretty-print values *)
let rec string_of_value = function
  | VInt i -> string_of_int i
  | VFloat f -> string_of_float f
  | VBool b -> string_of_bool b
  | VString s -> "\"" ^ s ^ "\""
  | VIVector(n, arr) -> 
      let elements = Array.to_list arr |> List.map string_of_int |> String.concat ", " in
      Printf.sprintf "%d\n[%s]" n elements
  | VFVector(n, arr) -> 
      let elements = Array.to_list arr |> List.map string_of_float |> String.concat ", " in
      Printf.sprintf "%d\n[%s]" n elements
  | VIMatrix(r, c, mat) ->
      let rows = Array.to_list mat |> List.map (fun row ->
        let elements = Array.to_list row |> List.map string_of_int |> String.concat ", " in
        "[" ^ elements ^ "]"
      ) |> String.concat ", " in
      Printf.sprintf "%d,%d\n[%s]" r c rows
  | VFMatrix(r, c, mat) ->
      let rows = Array.to_list mat |> List.map (fun row ->
        let elements = Array.to_list row |> List.map string_of_float |> String.concat ", " in
        "[" ^ elements ^ "]"
      ) |> String.concat ", " in
      Printf.sprintf "%d,%d\n[%s]" r c rows
  | VUnit -> "()"

(* Convert AST literals to runtime values *)
let convert_ivector_lit (size, elems) =
  let arr = Array.make size 0 in
  List.iteri (fun i v -> if i < size then arr.(i) <- v) elems;
  VIVector(size, arr)

let convert_fvector_lit (size, elems) =
  let arr = Array.make size 0.0 in
  List.iteri (fun i v -> if i < size then arr.(i) <- v) elems;
  VFVector(size, arr)

let convert_imatrix_lit (rows, cols, data) =
  let mat = Array.make_matrix rows cols 0 in
  List.iteri (fun i row -> 
    if i < rows then
      List.iteri (fun j v -> 
        if j < cols then mat.(i).(j) <- v
      ) row
  ) data;
  VIMatrix(rows, cols, mat)

let convert_fmatrix_lit (rows, cols, data) =
  let mat = Array.make_matrix rows cols 0.0 in
  List.iteri (fun i row -> 
    if i < rows then
      List.iteri (fun j v -> 
        if j < cols then mat.(i).(j) <- v
      ) row
  ) data;
  VFMatrix(rows, cols, mat)

(* Evaluate an expression *)
let rec eval_expr env = function
  | Empty -> VUnit
  | Var name -> 
      (match StringMap.find_opt name env with
       | Some v -> v
       | None -> raise (Runtime_error ("Undefined variable: " ^ name)))
  
  (* Literals *)
  | IntLit i -> VInt i
  | FloatLit f -> VFloat f
  | BoolLit b -> VBool b
  | StringLit s -> VString s
  | IVectorLit(size, elems) -> convert_ivector_lit (size, elems)
  | FVectorLit(size, elems) -> convert_fvector_lit (size, elems)
  | IMatrixLit(rows, cols, data) -> convert_imatrix_lit (rows, cols, data)
  | FMatrixLit(rows, cols, data) -> convert_fmatrix_lit (rows, cols, data)
  
  (* Binary operations *)
  | BinOp(e1, op, e2) ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      (match op, v1, v2 with
        (* Integer arithmetic *)
        | IAdd, VInt a, VInt b -> VInt (a + b)
        | ISub, VInt a, VInt b -> VInt (a - b)
        | IMul, VInt a, VInt b -> VInt (a * b)
        | IDiv, VInt a, VInt b -> 
            if b = 0 then raise (Runtime_error "Division by zero")
            else VInt (a / b)
        | IMod, VInt a, VInt b -> 
            if b = 0 then raise (Runtime_error "Modulo by zero")
            else VInt (a mod b)
        | Power, VInt a, VInt b ->
            if b < 0 then raise (Runtime_error "Negative power for integer")
            else VInt (int_of_float (float_of_int a ** float_of_int b))
            
        (* Float arithmetic *)
        | FAdd, VFloat a, VFloat b -> VFloat (a +. b)
        | FSub, VFloat a, VFloat b -> VFloat (a -. b)
        | FMul, VFloat a, VFloat b -> VFloat (a *. b)
        | FDiv, VFloat a, VFloat b -> 
            if b = 0.0 then raise (Runtime_error "Division by zero")
            else VFloat (a /. b)
        | FMod, VFloat a, VFloat b -> 
            if b = 0.0 then raise (Runtime_error "Modulo by zero")
            else VFloat (mod_float a b)
        | Power, VFloat a, VFloat b -> VFloat (a ** b)
        | Power, VInt a, VFloat b -> VFloat ((float_of_int a) ** b)
        | Power, VFloat a, VInt b -> VFloat (a ** (float_of_int b))
        
        (* Integer vector operations *)
        | IAdd, VIVector(n1, arr1), VIVector(n2, arr2) ->
            if n1 <> n2 then raise (Runtime_error "Vector dimension mismatch")
            else
              let result = Array.make n1 0 in
              for i = 0 to n1 - 1 do
                result.(i) <- arr1.(i) + arr2.(i)
              done;
              VIVector(n1, result)
              
        | ISub, VIVector(n1, arr1), VIVector(n2, arr2) ->
            if n1 <> n2 then raise (Runtime_error "Vector dimension mismatch")
            else
              let result = Array.make n1 0 in
              for i = 0 to n1 - 1 do
                result.(i) <- arr1.(i) - arr2.(i)
              done;
              VIVector(n1, result)
              
        | IMul, VIVector(n1, arr1), VIVector(n2, arr2) -> (* Dot product *)
            if n1 <> n2 then raise (Runtime_error "Vector dimension mismatch")
            else
              let mutable dot = 0 in
              for i = 0 to n1 - 1 do
                dot <- dot + (arr1.(i) * arr2.(i))
              done;
              VInt dot
              
        | IMul, VIVector(n, arr), VInt scalar ->
            let result = Array.make n 0 in
            for i = 0 to n - 1 do
              result.(i) <- arr.(i) * scalar
            done;
            VIVector(n, result)
            
        | IMul, VInt scalar, VIVector(n, arr) ->
            let result = Array.make n 0 in
            for i = 0 to n - 1 do
              result.(i) <- scalar * arr.(i)
            done;
            VIVector(n, result)
            
        (* Float vector operations *)
        | FAdd, VFVector(n1, arr1), VFVector(n2, arr2) ->
            if n1 <> n2 then raise (Runtime_error "Vector dimension mismatch")
            else
              let result = Array.make n1 0.0 in
              for i = 0 to n1 - 1 do
                result.(i) <- arr1.(i) +. arr2.(i)
              done;
              VFVector(n1, result)
              
        | FSub, VFVector(n1, arr1), VFVector(n2, arr2) ->
            if n1 <> n2 then raise (Runtime_error "Vector dimension mismatch")
            else
              let result = Array.make n1 0.0 in
              for i = 0 to n1 - 1 do
                result.(i) <- arr1.(i) -. arr2.(i)
              done;
              VFVector(n1, result)
              
        | FMul, VFVector(n1, arr1), VFVector(n2, arr2) -> (* Dot product *)
            if n1 <> n2 then raise (Runtime_error "Vector dimension mismatch")
            else
              let mutable dot = 0.0 in
              for i = 0 to n1 - 1 do
                dot <- dot +. (arr1.(i) *. arr2.(i))
              done;
              VFloat dot
              
        | FMul, VFVector(n, arr), VFloat scalar ->
            let result = Array.make n 0.0 in
            for i = 0 to n - 1 do
              result.(i) <- arr.(i) *. scalar
            done;
            VFVector(n, result)
            
        | FMul, VFloat scalar, VFVector(n, arr) ->
            let result = Array.make n 0.0 in
            for i = 0 to n - 1 do
              result.(i) <- scalar *. arr.(i)
            done;
            VFVector(n, result)
            
        (* Integer Matrix operations *)
        | IAdd, VIMatrix(r1, c1, mat1), VIMatrix(r2, c2, mat2) ->
            if r1 <> r2 || c1 <> c2 then raise (Runtime_error "Matrix dimension mismatch")
            else
              let result = Array.make_matrix r1 c1 0 in
              for i = 0 to r1 - 1 do
                for j = 0 to c1 - 1 do
                  result.(i).(j) <- mat1.(i).(j) + mat2.(i).(j)
                done
              done;
              VIMatrix(r1, c1, result)
              
        | ISub, VIMatrix(r1, c1, mat1), VIMatrix(r2, c2, mat2) ->
            if r1 <> r2 || c1 <> c2 then raise (Runtime_error "Matrix dimension mismatch")
            else
              let result = Array.make_matrix r1 c1 0 in
              for i = 0 to r1 - 1 do
                for j = 0 to c1 - 1 do
                  result.(i).(j) <- mat1.(i).(j) - mat2.(i).(j)
                done
              done;
              VIMatrix(r1, c1, result)
              
        | IMul, VIMatrix(r1, c1, mat1), VIMatrix(r2, c2, mat2) ->
            if c1 <> r2 then raise (Runtime_error "Matrix multiplication dimension mismatch")
            else
              let result = Array.make_matrix r1 c2 0 in
              for i = 0 to r1 - 1 do
                for j = 0 to c2 - 1 do
                  let mutable sum = 0 in
                  for k = 0 to c1 - 1 do
                    sum <- sum + (mat1.(i).(k) * mat2.(k).(j))
                  done;
                  result.(i).(j) <- sum
                done
              done;
              VIMatrix(r1, c2, result)
              
        | IMul, VIMatrix(r, c, mat), VInt scalar ->
            let result = Array.make_matrix r c 0 in
            for i = 0 to r - 1 do
              for j = 0 to c - 1 do
                result.(i).(j) <- mat.(i).(j) * scalar
              done
            done;
            VIMatrix(r, c, result)
            
        | IMul, VInt scalar, VIMatrix(r, c, mat) ->
            let result = Array.make_matrix r c 0 in
            for i = 0 to r - 1 do
              for j = 0 to c - 1 do
                result.(i).(j) <- scalar * mat.(i).(j)
              done
            done;
            VIMatrix(r, c, result)
            
        (* Float Matrix operations *)
        | FAdd, VFMatrix(r1, c1, mat1), VFMatrix(r2, c2, mat2) ->
            if r1 <> r2 || c1 <> c2 then raise (Runtime_error "Matrix dimension mismatch")
            else
              let result = Array.make_matrix r1 c1 0.0 in
              for i = 0 to r1 - 1 do
                for j = 0 to c1 - 1 do
                  result.(i).(j) <- mat1.(i).(j) +. mat2.(i).(j)
                done
              done;
              VFMatrix(r1, c1, result)
              
        | FSub, VFMatrix(r1, c1, mat1), VFMatrix(r2, c2, mat2) ->
            if r1 <> r2 || c1 <> c2 then raise (Runtime_error "Matrix dimension mismatch")
            else
              let result = Array.make_matrix r1 c1 0.0 in
              for i = 0 to r1 - 1 do
                for j = 0 to c1 - 1 do
                  result.(i).(j) <- mat1.(i).(j) -. mat2.(i).(j)
                done
              done;
              VFMatrix(r1, c1, result)
              
        | FMul, VFMatrix(r1, c1, mat1), VFMatrix(r2, c2, mat2) ->
            if c1 <> r2 then raise (Runtime_error "Matrix multiplication dimension mismatch")
            else
              let result = Array.make_matrix r1 c2 0.0 in
              for i = 0 to r1 - 1 do
                for j = 0 to c2 - 1 do
                  let mutable sum = 0.0 in
                  for k = 0 to c1 - 1 do
                    sum <- sum +. (mat1.(i).(k) *. mat2.(k).(j))
                  done;
                  result.(i).(j) <- sum
                done
              done;
              VFMatrix(r1, c2, result)
              
        | FMul, VFMatrix(r, c, mat), VFloat scalar ->
            let result = Array.make_matrix r c 0.0 in
            for i = 0 to r - 1 do
              for j = 0 to c - 1 do
                result.(i).(j) <- mat.(i).(j) *. scalar
              done
            done;
            VFMatrix(r, c, result)
            
        | FMul, VFloat scalar, VFMatrix(r, c, mat) ->
            let result = Array.make_matrix r c 0.0 in
            for i = 0 to r - 1 do
              for j = 0 to c - 1 do
                result.(i).(j) <- scalar *. mat.(i).(j)
              done
            done;
            VFMatrix(r, c, result)

        (* Matrix-vector operations *)
        | IMul, VIMatrix(r, c, mat), VIVector(n, vec) ->
            if c <> n then raise (Runtime_error "Matrix-vector dimension mismatch")
            else
              let result = Array.make r 0 in
              for i = 0 to r - 1 do
                let mutable sum = 0 in
                for j = 0 to c - 1 do
                  sum <- sum + (mat.(i).(j) * vec.(j))
                done;
                result.(i) <- sum
              done;
              VIVector(r, result)
              
        | FMul, VFMatrix(r, c, mat), VFVector(n, vec) ->
            if c <> n then raise (Runtime_error "Matrix-vector dimension mismatch")
            else
              let result = Array.make r 0.0 in
              for i = 0 to r - 1 do
                let mutable sum = 0.0 in
                for j = 0 to c - 1 do
                  sum <- sum +. (mat.(i).(j) *. vec.(j))
                done;
                result.(i) <- sum
              done;
              VFVector(r, result)
            
        (* Comparison operators *)
        | Equal, v1, v2 -> VBool (v1 = v2)
        | NotEqual, v1, v2 -> VBool (v1 <> v2)
        | Less, VInt a, VInt b -> VBool (a < b)
        | Greater, VInt a, VInt b -> VBool (a > b)
        | LessEq, VInt a, VInt b -> VBool (a <= b)
        | GreaterEq, VInt a, VInt b -> VBool (a >= b)
        | Less, VFloat a, VFloat b -> VBool (a < b)
        | Greater, VFloat a, VFloat b -> VBool (a > b)
        | LessEq, VFloat a, VFloat b -> VBool (a <= b)
        | GreaterEq, VFloat a, VFloat b -> VBool (a >= b)
        
        (* Logical operators *)
        | And, VBool a, VBool b -> VBool (a && b)
        | Or, VBool a, VBool b -> VBool (a || b)
        | Xor, VBool a, VBool b -> VBool ((a || b) && not (a && b))
        
        (* Special vector operations *)
        | Angle, VIVector(n1, vec1), VIVector(n2, vec2) ->
            if n1 <> n2 then raise (Runtime_error "Vector dimension mismatch for angle")
            else 
              (* Compute angle between vectors using dot product formula:
                 cos(θ) = (v1·v2)/(|v1|*|v2|) *)
              let dot_product = ref 0 in
              let mag1_squared = ref 0 in
              let mag2_squared = ref 0 in
              
              for i = 0 to n1 - 1 do
                dot_product := !dot_product + (vec1.(i) * vec2.(i));
                mag1_squared := !mag1_squared + (vec1.(i) * vec1.(i));
                mag2_squared := !mag2_squared + (vec2.(i) * vec2.(i))
              done;
              
              let denominator = sqrt(float_of_int !mag1_squared) *. sqrt(float_of_int !mag2_squared) in
              if denominator = 0.0 then VFloat 0.0
              else VFloat (acos(float_of_int !dot_product /. denominator))
              
        | Angle, VFVector(n1, vec1), VFVector(n2, vec2) ->
            if n1 <> n2 then raise (Runtime_error "Vector dimension mismatch for angle")
            else
              let dot_product = ref 0.0 in
              let mag1_squared = ref 0.0 in
              let mag2_squared = ref 0.0 in
              
              for i = 0 to n1 - 1 do
                dot_product := !dot_product +. (vec1.(i) *. vec2.(i));
                mag1_squared := !mag1_squared +. (vec1.(i) *. vec1.(i));
                mag2_squared := !mag2_squared +. (vec2.(i) *. vec2.(i))
              done;
              
              let denominator = sqrt(!mag1_squared) *. sqrt(!mag2_squared) in
              if denominator = 0.0 then VFloat 0.0
              else VFloat (acos(!dot_product /. denominator))
              
        | _ -> raise (Runtime_error "Invalid binary operation")
      )
      
  (* Unary operations *)
  | UnOp(op, e) ->
      let v = eval_expr env e in
      (match op, v with
      | INeg, VInt i -> VInt (-i)
      | FNeg, VFloat f -> VFloat (-.f)
      | Not, VBool b -> VBool (not b)
      
      (* Type casting *)
      | I2F, VInt i -> VFloat (float_of_int i)
      
      | Abs, VInt i -> VInt (abs i)
      | Abs, VFloat f -> VFloat (abs_float f)
      
      | Transpose, VIMatrix(r, c, mat) ->
          let result = Array.make_matrix c r 0 in
          for i = 0 to r - 1 do
            for j = 0 to c - 1 do
              result.(j).(i) <- mat.(i).(j)
            done
          done;
          VIMatrix(c, r, result)
          
      | Transpose, VFMatrix(r, c, mat) ->
          let result = Array.make_matrix c r 0.0 in
          for i = 0 to r - 1 do
            for j = 0 to c - 1 do
              result.(j).(i) <- mat.(i).(j)
            done
          done;
          VFMatrix(c, r, result)
          
      | Det, VIMatrix(r, c, mat) ->
          if r <> c then raise (Runtime_error "Determinant requires a square matrix")
          else if r = 1 then VInt mat.(0).(0)
          else if r = 2 then 
            VInt (mat.(0).(0) * mat.(1).(1) - mat.(0).(1) * mat.(1).(0))
          else 
            (* For larger matrices we would need a more complex algorithm *)
            raise (Runtime_error "Determinant not implemented for matrices larger than 2x2")
            
      | Det, VFMatrix(r, c, mat) ->
          if r <> c then raise (Runtime_error "Determinant requires a square matrix")
          else if r = 1 then VFloat mat.(0).(0)
          else if r = 2 then 
            VFloat (mat.(0).(0) *. mat.(1).(1) -. mat.(0).(1) *. mat.(1).(0))
          else 
            raise (Runtime_error "Determinant not implemented for matrices larger than 2x2")
            
      | Dimension, VIVector(n, _) -> VInt n
      | Dimension, VFVector(n, _) -> VInt n
      | Dimension, VIMatrix(r, c, _) -> 
          let result = Array.make 2 0 in
          result.(0) <- r;
          result.(1) <- c;
          VIVector(2, result)
          
      | Dimension, VFMatrix(r, c, _) -> 
          let result = Array.make 2 0 in
          result.(0) <- r;
          result.(1) <- c;
          VIVector(2, result)
          
      | Magnitude, VIVector(n, vec) ->
          let mutable sum_of_squares = 0 in
          for i = 0 to n - 1 do
            sum_of_squares <- sum_of_squares + (vec.(i) * vec.(i))
          done;
          VFloat (sqrt(float_of_int sum_of_squares))
          
      | Magnitude, VFVector(n, vec) ->
          let mutable sum_of_squares = 0.0 in
          for i = 0 to n - 1 do
            sum_of_squares <- sum_of_squares +. (vec.(i) *. vec.(i))
          done;
          VFloat (sqrt(sum_of_squares))
          
      | _ -> raise (Runtime_error "Invalid unary operation")
      )
      
  (* Indexing *)
  | VectorIndex(vec_expr, idx_expr) ->
      let vec = eval_expr env vec_expr in
      let idx = eval_expr env idx_expr in
      (match vec, idx with
      | VIVector(n, arr), VInt i ->
          if i < 0 || i >= n then raise (Runtime_error "Vector index out of bounds")
          else VInt arr.(i)
          
      | VFVector(n, arr), VInt i ->
          if i < 0 || i >= n then raise (Runtime_error "Vector index out of bounds")
          else VFloat arr.(i)
          
      | _ -> raise (Runtime_error "Invalid vector indexing")
      )
      
  | MatrixIndex(mat_expr, row_expr, col_expr) ->
      let mat = eval_expr env mat_expr in
      let row = eval_expr env row_expr in
      let col = eval_expr env col_expr in
      (match mat, row, col with
      | VIMatrix(rows, cols, mat_arr), VInt r, VInt c ->
          if r < 0 || r >= rows || c < 0 || c >= cols then 
            raise (Runtime_error "Matrix index out of bounds")
          else VInt mat_arr.(r).(c)
          
      | VFMatrix(rows, cols, mat_arr), VInt r, VInt c ->
          if r < 0 || r >= rows || c < 0 || c >= cols then 
            raise (Runtime_error "Matrix index out of bounds")
          else VFloat mat_arr.(r).(c)
          
      | _ -> raise (Runtime_error "Invalid matrix indexing")
      )
      
  | RowAccess(mat_expr, row_expr) ->
      let mat = eval_expr env mat_expr in
      let row = eval_expr env row_expr in
      (match mat, row with
      | VIMatrix(rows, cols, mat_arr), VInt r ->
          if r < 0 || r >= rows then raise (Runtime_error "Row index out of bounds")
          else
            let result = Array.make cols 0 in
            for i = 0 to cols - 1 do
              result.(i) <- mat_arr.(r).(i)
            done;
            VIVector(cols, result)
            
      | VFMatrix(rows, cols, mat_arr), VInt r ->
          if r < 0 || r >= rows then raise (Runtime_error "Row index out of bounds")
          else
            let result = Array.make cols 0.0 in
            for i = 0 to cols - 1 do
              result.(i) <- mat_arr.(r).(i)
            done;
            VFVector(cols, result)
            
      | _ -> raise (Runtime_error "Invalid row access")
      )

(* Execute a statement *)
let rec exec_stmt env = function
  | ExprStmt e -> 
      let _ = eval_expr env e in
      env
  
  | AssignStmt(var, e) ->
      let v = eval_expr env e in
      StringMap.add var v env
  
  | IfStmt(cond, then_stmts, else_stmts) ->
      let condition = eval_expr env cond in
      (match condition with
      | VBool true -> exec_stmts env then_stmts
      | VBool false -> exec_stmts env else_stmts
      | _ -> raise (Runtime_error "If condition must evaluate to a boolean"))
  
  | ForStmt(var, start_expr, end_expr, body) ->
      let start_val = eval_expr env start_expr in
      let end_val = eval_expr env end_expr in
      
      (match start_val, end_val with
      | VInt start, VInt end_val ->
          let rec loop i env =
            if i > end_val then env
            else
              let loop_env = StringMap.add var (VInt i) env in
              let new_env = exec_stmts loop_env body in
              loop (i + 1) new_env
          in
          loop start env
      | _ -> raise (Runtime_error "For loop bounds must be integers"))
  
  | WhileStmt(cond, body) ->
      let rec loop env =
        match eval_expr env cond with
        | VBool true ->
            let new_env = exec_stmts env body in
            loop new_env
        | VBool false -> env
        | _ -> raise (Runtime_error "While condition must evaluate to a boolean")
      in
      loop env
  
  | InputStmt e ->
      (match eval_expr env e with
      | VUnit -> (* Simple input with no arguments *)
          print_string "Input: ";
          flush stdout;
          let input_str = read_line () in
          (* Just return the environment unchanged, input will be handled elsewhere *)
          env
      | VString filename -> (* Read from file *)
          (* In a real implementation, this would read from a file *)
          Printf.printf "Reading from file: %s\n" filename;
          env
      | _ -> raise (Runtime_error "Input argument must be a string (filename) or empty"))
  
  | PrintStmt e ->
      (match eval_expr env e with
      | VUnit -> print_newline (); env
      | v -> 
          print_endline (string_of_value v);
          env)

(* Execute a list of statements *)
and exec_stmts env = function
  | [] -> env
  | s :: rest ->
      let env' = exec_stmt env s in
      exec_stmts env' rest

(* Type check and execute a program *)
let interpret program =
  (* First type check the program *)
  let type_result = type_check program in
  if is_error_message type_result then
    Printf.printf "Type error: %s\n" type_result
  else
    (* If type checking passed, execute the program *)
    try
      match program with
      | Program stmts ->
          let final_env = exec_stmts empty_runtime_env stmts in
          Printf.printf "Program executed successfully.\n"
    with
    | Runtime_error msg -> Printf.printf "Runtime error: %s\n" msg
    | e -> Printf.printf "Unexpected error: %s\n" (Printexc.to_string e)

(* Helper function to determine if a message is an error *)
let is_error_message msg =
  String.length msg >= 10 && String.sub msg 0 10 = "Type error" ||
  String.length msg >= 6 && String.sub msg 0 6 = "Error:"
