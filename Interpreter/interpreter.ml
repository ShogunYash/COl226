open Ast
open Ast_type_checker

(* Runtime value types *)
type value =
  | VInt of int
  | VFloat of float
  | VBool of bool
  | VString of string
  | VIVector of int * int list      (* Changed from array to list *)
  | VFVector of int * float list    (* Changed from array to list *)
  | VIMatrix of int * int * int list list    (* Changed from array array to list list *)
  | VFMatrix of int * int * float list list  (* Changed from array array to list list *)
  | VUnit

(* Exception for runtime errors *)
exception Runtime_error of string

(* Environment for variable values *)
module StringMap = Map.Make(String)
type runtime_env = {
  values: value StringMap.t;
  declared: bool StringMap.t;  (* Track whether a variable has been declared *)
}

(* Empty environment *)
let empty_runtime_env = {
  values = StringMap.empty;
  declared = StringMap.empty;
}

(* Environment operations *)
let lookup_var env name =
  match StringMap.find_opt name env.values with
  | Some v -> v
  | None -> raise (Runtime_error ("Undefined variable: " ^ name))

let declare_var env name value =
  (* Check if variable already exists *)
  if StringMap.mem name env.declared then
    raise (Runtime_error ("Variable '" ^ name ^ "' is already declared. Re-declaration is not allowed."))
  else
    { values = StringMap.add name value env.values;
      declared = StringMap.add name true env.declared }

let update_var env name value =
  if StringMap.mem name env.declared then
    { env with values = StringMap.add name value env.values }
  else
    raise (Runtime_error ("Cannot assign to undeclared variable: " ^ name))

(* Pretty-print values *)
let rec string_of_value = function
  | VInt i -> string_of_int i
  | VFloat f -> string_of_float f
  | VBool b -> string_of_bool b
  | VString s -> "\"" ^ s ^ "\""
  | VIVector(n, lst) -> 
      let elements = List.map string_of_int lst |> String.concat ", " in
      Printf.sprintf "%d\n[%s]" n elements
  | VFVector(n, lst) -> 
      let elements = List.map string_of_float lst |> String.concat ", " in
      Printf.sprintf "%d\n[%s]" n elements
  | VIMatrix(r, c, mat) ->
      let rows = List.map (fun row ->
        let elements = List.map string_of_int row |> String.concat ", " in
        "[" ^ elements ^ "]"
      ) mat |> String.concat ", " in
      Printf.sprintf "%d,%d\n[%s]" r c rows
  | VFMatrix(r, c, mat) ->
      let rows = List.map (fun row ->
        let elements = List.map string_of_float row |> String.concat ", " in
        "[" ^ elements ^ "]"
      ) mat |> String.concat ", " in
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
  | Var name -> lookup_var env name
  
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
        | IAdd, VIVector(n1, lst1), VIVector(n2, lst2) ->
            if n1 <> n2 then raise (Runtime_error "Vector dimension mismatch")
            else
              let rec add_vectors v1 v2 =
                match v1, v2 with
                | [], [] -> []
                | x::xs, y::ys -> (x + y) :: add_vectors xs ys
                | _, _ -> raise (Runtime_error "Vector dimension mismatch")
              in
              VIVector(n1, add_vectors lst1 lst2)
              
        | ISub, VIVector(n1, lst1), VIVector(n2, lst2) ->
            if n1 <> n2 then raise (Runtime_error "Vector dimension mismatch")
            else
              let rec sub_vectors v1 v2 =
                match v1, v2 with
                | [], [] -> []
                | x::xs, y::ys -> (x - y) :: sub_vectors xs ys
                | _, _ -> raise (Runtime_error "Vector dimension mismatch")
              in
              VIVector(n1, sub_vectors lst1 lst2)
              
        | IMul, VIVector(n1, lst1), VIVector(n2, lst2) -> (* Dot product *)
            if n1 <> n2 then raise (Runtime_error "Vector dimension mismatch")
            else
              let rec dot_product v1 v2 acc =
                match v1, v2 with
                | [], [] -> acc
                | x::xs, y::ys -> dot_product xs ys (acc + (x * y))
                | _, _ -> raise (Runtime_error "Vector dimension mismatch")
              in
              VInt (dot_product lst1 lst2 0)
              
        | IMul, VIVector(n, lst), VInt scalar ->
            let rec scalar_mul lst scalar =
              match lst with
              | [] -> []
              | x::xs -> (x * scalar) :: scalar_mul xs scalar
            in
            VIVector(n, scalar_mul lst scalar)
            
        | IMul, VInt scalar, VIVector(n, lst) ->
            let rec scalar_mul lst scalar =
              match lst with
              | [] -> []
              | x::xs -> (scalar * x) :: scalar_mul xs scalar
            in
            VIVector(n, scalar_mul lst scalar)
            
        (* Float vector operations *)
        | FAdd, VFVector(n1, lst1), VFVector(n2, lst2) ->
            if n1 <> n2 then raise (Runtime_error "Vector dimension mismatch")
            else
              let rec add_vectors v1 v2 =
                match v1, v2 with
                | [], [] -> []
                | x::xs, y::ys -> (x +. y) :: add_vectors xs ys
                | _, _ -> raise (Runtime_error "Vector dimension mismatch")
              in
              VFVector(n1, add_vectors lst1 lst2)
              
        | FSub, VFVector(n1, lst1), VFVector(n2, lst2) ->
            if n1 <> n2 then raise (Runtime_error "Vector dimension mismatch")
            else
              let rec sub_vectors v1 v2 =
                match v1, v2 with
                | [], [] -> []
                | x::xs, y::ys -> (x -. y) :: sub_vectors xs ys
                | _, _ -> raise (Runtime_error "Vector dimension mismatch")
              in
              VFVector(n1, sub_vectors lst1 lst2)
              
        | FMul, VFVector(n1, lst1), VFVector(n2, lst2) -> (* Dot product *)
            if n1 <> n2 then raise (Runtime_error "Vector dimension mismatch")
            else
              let rec dot_product v1 v2 acc =
                match v1, v2 with
                | [], [] -> acc
                | x::xs, y::ys -> dot_product xs ys (acc +. (x *. y))
                | _, _ -> raise (Runtime_error "Vector dimension mismatch")
              in
              VFloat (dot_product lst1 lst2 0.0)
              
        | FMul, VFVector(n, lst), VFloat scalar ->
            let rec scalar_mul lst scalar =
              match lst with
              | [] -> []
              | x::xs -> (x *. scalar) :: scalar_mul xs scalar
            in
            VFVector(n, scalar_mul lst scalar)
            
        | FMul, VFloat scalar, VFVector(n, lst) ->
            let rec scalar_mul lst scalar =
              match lst with
              | [] -> []
              | x::xs -> (scalar *. x) :: scalar_mul xs scalar
            in
            VFVector(n, scalar_mul lst scalar)
            
        (* Integer Matrix operations *)
        | IAdd, VIMatrix(r1, c1, mat1), VIMatrix(r2, c2, mat2) ->
            if r1 <> r2 || c1 <> c2 then raise (Runtime_error "Matrix dimension mismatch")
            else
              let rec add_rows row1 row2 =
                match row1, row2 with
                | [], [] -> []
                | x::xs, y::ys -> (x + y) :: add_rows xs ys
                | _, _ -> raise (Runtime_error "Matrix dimension mismatch")
              in
              
              let rec add_matrices m1 m2 =
                match m1, m2 with
                | [], [] -> []
                | row1::rest1, row2::rest2 -> (add_rows row1 row2) :: add_matrices rest1 rest2
                | _, _ -> raise (Runtime_error "Matrix dimension mismatch")
              in
              
              VIMatrix(r1, c1, add_matrices mat1 mat2)
              
        | ISub, VIMatrix(r1, c1, mat1), VIMatrix(r2, c2, mat2) ->
            if r1 <> r2 || c1 <> c2 then raise (Runtime_error "Matrix dimension mismatch")
            else
              let rec sub_rows row1 row2 =
                match row1, row2 with
                | [], [] -> []
                | x::xs, y::ys -> (x - y) :: sub_rows xs ys
                | _, _ -> raise (Runtime_error "Matrix dimension mismatch")
              in
              
              let rec sub_matrices m1 m2 =
                match m1, m2 with
                | [], [] -> []
                | row1::rest1, row2::rest2 -> (sub_rows row1 row2) :: sub_matrices rest1 rest2
                | _, _ -> raise (Runtime_error "Matrix dimension mismatch")
              in
              
              VIMatrix(r1, c1, sub_matrices mat1 mat2)
              
        | IMul, VIMatrix(r1, c1, mat1), VIMatrix(r2, c2, mat2) ->
            if c1 <> r2 then raise (Runtime_error "Matrix multiplication dimension mismatch")
            else
              (* Get column j from matrix *)
              let get_column mat j =
                List.map (fun row -> 
                  let rec get_jth_elem lst index =
                    match lst with
                    | [] -> raise (Runtime_error "Column index out of bounds")
                    | x::xs -> if index = 0 then x else get_jth_elem xs (index-1)
                  in
                  get_jth_elem row j
                ) mat
              in
              
              (* Dot product of two lists *)
              let dot_product lst1 lst2 =
                let rec dot lst1 lst2 acc =
                  match lst1, lst2 with
                  | [], [] -> acc
                  | x::xs, y::ys -> dot xs ys (acc + (x * y))
                  | _, _ -> raise (Runtime_error "Vector dimension mismatch in dot product")
                in
                dot lst1 lst2 0
              in
              
              (* Compute result matrix *)
              let rec compute_result_matrix i acc =
                if i = r1 then List.rev acc
                else
                  (* Get row i from mat1 *)
                  let row_i = 
                    let rec get_ith_row mat index =
                      match mat with
                      | [] -> raise (Runtime_error "Row index out of bounds")
                      | x::xs -> if index = 0 then x else get_ith_row xs (index-1)
                    in
                    get_ith_row mat1 i
                  in
                  
                  (* Compute row i of result *)
                  let rec compute_row j row_acc =
                    if j = c2 then List.rev row_acc
                    else
                      let col_j = get_column mat2 j in
                      compute_row (j+1) ((dot_product row_i col_j)::row_acc)
                  in
                  
                  let result_row = compute_row 0 [] in
                  compute_result_matrix (i+1) (result_row::acc)
              in
              
              VIMatrix(r1, c2, compute_result_matrix 0 [])
              
        | IMul, VIMatrix(r, c, mat), VInt scalar ->
            let rec scalar_mul_row row scalar =
              match row with
              | [] -> []
              | x::xs -> (x * scalar) :: scalar_mul_row xs scalar
            in
            
            let rec scalar_mul_matrix mat scalar =
              match mat with
              | [] -> []
              | row::rest -> (scalar_mul_row row scalar) :: scalar_mul_matrix rest scalar
            in
            
            VIMatrix(r, c, scalar_mul_matrix mat scalar)
            
        | IMul, VInt scalar, VIMatrix(r, c, mat) ->
            let rec scalar_mul_row row scalar =
              match row with
              | [] -> []
              | x::xs -> (scalar * x) :: scalar_mul_row xs scalar
            in
            
            let rec scalar_mul_matrix mat scalar =
              match mat with
              | [] -> []
              | row::rest -> (scalar_mul_row row scalar) :: scalar_mul_matrix rest scalar
            in
            
            VIMatrix(r, c, scalar_mul_matrix mat scalar)
            
        (* Float Matrix operations *)
        | FAdd, VFMatrix(r1, c1, mat1), VFMatrix(r2, c2, mat2) ->
            if r1 <> r2 || c1 <> c2 then raise (Runtime_error "Matrix dimension mismatch")
            else
              let rec add_rows row1 row2 =
                match row1, row2 with
                | [], [] -> []
                | x::xs, y::ys -> (x +. y) :: add_rows xs ys
                | _, _ -> raise (Runtime_error "Matrix dimension mismatch")
              in
              
              let rec add_matrices m1 m2 =
                match m1, m2 with
                | [], [] -> []
                | row1::rest1, row2::rest2 -> (add_rows row1 row2) :: add_matrices rest1 rest2
                | _, _ -> raise (Runtime_error "Matrix dimension mismatch")
              in
              
              VFMatrix(r1, c1, add_matrices mat1 mat2)
              
        | FSub, VFMatrix(r1, c1, mat1), VFMatrix(r2, c2, mat2) ->
            if r1 <> r2 || c1 <> c2 then raise (Runtime_error "Matrix dimension mismatch")
            else
              let rec sub_rows row1 row2 =
                match row1, row2 with
                | [], [] -> []
                | x::xs, y::ys -> (x -. y) :: sub_rows xs ys
                | _, _ -> raise (Runtime_error "Matrix dimension mismatch")
              in
              
              let rec sub_matrices m1 m2 =
                match m1, m2 with
                | [], [] -> []
                | row1::rest1, row2::rest2 -> (sub_rows row1 row2) :: sub_matrices rest1 rest2
                | _, _ -> raise (Runtime_error "Matrix dimension mismatch")
              in
              
              VFMatrix(r1, c1, sub_matrices mat1 mat2)
              
        | FMul, VFMatrix(r1, c1, mat1), VFMatrix(r2, c2, mat2) ->
            if c1 <> r2 then raise (Runtime_error "Matrix multiplication dimension mismatch")
            else
              (* Get column j from matrix *)
              let get_column mat j =
                List.map (fun row -> 
                  let rec get_jth_elem lst index =
                    match lst with
                    | [] -> raise (Runtime_error "Column index out of bounds")
                    | x::xs -> if index = 0 then x else get_jth_elem xs (index-1)
                  in
                  get_jth_elem row j
                ) mat
              in
              
              (* Dot product of two lists *)
              let dot_product lst1 lst2 =
                let rec dot lst1 lst2 acc =
                  match lst1, lst2 with
                  | [], [] -> acc
                  | x::xs, y::ys -> dot xs ys (acc +. (x *. y))
                  | _, _ -> raise (Runtime_error "Vector dimension mismatch in dot product")
                in
                dot lst1 lst2 0.0
              in
              
              (* Compute result matrix *)
              let rec compute_result_matrix i acc =
                if i = r1 then List.rev acc
                else
                  (* Get row i from mat1 *)
                  let row_i = 
                    let rec get_ith_row mat index =
                      match mat with
                      | [] -> raise (Runtime_error "Row index out of bounds")
                      | x::xs -> if index = 0 then x else get_ith_row xs (index-1)
                    in
                    get_ith_row mat1 i
                  in
                  
                  (* Compute row i of result *)
                  let rec compute_row j row_acc =
                    if j = c2 then List.rev row_acc
                    else
                      let col_j = get_column mat2 j in
                      compute_row (j+1) ((dot_product row_i col_j)::row_acc)
                  in
                  
                  let result_row = compute_row 0 [] in
                  compute_result_matrix (i+1) (result_row::acc)
              in
              
              VFMatrix(r1, c2, compute_result_matrix 0 [])
              
        | FMul, VFMatrix(r, c, mat), VFloat scalar ->
            let rec scalar_mul_row row scalar =
              match row with
              | [] -> []
              | x::xs -> (x *. scalar) :: scalar_mul_row xs scalar
            in
            
            let rec scalar_mul_matrix mat scalar =
              match mat with
              | [] -> []
              | row::rest -> (scalar_mul_row row scalar) :: scalar_mul_matrix rest scalar
            in
            
            VFMatrix(r, c, scalar_mul_matrix mat scalar)
            
        | FMul, VFloat scalar, VFMatrix(r, c, mat) ->
            let rec scalar_mul_row row scalar =
              match row with
              | [] -> []
              | x::xs -> (scalar *. x) :: scalar_mul_row xs scalar
            in
            
            let rec scalar_mul_matrix mat scalar =
              match mat with
              | [] -> []
              | row::rest -> (scalar_mul_row row scalar) :: scalar_mul_matrix rest scalar
            in
            
            VFMatrix(r, c, scalar_mul_matrix mat scalar)

        (* Matrix-vector operations *)
        | IMul, VIMatrix(r, c, mat), VIVector(n, vec) ->
            if c <> n then raise (Runtime_error "Matrix-vector dimension mismatch")
            else
              (* Dot product of two lists *)
              let rec dot_product v1 v2 acc =
                match v1, v2 with
                | [], [] -> acc
                | x::xs, y::ys -> dot_product xs ys (acc + (x * y))
                | _, _ -> raise (Runtime_error "Vector dimension mismatch in dot product")
              in
              
              (* For each row of the matrix, compute the dot product with the vector *)
              let result = List.map (fun row -> dot_product row vec 0) mat in
              
              VIVector(r, result)
              
        | FMul, VFMatrix(r, c, mat), VFVector(n, vec) ->
            if c <> n then raise (Runtime_error "Matrix-vector dimension mismatch")
            else
              (* Dot product of two lists *)
              let rec dot_product v1 v2 acc =
                match v1, v2 with
                | [], [] -> acc
                | x::xs, y::ys -> dot_product xs ys (acc +. (x *. y))
                | _, _ -> raise (Runtime_error "Vector dimension mismatch in dot product")
              in
              
              (* For each row of the matrix, compute the dot product with the vector *)
              let result = List.map (fun row -> dot_product row vec 0.0) mat in
              
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
              
              let rec compute_values v1 v2 dot mag1 mag2 =
                match v1, v2 with
                | [], [] -> (dot, mag1, mag2)
                | x::xs, y::ys ->
                    let new_dot = dot + (x * y) in
                    let new_mag1 = mag1 + (x * x) in
                    let new_mag2 = mag2 + (y * y) in
                    compute_values xs ys new_dot new_mag1 new_mag2
                | _, _ -> raise (Runtime_error "Vector dimension mismatch")
              in
              
              let (dot, mag1_sq, mag2_sq) = compute_values vec1 vec2 0 0 0 in
              
              let denominator = sqrt(float_of_int mag1_sq) *. sqrt(float_of_int mag2_sq) in
              if denominator = 0.0 then VFloat 0.0
              else VFloat (acos(float_of_int dot /. denominator))
              
        | Angle, VFVector(n1, vec1), VFVector(n2, vec2) ->
            if n1 <> n2 then raise (Runtime_error "Vector dimension mismatch for angle")
            else
              let rec compute_values v1 v2 dot mag1 mag2 =
                match v1, v2 with
                | [], [] -> (dot, mag1, mag2)
                | x::xs, y::ys ->
                    let new_dot = dot +. (x *. y) in
                    let new_mag1 = mag1 +. (x *. x) in
                    let new_mag2 = mag2 +. (y *. y) in
                    compute_values xs ys new_dot new_mag1 new_mag2
                | _, _ -> raise (Runtime_error "Vector dimension mismatch")
              in
              
              let (dot, mag1_sq, mag2_sq) = compute_values vec1 vec2 0.0 0.0 0.0 in
              
              let denominator = sqrt(mag1_sq) *. sqrt(mag2_sq) in
              if denominator = 0.0 then VFloat 0.0
              else VFloat (acos(dot /. denominator))
              
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
          (* Get column j from matrix *)
          let get_column mat j =
            List.map (fun row -> 
              let rec get_jth_elem lst index =
                match lst with
                | [] -> raise (Runtime_error "Column index out of bounds")
                | x::xs -> if index = 0 then x else get_jth_elem xs (index-1)
              in
              get_jth_elem row j
            ) mat
          in
          
          (* Create transpose matrix by getting each column *)
          let rec build_transpose j acc =
            if j >= c then List.rev acc
            else build_transpose (j+1) ((get_column mat j)::acc)
          in
          
          VIMatrix(c, r, build_transpose 0 [])
          
      | Transpose, VFMatrix(r, c, mat) ->
          (* Get column j from matrix *)
          let get_column mat j =
            List.map (fun row -> 
              let rec get_jth_elem lst index =
                match lst with
                | [] -> raise (Runtime_error "Column index out of bounds")
                | x::xs -> if index = 0 then x else get_jth_elem xs (index-1)
              in
              get_jth_elem row j
            ) mat
          in
          
          (* Create transpose matrix by getting each column *)
          let rec build_transpose j acc =
            if j >= c then List.rev acc
            else build_transpose (j+1) ((get_column mat j)::acc)
          in
          
          VFMatrix(c, r, build_transpose 0 [])
          
      | Det, VIMatrix(r, c, mat) ->
          if r <> c then raise (Runtime_error "Determinant requires a square matrix")
          else if r = 1 then 
            match mat with
            | [row] -> 
                (match row with
                 | [value] -> VInt value
                 | _ -> raise (Runtime_error "Invalid matrix format"))
            | _ -> raise (Runtime_error "Invalid matrix format")
          else if r = 2 then
            match mat with
            | [row1; row2] -> 
                (match row1, row2 with
                 | [a; b], [c; d] -> VInt (a * d - b * c)
                 | _, _ -> raise (Runtime_error "Invalid matrix format"))
            | _ -> raise (Runtime_error "Invalid matrix format")
          else 
            (* For larger matrices we would need a more complex algorithm *)
            raise (Runtime_error "Determinant not implemented for matrices larger than 2x2")
            
      | Det, VFMatrix(r, c, mat) ->
          if r <> c then raise (Runtime_error "Determinant requires a square matrix")
          else if r = 1 then
            match mat with
            | [row] -> 
                (match row with
                 | [value] -> VFloat value
                 | _ -> raise (Runtime_error "Invalid matrix format"))
            | _ -> raise (Runtime_error "Invalid matrix format")
          else if r = 2 then
            match mat with
            | [row1; row2] -> 
                (match row1, row2 with
                 | [a; b], [c; d] -> VFloat (a *. d -. b *. c)
                 | _, _ -> raise (Runtime_error "Invalid matrix format"))
            | _ -> raise (Runtime_error "Invalid matrix format")
          else 
            raise (Runtime_error "Determinant not implemented for matrices larger than 2x2")
            
      | Dimension, VIVector(n, _) -> VInt n
      | Dimension, VFVector(n, _) -> VInt n
      | Dimension, VIMatrix(r, c, _) -> 
          VIVector(2, [r; c])
          
      | Dimension, VFMatrix(r, c, _) -> 
          VIVector(2, [r; c])
          
      | Magnitude, VIVector(n, vec) ->
          let rec sum_squares lst acc =
            match lst with
            | [] -> acc
            | x::xs -> sum_squares xs (acc + (x * x))
          in
          let sum_of_squares = sum_squares vec 0 in
          VFloat (sqrt(float_of_int sum_of_squares))
          
      | Magnitude, VFVector(n, vec) ->
          let rec sum_squares lst acc =
            match lst with
            | [] -> acc
            | x::xs -> sum_squares xs (acc +. (x *. x))
          in
          let sum_of_squares = sum_squares vec 0.0 in
          VFloat (sqrt(sum_of_squares))
          
      | _ -> raise (Runtime_error "Invalid unary operation")
      )
      
  (* Indexing *)
  | VectorIndex(vec_expr, idx_expr) ->
      let vec = eval_expr env vec_expr in
      let idx = eval_expr env idx_expr in
      (match vec, idx with
      | VIVector(n, lst), VInt i ->
          if i < 0 || i >= n then raise (Runtime_error "Vector index out of bounds")
          else 
            let rec get_nth lst n =
              match lst with
              | [] -> raise (Runtime_error "Vector index out of bounds")
              | x::xs -> if n = 0 then x else get_nth xs (n-1)
            in
            VInt (get_nth lst i)
          
      | VFVector(n, lst), VInt i ->
          if i < 0 || i >= n then raise (Runtime_error "Vector index out of bounds")
          else
            let rec get_nth lst n =
              match lst with
              | [] -> raise (Runtime_error "Vector index out of bounds")
              | x::xs -> if n = 0 then x else get_nth xs (n-1)
            in
            VFloat (get_nth lst i)
          
      | _ -> raise (Runtime_error "Invalid vector indexing")
      )
      
  | MatrixIndex(mat_expr, row_expr, col_expr) ->
      let mat = eval_expr env mat_expr in
      let row = eval_expr env row_expr in
      let col = eval_expr env col_expr in
      (match mat, row, col with
      | VIMatrix(rows, cols, mat_lst), VInt r, VInt c ->
          if r < 0 || r >= rows || c < 0 || c >= cols then 
            raise (Runtime_error "Matrix index out of bounds")
          else 
            let rec get_row lst row_idx =
              match lst with
              | [] -> raise (Runtime_error "Matrix row index out of bounds")
              | x::xs -> if row_idx = 0 then x else get_row xs (row_idx-1)
            in
            let row_lst = get_row mat_lst r in
            
            let rec get_col lst col_idx =
              match lst with
              | [] -> raise (Runtime_error "Matrix column index out of bounds")
              | x::xs -> if col_idx = 0 then x else get_col xs (col_idx-1)
            in
            
            VInt (get_col row_lst c)
          
      | VFMatrix(rows, cols, mat_lst), VInt r, VInt c ->
          if r < 0 || r >= rows || c < 0 || c >= cols then 
            raise (Runtime_error "Matrix index out of bounds")
          else
            let rec get_row lst row_idx =
              match lst with
              | [] -> raise (Runtime_error "Matrix row index out of bounds")
              | x::xs -> if row_idx = 0 then x else get_row xs (row_idx-1)
            in
            let row_lst = get_row mat_lst r in
            
            let rec get_col lst col_idx =
              match lst with
              | [] -> raise (Runtime_error "Matrix column index out of bounds")
              | x::xs -> if col_idx = 0 then x else get_col xs (col_idx-1)
            in
            
            VFloat (get_col row_lst c)
          
      | _ -> raise (Runtime_error "Invalid matrix indexing")
      )
      
  | RowAccess(mat_expr, row_expr) ->
      let mat = eval_expr env mat_expr in
      let row = eval_expr env row_expr in
      (match mat, row with
      | VIMatrix(rows, cols, mat_lst), VInt r ->
          if r < 0 || r >= rows then raise (Runtime_error "Row index out of bounds")
          else
            let rec get_row lst row_idx =
              match lst with
              | [] -> raise (Runtime_error "Matrix row index out of bounds")
              | x::xs -> if row_idx = 0 then x else get_row xs (row_idx-1)
            in
            let row_lst = get_row mat_lst r in
            VIVector(cols, row_lst)
            
      | VFMatrix(rows, cols, mat_lst), VInt r ->
          if r < 0 || r >= rows then raise (Runtime_error "Row index out of bounds")
          else
            let rec get_row lst row_idx =
              match lst with
              | [] -> raise (Runtime_error "Matrix row index out of bounds")
              | x::xs -> if row_idx = 0 then x else get_row xs (row_idx-1)
            in
            let row_lst = get_row mat_lst r in
            VFVector(cols, row_lst)
            
      | _ -> raise (Runtime_error "Invalid row access")
      )

(* Execute a statement *)
let rec exec_stmt env = function
  | ExprStmt e -> 
      let _ = eval_expr env e in
      env
  
  | DeclareStmt(var, e) ->
      (* For let declaration, variable must not exist *)
      let v = eval_expr env e in
      if StringMap.mem var env.declared then
        raise (Runtime_error ("Variable '" ^ var ^ "' is already declared. Use regular assignment to update."))
      else
        declare_var env var v
      
  | AssignStmt(var, e) ->
      (* For assignment, variable must exist and types must match *)
      let v = eval_expr env e in
      if not (StringMap.mem var env.declared) then
        raise (Runtime_error ("Cannot assign to undeclared variable: '" ^ var ^ "'. Use 'let' to declare first."))
      else
        update_var env var v
  
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
          (* Execute the loop with temporary iterator variable *)
          let rec loop i loop_env =
            if i > end_val then loop_env
            else
              (* Create a new loop environment for each iteration, with the iterator variable *)
              let iter_env = {
                values = StringMap.add var (VInt i) loop_env.values;
                (* Note: We track the iterator in loop scope but not main scope *)
                declared = StringMap.add var true loop_env.declared;
              } in
              let new_env = exec_stmts iter_env body in
              loop (i + 1) new_env
          in
          
          (* Start looping with a copy of our current environment *)
          let final_loop_env = loop start env in
          
          (* Return the loop environment but remove the iterator variable *)
          {
            values = StringMap.remove var final_loop_env.values;
            declared = StringMap.remove var final_loop_env.declared;
          }
          
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
    | Runtime_error msg -> 
        Printf.printf "Runtime error: %s\n" msg;
        exit 1  (* Exit with error code *)
    | e -> 
        Printf.printf "Unexpected error: %s\n" (Printexc.to_string e);
        exit 1  (* Exit with error code *)

(* Helper function to determine if a message is an error *)
let is_error_message msg =
  String.length msg >= 10 && String.sub msg 0 10 = "Type error" ||
  String.length msg >= 6 && String.sub msg 0 6 = "Error:"
