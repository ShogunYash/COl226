type vector = float list;;

type types = 
| Bool  
| Scalar  
| Vector of int (* Represents an n-dimensional vector *)
;;

type expr =  
| T | F  (* Boolean constants *)
| ConstS of float  (* Scalar constants *)
| ConstV of float list  (* Vector constants *)
| Add of expr * expr  (* Overloaded: Boolean OR, scalar addition, vector addition *)
| Inv of expr  (* Overloaded: Boolean negation, scalar negation, vector negation *)
| ScalProd of expr * expr  (* Overloaded: Boolean AND, scalar multiplication, scalar-vector multiplication *)
| DotProd of expr * expr  (* Dot product of two vectors *)
| Mag of expr  (* Overloaded: Absolute value of a scalar or magnitude of a vector *)
| Angle of expr * expr  (* Angle between two vectors *)
| IsZero of expr  (* Overloaded: Zero-check for boolean, scalar, or vector *)
| Cond of expr * expr * expr  (* Conditional (if-then-else) *)
;;

type values = 
| B of bool  (* Boolean value *)
| S of float  (* Scalar value *)
| V of float list  (* Vector value *)
;;

exception Wrong of expr;;

module Vectors = struct
  exception DimensionError of string
  
  let create n x : vector =
    let rec aux n acc =
      if n < 1 then raise (DimensionError "Dimension must >=1")
      else if n = 1 then x :: acc
      else aux (n - 1) (x :: acc)
    in
    aux n []

  let dim (v: vector)  =
  if v = [] then raise (Wrong (ConstV v))
  else
    let rec aux v acc =
      match v with
      | [] -> acc
      | _ :: t -> aux t (acc + 1)
    in
    aux v 0
    
  let is_zero (v: vector) =
    if v = [] then raise (Wrong (IsZero (ConstV v)))
    else
    let rec aux v =
      match v with
      | [] -> true
      | h :: t -> if h = 0.0 then aux t else false
    in
    aux v
  
  let rec reverse ( v : vector) acc : vector =
    match v with
    | [] -> acc
    | h :: t -> reverse t (h :: acc)

  let unit n j : vector =
    if n < 1 || j < 1 || j > n then raise (DimensionError "Invalid dimensions or index")
    else
      let rec aux counter acc =
        if counter > n then reverse acc []  (* Reversing the list to maintain the order *)
        else if counter = j then aux (counter + 1) (1.0 :: acc)
        else aux (counter + 1) (0.0 :: acc)
      in
      aux 1 []

  let rec map f (v : vector) (acc : vector) : vector = match v with
    | [] -> reverse acc []
    | h :: t -> map f t (f h :: acc)

  let scale c (v : vector) : vector = 
    if v = [] then raise (Wrong (ScalProd( ConstS c , ConstV v)))
    else
      map (fun x -> c *. x) v []

  let addv (v1:vector) (v2: vector) : vector =
    if v1 = [] || v2 = [] then raise (Wrong (Add( ConstV v1 , ConstV v2)))
    else
      let rec aux v1 v2 acc =
        match v1, v2 with
        | [], [] -> reverse acc []
        | h1 :: t1, h2 :: t2 -> aux t1 t2 ((h1 +. h2) :: acc)
        | _ -> raise (Wrong (Add( ConstV v1 , ConstV v2))) (* To handle unexpected mismatched lengths *)
      in
      aux v1 v2 []  

  let dot_prod (v1:vector) (v2: vector) =
    if v1 = [] || v2 = [] then raise (Wrong (DotProd( ConstV v1 , ConstV v2)))
    else
      let rec aux v1 v2 acc =
        match v1, v2 with
        | [], [] -> acc
        | h1 :: t1, h2 :: t2 -> aux t1 t2 (acc +. (h1 *. h2))
        | _ -> raise (Wrong (DotProd( ConstV v1 , ConstV v2))) (* To handle unexpected mismatched lengths *)
      in
      aux v1 v2 0.0
      
  let inv (v : vector) : vector =
    if v = [] then raise (Wrong (Inv( ConstV v)))
    else
      let rec aux v acc =
        match v with
        | [] -> List.rev acc
        | h :: t -> aux t ((-1.0 *. h) :: acc)
      in
      aux v []
      
  let length v = sqrt (dot_prod v v)

  let angle v1 v2 =
    let dot = dot_prod v1 v2 in
    let len1 = length v1 in
    let len2 = length v2 in
    if len1 = 0.0 || len2 = 0.0 then raise (DimensionError "Cannot compute angle with zero-length vector")
    else 
      let cos_theta = dot /. (len1 *. len2) in
      let cos_theta = max (-1.0) (min 1.0 cos_theta) in  (* Clamp to [-1, 1] *)
      acos cos_theta
end

open Vectors

let rec type_of (e: expr) : types = match e with
  | T | F -> Bool  
  | ConstS _ -> Scalar  
  | ConstV v -> Vector (dim v)  
  | Add (e1, e2) -> (
      match (type_of e1, type_of e2) with
      | (Bool, Bool) -> Bool  
      | (Scalar, Scalar) -> Scalar  
      | (Vector n1, Vector n2) -> 
          if n1 = n2 then Vector n1 
          else raise (Wrong (Add (e1, e2)))  (* Raising Wrong with the expression that caused the issue *)
      | _ -> raise (Wrong (Add (e1, e2)))  (* Raising Wrong for type mismatches *)
    )    
  | Inv e1 -> (
      match type_of e1 with
      | Bool -> Bool  
      | Scalar -> Scalar  
      | Vector n -> Vector n  
    )
  | ScalProd (e1, e2) -> (
      match (type_of e1, type_of e2) with
      | (Bool, Bool) -> Bool  
      | (Scalar, Scalar) -> Scalar  
      | (Scalar, Vector n) -> Vector n  
      | _ -> raise (Wrong (ScalProd(e1, e2)))
    )

  | DotProd (e1, e2) -> (
      match (type_of e1, type_of e2) with
      | (Vector n1, Vector n2) when n1 = n2 -> Scalar
      | _ -> raise (Wrong (DotProd(e1, e2)))
    )

  | Mag e1 -> (
      match type_of e1 with
      | Scalar -> Scalar  
      | Vector _ -> Scalar 
      | _ -> raise (Wrong (Mag e1))
    )

  | Angle (e1, e2) -> (
      match (type_of e1, type_of e2) with
      | (Vector n1, Vector n2) when n1 = n2 -> Scalar
      | _ -> raise (Wrong (Angle(e1, e2)))
    )

  | IsZero e1 -> (
      match type_of e1 with
      | Bool -> Bool  
      | Scalar -> Bool  
      | Vector _ -> Bool  
    )

  | Cond (e1, e2, e3) -> (
      match (type_of e1, type_of e2, type_of e3) with
      | (Bool, t2, t3) when t2 = t3 -> t2  
      | _ -> raise (Wrong (Cond(e1,e2,e3)))
    )
;;

let rec eval (e: expr) : values = match e with
  | T -> B true
  | F -> B false
  | ConstS x -> S x
  | ConstV v -> if (v != []) then V v else raise (Wrong e)
  | Add (e1, e2) -> (
      match eval e1, eval e2 with
      | B b1, B b2 -> B (b1 || b2)
      | S s1, S s2 -> S (s1 +. s2)
      | V v1, V v2 when (dim v1 = dim v2) -> V (addv v1 v2)
      | _ -> raise (Wrong e)
    )
  | Inv e1 -> (
      match eval e1 with
      | B b -> B (not b)
      | S s -> S (-1.0 *.s)
      | V v -> V (inv v)
    )
  | ScalProd (e1, e2) -> (
      match eval e1, eval e2 with
      | B b1, B b2 -> B (b1 && b2)
      | S s1, S s2 -> S (s1 *. s2)
      | S s, V v | V v, S s -> V (scale s v)
      | _ -> raise (Wrong e)
    )
  | DotProd (e1, e2) -> (
      match eval e1, eval e2 with
      | V v1, V v2 when (dim v1 = dim v2)-> S (dot_prod v1 v2)
      | _ -> raise (Wrong e)
    )
  | Mag e1 -> (
      match eval e1 with
      | S s -> S (abs_float s)
      | V v -> S (length v)
      | _ -> raise (Wrong e)
    )
  | Angle (e1, e2) -> (
      match eval e1, eval e2 with
      | V v1, V v2 when (dim v1 = dim v2)-> S (angle v1 v2)
      | _ -> raise (Wrong e)
    )
  | IsZero e1 -> (
      match eval e1 with
      | B b -> B (not b)
      | S s -> B (s = 0.0)
      | V v -> B (is_zero v)
    )
    | Cond (e1, e2, e3) -> (
      match eval e1 with
      | B true when type_of e2 = type_of e3 -> eval e2
      | B false when type_of e2 = type_of e3 -> eval e3
      | _ -> raise (Wrong e)
    )
;;