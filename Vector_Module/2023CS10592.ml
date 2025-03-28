type vector = float list
module Vector = struct
  exception DimensionError of string
  
  let create n x : vector =
    let rec aux n acc =
      if n < 1 then raise (DimensionError "Dimension must >=1")
      else if n = 1 then x :: acc
      else aux (n - 1) (x :: acc)
    in
    aux n []

  let dim (v: vector)  =
  if v = [] then raise (DimensionError "Empty vector")
  else
    let rec aux v acc =
      match v with
      | [] -> acc
      | _ :: t -> aux t (acc + 1)
    in
    aux v 0
    
  let is_zero (v: vector) =
    if v = [] then raise (DimensionError "Empty vector")
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
    if v = [] then raise (DimensionError "Empty vector")
    else
      map (fun x -> c *. x) v []

  let addv (v1:vector) (v2: vector) : vector =
    if v1 = [] || v2 = [] then raise (DimensionError "Empty vector")
    else
      let rec aux v1 v2 acc =
        match v1, v2 with
        | [], [] -> reverse acc []
        | h1 :: t1, h2 :: t2 -> aux t1 t2 ((h1 +. h2) :: acc)
        | _ -> raise (DimensionError "Vectors must have the same dimension") (* To handle unexpected mismatched lengths *)
      in
      aux v1 v2 []  

  let dot_prod (v1:vector) (v2: vector) =
    if v1 = [] || v2 = [] then raise (DimensionError "Empty vector")
    else
      let rec aux v1 v2 acc =
        match v1, v2 with
        | [], [] -> acc
        | h1 :: t1, h2 :: t2 -> aux t1 t2 (acc +. (h1 *. h2))
        | _ -> raise (DimensionError "Vectors must have the same dimension") (* To handle unexpected mismatched lengths *)
      in
      aux v1 v2 0.0
      
  let inv (v : vector) : vector =
    if v = [] then raise (DimensionError "Empty vector")
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

(* Test Cases *)
open Vector
let () = 
(* Test Cases for create *)
Printf.printf "Test Cases for create function starts\n";;

let n = 5;;
let result = create n 0.0;;
Printf.printf "Zero representation of dimension 5:\n";;
Printf.printf "[ ";;
List.iter (Printf.printf "%0.2f ") result;;
Printf.printf "]";;
print_newline ();;

let result = create 3 1.5;;
Printf.printf "Constant 1.5 representation of dimension 3:\n";;
Printf.printf "[ ";;
List.iter (Printf.printf "%0.2f ") result;;
Printf.printf "]";;
print_newline ();;

let result = create 4 (-2.5);;
Printf.printf "Constant -2.5 representation of dimension 4:\n";;
Printf.printf "[ ";;
List.iter (Printf.printf "%0.2f ") result;;
Printf.printf "]";;
print_newline ();;

let result = create 6 3.14;;
Printf.printf "Constant 3.14 representation of dimension 6:\n";;
Printf.printf "[ ";;
List.iter (Printf.printf "%0.2f ") result;;
Printf.printf "]";;
print_newline ();;

let result = create 2 100.0;;
Printf.printf "Constant 100.0 representation of dimension 2:\n";;
Printf.printf "[ ";;
List.iter (Printf.printf "%0.2f ") result;;
Printf.printf "]";;
print_newline ();;

(* Test Cases for dim *)
Printf.printf "\nTest Cases for dim function starts\n";;

let v : vector = [1.0; 2.0; 3.0; 4.0; 5.0];;
let result = dim v;;
Printf.printf "Dimension of [1.0; 2.0; 3.0; 4.0; 5.0] is %d\n" result;;

let v : vector = [5.; 3.; 3.; 34.; 42.];;
let result = dim v;;
Printf.printf "Dimension of [5.; 3.; 3.; 34.; 42.] is %d\n" result;;

let v : vector = [3.14];;
let result = dim v;;
Printf.printf "Dimension of [3.14] is %d\n" result;;

let v : vector = [1.0; 2.0];;
let result = dim v;;
Printf.printf "Dimension of [1.0; 2.0] is %d\n" result;;

let v : vector = [0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0];;
let result = dim v;;
Printf.printf "Dimension of [0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0] is %d\n" result;;

(* Test Cases for is_zero *)
Printf.printf "\nTest Cases for is_zero function starts\n";;

let v : vector = [0.0; 0.0; 0.0];;
let result = is_zero v;;
Printf.printf "Is [0.0; 0.0; 0.0] zero? %b\n" result;;

let v : vector = [1.0; 0.0; 0.0];;
let result = is_zero v;;
Printf.printf "Is [1.0; 0.0; 0.0] zero? %b\n" result;;

let v : vector = [0.0; 0.0; 0.0; 0.001];;
let result = is_zero v;;
Printf.printf "Is [0.0; 0.0; 0.0; 0.001] zero? %b\n" result;;

let v : vector = [0.0];;
let result = is_zero v;;
Printf.printf "Is [0.0] zero? %b\n" result;;

(* Test Cases for unit *)
Printf.printf "\nTest Cases for unit function starts\n";;

let n = 5;;
let j = 3;;
let result = unit n j;;
Printf.printf "Unit vector of dimension %d with 1 at position %d:\n" n j;;
List.iter (Printf.printf "%0.2f ") result;;
print_newline ();;

let n = 3;;
let j = 1;;
let result = unit n j;;
Printf.printf "Unit vector of dimension %d with 1 at position %d:\n" n j;;
List.iter (Printf.printf "%0.2f ") result;;
print_newline ();;

let n = 4;;
let j = 4;;
let result = unit n j;;
Printf.printf "Unit vector of dimension %d with 1 at position %d:\n" n j;;
List.iter (Printf.printf "%0.2f ") result;;
print_newline ();;

let n = 2;;
let j = 2;;
let result = unit n j;;
Printf.printf "Unit vector of dimension %d with 1 at position %d:\n" n j;;
List.iter (Printf.printf "%0.2f ") result;;
print_newline ();;

let n = 6;;
let j = 1;;
let result = unit n j;;
Printf.printf "Unit vector of dimension %d with 1 at position %d:\n" n j;;
List.iter (Printf.printf "%0.2f ") result;;
print_newline ();;

(* Test Cases for addv *)
Printf.printf "\nTest Cases for addv function starts\n";;

let v1 : vector = [1.0; 2.0; 3.0];;
let v2 : vector = [4.0; 5.0; 6.0];;
let result = addv v1 v2;;
Printf.printf "Sum of [1.0; 2.0; 3.0] and [4.0; 5.0; 6.0]:\n";;
List.iter (Printf.printf "%0.2f ") result;;
print_newline ();;

let v1 : vector = [-1.0; -2.0; -3.0];;
let v2 : vector = [1.0; 2.0; 3.0];;
let result = addv v1 v2;;
Printf.printf "Sum of [-1.0; -2.0; -3.0] and [1.0; 2.0; 3.0]:\n";;
List.iter (Printf.printf "%0.2f ") result;;
print_newline ();;

let v1 : vector = [0.0; 0.0; 0.0];;
let v2 : vector = [1.0; 1.0; 1.0];;
let result = addv v1 v2;;
Printf.printf "Sum of [0.0; 0.0; 0.0] and [1.0; 1.0; 1.0]:\n";;
List.iter (Printf.printf "%0.2f ") result;;
print_newline ();;

let v1 : vector = [10.0; 20.0];;
let v2 : vector = [-5.0; -10.0];;
let result = addv v1 v2;;
Printf.printf "Sum of [10.0; 20.0] and [-5.0; -10.0]:\n";;
List.iter (Printf.printf "%0.2f ") result;;
print_newline ();;

(* Test Cases for dot_prod *)
Printf.printf "\nTest Cases for dot_prod function starts\n";;

let v1 : vector = [1.0; 2.0; 3.0];;
let v2 : vector = [4.0; 5.0; 6.0];;
let result = dot_prod v1 v2;;
Printf.printf "Dot product of [1.0; 2.0; 3.0] and [4.0; 5.0; 6.0] is %0.2f\n" result;;

let v1 : vector = [1.0; 1.0; 1.0];;
let v2 : vector = [2.0; 2.0; 2.0];;
let result = dot_prod v1 v2;;
Printf.printf "Dot product of [1.0; 1.0; 1.0] and [2.0; 2.0; 2.0] is %0.2f\n" result;;

let v1 : vector = [-1.0; 2.0; -3.0];;
let v2 : vector = [4.0; -5.0; 6.0];;
let result = dot_prod v1 v2;;
Printf.printf "Dot product of [-1.0; 2.0; -3.0] and [4.0; -5.0; 6.0] is %0.2f\n" result;;

let v1 : vector = [0.0; 0.0; 0.0];;
let v2 : vector = [1.0; 2.0; 3.0];;
let result = dot_prod v1 v2;;
Printf.printf "Dot product of [0.0; 0.0; 0.0] and [1.0; 2.0; 3.0] is %0.2f\n" result;;

(* Test Cases for length *)
Printf.printf "\nTest Cases for length function starts\n";;

let v : vector = [3.0; 4.0];;
let result = length v;;
Printf.printf "Length of [3.0; 4.0] is %0.2f\n" result;;

let v : vector = [1.0; 1.0; 1.0];;
let result = length v;;
Printf.printf "Length of [1.0; 1.0; 1.0] is %0.2f\n" result;;

let v : vector = [0.0; 0.0; 0.0];;
let result = length v;;
Printf.printf "Length of [0.0; 0.0; 0.0] is %0.2f\n" result;;

let v : vector = [-2.0; -2.0];;
let result = length v;;
Printf.printf "Length of [-2.0; -2.0] is %0.2f\n" result;;

(* Test Cases for scale *)
Printf.printf "\nTest Cases for scale function starts\n";;

let v : vector = [1.0; 2.0; 3.0];;
let result = scale 2.0 v;;
Printf.printf "Scale 2.0 of [1.0; 2.0; 3.0]:\n";;
List.iter (Printf.printf "%0.2f ") result;;
print_newline ();;

let v : vector = [1.0; 1.0; 1.0];;
let result = scale 3.0 v;;
Printf.printf "Scale 3.0 of [1.0; 1.0; 1.0]:\n";;
List.iter (Printf.printf "%0.2f ") result;;
print_newline ();;

let v : vector = [0.0; 0.0; 0.0];;
let result = scale 5.0 v;;
Printf.printf "Scale 5.0 of [0.0; 0.0; 0.0]:\n";;
List.iter (Printf.printf "%0.2f ") result;;
print_newline ();;

(* Test Cases for angle *)
Printf.printf "\nTest Cases for angle function starts\n";;

let v1 : vector = [1.0; 0.0];;
let v2 : vector = [0.0; 1.0];;
let result = angle v1 v2;;
Printf.printf "Angle between [1.0; 0.0] and [0.0; 1.0] is %0.2f radians\n" result;;

let v1 : vector = [1.0; 1.0];;
let v2 : vector = [1.0; 1.0];;
let result = angle v1 v2;;
Printf.printf "Angle between [1.0; 1.0] and [1.0; 1.0] is %0.2f radians\n" result;;

let v1 : vector = [1.0; 0.0; 0.0];;
let v2 : vector = [-1.0; 0.0; 0.0];;
let result = angle v1 v2;;
Printf.printf "Angle between [1.0; 0.0; 0.0] and [-1.0; 0.0; 0.0] is %0.2f radians\n" result;;

let v1 : vector = [3.0; 4.0; 0.0];;
let v2 : vector = [0.0; 0.0; 5.0];;
let result = angle v1 v2;;
Printf.printf "Angle between [3.0; 4.0; 0.0] and [0.0; 0.0; 5.0] is %0.2f radians\n" result;;

let v1 : vector = [1.0; 1.0; 1.0];;
let v2 : vector = [2.0; 2.0; 2.0];;
let result = angle v1 v2;;
Printf.printf "Angle between [1.0; 1.0; 1.0] and [2.0; 2.0; 2.0] is %0.2f radians\n" result;;

let v1 : vector = [1.0; 0.0; 0.0];;
let v2 : vector = [-1.0; 1.0; 0.0];;
let result = angle v1 v2;;
Printf.printf "Checking whether outputing small or large angle\n";;
Printf.printf "Angle between [1.0; 0.0; 0.0] and [-1.0; 1.0; 0.0] is %0.2f radians\n" result;;

(* Test Cases for inv *)

(* Exception Test Cases *)

(* Helper function to run exception tests *)
let test_exception name f =
  try
    f ();
    Printf.printf "%s: No exception raised (FAIL)\n" name
  with
  | Vector.DimensionError msg -> Printf.printf "%s: DimensionError raised - %s (PASS)\n" name msg
  | _ -> Printf.printf "%s: Unexpected exception raised (FAIL)\n" name
;;

Printf.printf("\n");;
Printf.printf "Exception Test Cases\n\n";;

(* Test Cases for create *)
Printf.printf "Exception Test Cases for create function:\n";;

test_exception "create 1" (fun () -> let _ = create 0 1.0 in ());;
test_exception "create 2" (fun () -> let _ = create (-1) 1.0 in ());;

(* Test Cases for unit *)
Printf.printf "\nException Test Cases for unit function:\n";;

test_exception "unit 1" (fun () -> let _ = unit 0 1 in ());;
test_exception "unit 2" (fun () -> let _ = unit 5 0 in ());;
test_exception "unit 3" (fun () -> let _ = unit 5 6 in ());;
test_exception "unit 4" (fun () -> let _ = unit (-1) 1 in ());;

(* Test Cases for addv *)
Printf.printf "\nException Test Cases for addv function:\n";;

test_exception "addv 1" (fun () -> let _ = addv [1.0; 2.0] [1.0; 2.0; 3.0] in ());;
test_exception "addv 2" (fun () -> let _ = addv [] [1.0] in ());;
test_exception "addv 3" (fun () -> let _ = addv [] [] in ());;

(* Test Cases for dot_prod *)
Printf.printf "\nException Test Cases for dot_prod function:\n";;

test_exception "dot_prod 1" (fun () -> let _ = dot_prod [1.0; 2.0] [1.0; 2.0; 3.0] in ());;
test_exception "dot_prod 2" (fun () -> let _ = dot_prod [] [1.0] in ());;
test_exception "dot_prod 3" (fun () -> let _ = dot_prod [] [] in ());;

(* Test Cases for angle *)
Printf.printf "\nException Test Cases for angle function:\n";;

test_exception "angle 1" (fun () -> let _ = angle [0.0; 0.0] [1.0; 1.0] in ());;
test_exception "angle 2" (fun () -> let _ = angle [1.0; 1.0] [0.0; 0.0] in ());;
test_exception "angle 3" (fun () -> let _ = angle [] [] in ());;
test_exception "angle 4" (fun () -> let _ = angle [1.0; 2.0] [1.0; 2.0; 3.0] in ());;

(* Test Cases for is_zero *)
Printf.printf "\nException Test Cases for is_zero function:\n";;

test_exception "is_zero 1" (fun () -> let _ = is_zero [] in ());;

(* Test Cases for length *)
Printf.printf "\nException Test Cases for length function:\n";;

test_exception "length 1" (fun () -> let _ = length [] in ());;

(* Test Cases for scale *)
Printf.printf "\nException Test Cases for scale function:\n";;

test_exception "scale 1" (fun () -> let _ = scale 1.0 [] in ());;

(* Properties Proof *)
let u : vector = [1.0; 2.0; 3.0];;
let v : vector = [4.0; 5.0; 6.0];;
let w : vector = [7.0; 8.0; 9.0];;

(* The functions made by me are tail recursive so I could have did in two ways either first reverse the vector so finally I will get my right answer 
   or which I have done is reversing the vector at last but in both cases answer will be the same. So in my proofs I have solved for the dimensions of vector one can choose both ways either give input
  reversed or later reversing the answer *)
(* Plus Extra points some functions don't care whether the function is tail recursive or not*)

(* 1. Commutativity: addv u v = addv v u *)
(* Proof: 
Base Case : u = [h1] and v = [h2]
addv u v = addv [h1] [h2] = [h1 + h2] = addv [h2] [h1] = addv v u Since floating point addition is commutative
Inductive Hypothesis: Assume addv u v = addv v u for k dimension vector
Inductive Step: Let u = [h1::t1] and v = [h2::t2] for k+1 dimension vector
addv u v = addv [h1::t1] [h2::t2] = [h1 + h2] :: addv t1 t2 = [h2 + h1] :: addv t2 t1 = addv v u
By induction, addv u v = addv v u *)

let b = addv v u = addv u v in
Printf.printf "Commutativity %B" b ;;
print_newline ();;

(* 2. Associativity: addv u (addv v w) = addv (addv u v) w *)
(* Proof: 
Base Case : u = [h1] and v = [h2] and w = [h3]
addv u (addv v w) = addv [h1] (addv [h2] [h3]) = addv [h1] [h2 + h3] = [h1 + h2 + h3] = addv [h1 + h2] [h3] = addv (addv [h1] [h2]) [h3] = addv (addv u v) w
Inductive Hypothesis: Assume addv u (addv v w) = addv (addv u v) w for k dimension vector
Inductive Step: Let u = [h1::t1] and v = [h2::t2] and w = [h3::t3] for k+1 dimension vector
addv u (addv v w) = addv [h1::t1] (addv [h2::t2] [h3::t3]) = addv [h1::t1] [h2 + h3 :: addv t2 t3] = [h1 + (h2 + h3) :: addv t1 (addv t2 t3)]
By induction hypothesis, addv t1 (addv t2 t3) = addv (addv t1 t2) t3
                  = [(h1 + h2) + h3 :: addv (addv t1 t2) t3] = addv [h1 + h2 :: addv t1 t2] [h3::t3] = addv (addv [h1::t1] [h2::t2]) [h3::t3] = addv (addv u v) w
Therefore, addv u (addv v w) = addv (addv u v) w *)
let b = addv u (addv v w) = addv (addv u v) w in
Printf.printf "Associativity %B" b ;;
print_newline ();;

(* 3. Identity of addition: addv v (create (dim v) 0.0) = v *)
(* Proof:
        To prove: addv v (create (dim v) 0.0) = v
        Base Case : v = [h1]
        addv v (create (dim v) 0.0) = addv [h1] (create 1 0.0) = [h1 + 0.0] = [h1] = v
        Inductive Hypothesis: Assume addv v (create (dim v) 0.0) = v for k dimension vector
        Inductive Step: Let v = [h1::t1] for k+1 dimension vector
        addv v (create (dim v) 0.0) = addv [h1::t1] (create (k+1) 0.0) = [h1 + 0.0 :: addv t1 (create k 0.0)] = [h1 :: t1] = v    By induction hypothesis, addv t1 (create k 0.0) = t1
        Therefore, addv v (create (dim v) 0.0) = v *)

let b = addv v (create (dim v) 0.0) = v in
Printf.printf "Identity of addition %B" b ;;
print_newline ();;

(* 4. Identity scalar: scale 1.0 v = v *)
(* Proof: 
          To prove: scale 1.0 v = v
          Base Case : v = [h1]
          scale 1.0 v = scale 1.0 [h1] = [1.0 *. h1] = [h1] = v
          Inductive Hypothesis: Assume scale 1.0 v = v for k dimension vector
          Inductive Step: Let v = [h1::t1] for k+1 dimension vector
          scale 1.0 v = scale 1.0 [h1::t1] = [1.0 *. h1 :: scale 1.0 t1] = [h1 :: t1] = v    By induction hypothesis, scale 1.0 t1 = t1
          Therefore, scale 1.0 v = v *)

let b = ( scale 1.0 v = v ) in
Printf.printf "Identity scalar %B" b ;;
print_newline ();;

(* 5. Annihilator scalar: scale 0.0 v = create (dim v) 0.0 *)
(* Proof: 
        To prove: scale 0.0 v = create (dim v) 0.0
        Base Case : v = [h1]
        scale 0.0 v = scale 0.0 [h1] = [0.0 *. h1] = [0.0] = create 1 0.0
        Inductive Hypothesis: Assume scale 0.0 v = create (dim v) 0.0 for k dimension vector
        Inductive Step: Let v = [h1::t1] for k+1 dimension vector
        scale 0.0 v = scale 0.0 [h1::t1] = [0.0 *. h1 :: scale 0.0 t1] = [0.0 :: create k 0.0] = create (k+1) 0.0    By induction hypothesis, scale 0.0 t1 = create k 0.0
        Therefore, scale 0.0 v = create (dim v) 0.0 *)
let b = scale 0.0 v = create (dim v) 0.0 in
Printf.printf "Annihilator scalar %B" b;;
print_newline ();;

(* 6. Additive Inverse: addv v (inv v) = create (dim v) 0.0 *)
(* Proof: 
        To prove: addv v (inv v) = create (dim v) 0.0
        Base Case : v = [h1]
        addv v (inv v) = addv [h1] (inv [h1]) = [h1 + (-1.0 *. h1)] = [0.0] = create 1 0.0
        Inductive Hypothesis: Assume addv v (inv v) = create (dim v) 0.0 for k dimension vector
        Inductive Step: Let v = [h1::t1] for k+1 dimension vector
        addv v (inv v) = addv h1::t1 (inv h1::t1) = addv h1::t1 [(-1.0 *. h1)::inv t1] = h1-h1::addv t1 (inv t1)
                       = 0.0 :: create k 0.0      By Inductive Hypothesis
                       = create k+1 0.0
        By Induction addv v (inv v) = create (dim v) 0.0 *)
let b = addv v (inv v) = create (dim v) 0.0 in
Printf.printf "Additive Inverse %B" b;;
print_newline ();;

(* 7. Scalar product combination: scale b (scale c v) = scale (b *. c) v *)
(* Proof: 
        To prove: scale b (scale c v) = scale (b *. c) v
        Base Case : v = [h1]
        scale b (scale c v) = scale b (scale c [h1]) = scale b [c *. h1] = [b *. (c *. h1)] = scale (b *. c) [h1] = scale (b *. c) v
        Inductive Hypothesis: Assume scale b (scale c v) = scale (b *. c) v for k dimension vector
        Inductive Step: Let v = [h1::t1] for k+1 dimension vector
        scale b (scale c v) = scale b (scale c [h1::t1]) = scale b [c *. h1::scale c t1] = [b *. (c *. h1)::scale b (scale c t1)] = [b *. (c *. h1)::scale (b *. c) t1]
                           = scale (b *. c) [h1::t1] = scale (b *. c) v
        Therefore, scale b (scale c v) = scale (b *. c) v *)
let b = scale 3.0 (scale 2.0 v) = scale (3.0 *. 2.0) v in
Printf.printf "Scalar product combination %B" b;;
print_newline ();;

(* 8. Scalar sum-product distribution: scale (b +. c) v = addv (scale b v) (scale c v) *)
(* Proof: 
          To prove: scale (b +. c) v = addv (scale b v) (scale c v)
          Base Case : v = [h1]
          scale (b +. c) v = scale (b +. c) [h1] = [(b +. c) *. h1] = [b *. h1 +. c *. h1] = addv [b *. h1] [c *. h1] = addv (scale b [h1]) (scale c [h1]) = addv (scale b v) (scale c v)
          Inductive Hypothesis: Assume scale (b +. c) v = addv (scale b v) (scale c v) for k dimension vector
          Inductive Step: Let v = [h1::t1] for k+1 dimension vector
          scale (b +. c) v = scale (b +. c) [h1::t1] = [(b +. c) *. h1::scale (b +. c) t1] = [b *. h1 +. c *. h1::addv (scale b t1) (scale c t1)] 
                           = [b *. h1 + c *. h1::addv (scale b t1) (scale c t1)] = addv (scale b [h1::t1]) (scale c [h1::t1]) = addv (scale b v) (scale c v)
          Therefore, scale (b +. c) v = addv (scale b v) (scale c v) *)

let b = scale (5.0) v = addv (scale 3.0 v) (scale 2.0 v) in 
Printf.printf "Scalar sum-product distribution %B" b;; 
print_newline ();;

(* 9. Scalar Distribution over vector sums: scale b (addv u v) = addv (scale b u) (scale b v) *)
(* Proof: 
          To prove: scale b (addv u v) = addv (scale b u) (scale b v)
          Base Case : u = [h1] and v = [h2]
          scale b (addv u v) = scale b (addv [h1] [h2]) = scale b [h1 + h2] = [b *. (h1 + h2)] = [b *. h1 + b *. h2] = addv [b *. h1] [b *. h2] = addv (scale b [h1]) (scale b [h2]) = addv (scale b u) (scale b v)
          Inductive Hypothesis: Assume scale b (addv u v) = addv (scale b u) (scale b v) for k dimension vector
          Inductive Step: Let u = [h1::t1] and v = [h2::t2] for k+1 dimension vector
          scale b (addv u v) = scale b (addv [h1::t1] [h2::t2]) = scale b [h1 + h2::addv t1 t2] = [b *. (h1 + h2)::addv (scale b t1) (scale b t2)] = [b *. h1 + b *. h2::addv (scale b t1) (scale b t2)]
                           = addv [b *. h1::scale b t1] [b *. h2::scale b t2] = addv (scale b [h1::t1]) (scale b [h2::t2]) = addv (scale b u) (scale b v)
          Therefore, scale b (addv u v) = addv (scale b u) (scale b v) *)
let b = scale 3.0 (addv u v) = addv (scale 3.0 u) (scale 3.0 v) in
Printf.printf "Scalar Distribution over vector sums %B" b;;
print_newline ();;

(*Extra properties at least three*)

(* 10. Length of a scaled vector: length (scale c v) = abs_float c *. length v *)
(* Proof: 
          To prove: length (scale c v) = abs_float c *. length v
          Base Case : v = [h1]
          length (scale c v) = length (scale c [h1]) = length [c *. h1] = abs_float c *. abs_float h1 = abs_float c *. length [h1] = abs_float c *. length v
          Inductive Hypothesis: Assume length (scale c v) = abs_float c *. length v for k dimension vector
          Inductive Step: Let v = [h1::t1] for k+1 dimension vector
          length (scale c v) = length (scale c [h1::t1]) = length [c *. h1::scale c t1] = sqrt[sqaure(abs_float c *. abs_float h1) +. square(length (scale c t1))]
                            = sqrt[sqaure(abs_float c *. abs_float h1) +. square(abs_float c *. length t1)] = sqrt[sqaure(abs_float c) *. sqaure(abs_float h1) +. square(abs_float c) *. square(length t1)]
                            = sqrt[sqaure(abs_float c) *. (sqaure(abs_float h1) +. square(length t1))] = abs_float c *. sqrt[sqaure(abs_float h1) +. square(length t1)] = abs_float c *. length [h1::t1] = abs_float c *. length v
          Therefore, length (scale c v) = abs_float c *. length v *)
let b = abs_float 3.0 *. length v = length (scale 3.0 v) in
Printf.printf "Length of a scaled vector %B" b;;
print_newline ();;

(* 11. Dot product of a scaled vector: dot_prod (scale c v) (scale d u) = c *. d *. dot_prod v u *)
(* Proof: 
          To prove: dot_prod (scale c v) (scale d u) = c *. d *. dot_prod v u
          Base Case : v = [h1] and u = [h2]
          dot_prod (scale c v) (scale d u) = dot_prod (scale c [h1]) (scale d [h2]) = dot_prod [c *. h1] [d *. h2] = (c *. h1) *. (d *. h2) = c *. d *. h1 *. h2 = c *. d *. dot_prod [h1] [h2] = c *. d *. dot_prod v u
          Inductive Hypothesis: Assume dot_prod (scale c v) (scale d u) = c *. d *. dot_prod v u for k dimension vector
          Inductive Step: Let v = [h1::t1] and u = [h2::t2] for k+1 dimension vector
          dot_prod (scale c v) (scale d u) = dot_prod (scale c [h1::t1]) (scale d [h2::t2]) = dot_prod [c *. h1::scale c t1] [d *. h2::scale d t2] = (c *. h1) *. (d *. h2) +. dot_prod (scale c t1) (scale d t2)
                                          = c *. d *. h1 *. h2 +. c *. d *. dot_prod t1 t2 = c *. d *. dot_prod [h1::t1] [h2::t2] = c *. d *. dot_prod v u
          Therefore, dot_prod (scale c v) (scale d u) = c *. d *. dot_prod v u *)
let b = 2.0 *. 3.0 *. dot_prod v u = dot_prod (scale 2.0 v) (scale 3.0 u) in
Printf.printf "Dot product of a scaled vector %B" b;;
print_newline ();;

(* 12. Angle between scaled vectors: angle (scale c v) (scale d u) = angle v u *)
(* Proof: 
          To prove: angle (scale c v) (scale d u) = angle v u
          Base Case : v = [h1] and u = [h2]
          angle (scale c v) (scale d u) = angle (scale c [h1]) (scale d [h2]) = angle [c *. h1] [d *. h2] = acos((c *. h1) *. (d *. h2) /. (length [c *. h1] *. length [d *. h2])) = acos(c *. d *. h1 *. h2 /. (abs_float c *. abs_float h1 *. abs_float d *. abs_float h2))
                                        = acos(c *. d *. h1 *. h2 /. (abs_float c *. abs_float d *. length [h1] *. length [h2])) = acos(c *. d *. h1 *. h2 /. (abs_float c *. abs_float d *. length v *. length u)) = acos(dot_prod v u /. (length v *. length u)) = angle v u
          Inductive Hypothesis: Assume angle (scale c v) (scale d u) = angle v u for k dimension vector
          Inductive Step: Let v = [h1::t1] and u = [h2::t2] for k+1 dimension vector
          angle (scale c v) (scale d u) = angle (scale c [h1::t1]) (scale d [h2::t2]) = angle [c *. h1::scale c t1] [d *. h2::scale d t2] = acos((c *. h1) *. (d *. h2) +. dot_prod (scale c t1) (scale d t2) /. (length (scale c [h1::t1]) *. length (scale d [h2::t2]))
                                        = acos(c *. d *. h1 *. h2 +. dot_prod (scale c t1) (scale d t2) /. { sqrt[square(abs_float c *. abs_float h1) +. square(length (scale c t1))] *. sqrt[square(abs_float d *. abs_float h2) +. square(length (scale d t2))])}
                                        = acos(c *. d *. h1 *. h2 +. dot_prod (scale c t1) (scale d t2) /. { sqrt[square(abs_float c) *. square(abs_float h1) +. square(abs_float c) *. square(length t1)] *. sqrt[square(abs_float d) *. square(abs_float h2) +. square(abs_float d) *. square(length t2)])}
                                        = acos(c *. d *. h1 *. h2 +. dot_prod (scale c t1) (scale d t2) /. { abs_float c *. abs_float d *. sqrt[square(abs_float h1) +. square(length t1)] *. sqrt[square(abs_float h2) +. square(length t2)])}
                                        = acos(c *. d *. h1 *. h2 +. dot_prod (scale c t1) (scale d t2) /. { abs_float c *. abs_float d *. length [h1::t1] *. length [h2::t2])} = acos(dot_prod v u /. (length v *. length u)) = angle v u
          Therefore, angle (scale c v) (scale d u) = angle v u *)
let b = ( abs_float(angle v u -. angle (scale 2.0 v) (scale 3.0 u)) < 1e-10 ) in
Printf.printf "Angle between scaled vectors %B" b;;
print_newline ();;

(* 13. Dot product of a vector with itself: dot_prod v v = length v *. length v *)
(* Proof: 
          To prove: dot_prod v v = length v *. length v
          Base Case : v = [h1]
                dot_prod v v = dot_prod [h1] [h1] = h1 *. h1 = square h1 = length [h1] *. length [h1] = length v *. length v
          Inductive Hypothesis: Assume dot_prod v v = length v *. length v for k dimension vector
          Inductive Step: Let v = [h1::t1] for k+1 dimension vector
                  dot_prod v v = dot_prod [h1::t1] [h1::t1] = h1 *. h1 +. dot_prod t1 t1 = square h1 +. length t1 *. length t1 = length [h1::t1] *. length [h1::t1] = length v *. length v
          Therefore, dot_prod v v = length v *. length v *)    
let b = abs_float(length v *. length v -. dot_prod v v) < 1e-10 in
Printf.printf "Dot product of a vector with itself %B" b;;
print_newline ();;