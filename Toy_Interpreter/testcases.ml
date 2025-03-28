open A2

(* Test cases for type_of function *)
(* ======== Type Checking: Boolean Constants ======== *)
let%test "type_of T" = (type_of T = Bool)
let%test "type_of F" = (type_of F = Bool)

(* ======== Type Checking: Scalar and Vector Constants ======== *)
let%test "type_of (ConstS 3.14)" = (type_of (ConstS 3.14) = Scalar)
let%test "type_of (ConstV [1.0; 2.0; 3.0])" = (type_of (ConstV [1.0; 2.0; 3.0]) = Vector 3)

(* ======== Type Checking: Add Expression ======== *)
let%test "type_of (Add (ConstS 2.0, ConstS 3.0))" = (type_of (Add (ConstS 2.0, ConstS 3.0)) = Scalar)
let%test "type_of (Add T, F)" = (type_of (Add (T, F)) = Bool)
let%test "type_of (Add (ConstV [1.0; 2.0], ConstV [3.0; 4.0]))" = (type_of (Add (ConstV [1.0; 2.0], ConstV [3.0; 4.0])) = Vector 2)

(* ======== Type Checking: Inversion Expression ======== *)
let%test "type_of (Inv (ConstS (-5.0)))" = (type_of (Inv (ConstS (-5.0))) = Scalar)
let%test "type_of (Inv (ConstV [1.0; -1.0]))" = (type_of (Inv (ConstV [1.0; -1.0])) = Vector 2)

(* ======== Type Checking: Scalar Product ======== *)
let%test "type_of (ScalProd (ConstS 2.0, ConstS 3.0))" = (type_of (ScalProd (ConstS 2.0, ConstS 3.0)) = Scalar)
let%test "type_of (ScalProd (ConstS 2.0, ConstV [1.0; 2.0]))" = (type_of (ScalProd (ConstS 2.0, ConstV [1.0; 2.0])) = Vector 2)

(* ======== Type Checking: Dot Product ======== *)
let%test "type_of (DotProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0]))" = (type_of (DotProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0])) = Scalar)

(* ======== Type Checking: Magnitude ======== *)
let%test "type_of (Mag (ConstV [3.0; 4.0]))" = (type_of (Mag (ConstV [3.0; 4.0])) = Scalar)

(* ======== Type Checking: Angle Calculation ======== *)
let%test "type_of (Angle (ConstV [1.0; 0.0], ConstV [0.0; 1.0]))" = (type_of (Angle (ConstV [1.0; 0.0], ConstV [0.0; 1.0])) = Scalar)

(* ======== Type Checking: Conditionals ======== *)
let%test "type_of (Cond (T, ConstS 2.0, ConstS 3.0))" = (type_of (Cond (T, ConstS 2.0, ConstS 3.0)) = Scalar)
let%test "type_of (Cond (T, ConstV [1.0; 2.0], ConstV [3.0; 4.0]))" = (type_of (Cond (T, ConstV [1.0; 2.0], ConstV [3.0; 4.0])) = Vector 2)

(* ======== Deeply Nested Cases ======== *)
let%test "type_of (Add (Add (ConstS 1.0, ConstS 2.0), ConstS 3.0))" = (type_of (Add (Add (ConstS 1.0, ConstS 2.0), ConstS 3.0)) = Scalar)
let%test "type_of (Angle (Inv (ConstV [1.0; 0.0]), Inv (ConstV [0.0; 1.0])))" = (type_of (Angle (Inv (ConstV [1.0; 0.0]), Inv (ConstV [0.0; 1.0]))) = Scalar)

(* ==================== Type Checking Tests ==================== *)

(* ======== Type Checking: Add Expression ======== *)
let%test "type_of (Add (ConstS 2.0, ConstS 3.0))" = (type_of (Add (ConstS 2.0, ConstS 3.0)) = Scalar)
let%test "type_of (Add (ConstV [1.0; 2.0], ConstV [3.0; 4.0]))" = (type_of (Add (ConstV [1.0; 2.0], ConstV [3.0; 4.0])) = Vector 2)

(* ======== Type Checking: Inv Expression ======== *)
let%test "type_of (Inv T)" = (type_of (Inv T) = Bool)
let%test "type_of (Inv (ConstS 5.0))" = (type_of (Inv (ConstS 5.0)) = Scalar)
let%test "type_of (Inv (ConstV [1.0; -2.0; 3.0]))" = (type_of (Inv (ConstV [1.0; -2.0; 3.0])) = Vector 3)

(* ======== Type Checking: Scalar Product ======== *)
let%test "type_of (ScalProd (ConstS 2.0, ConstS 3.0))" = (type_of (ScalProd (ConstS 2.0, ConstS 3.0)) = Scalar)
let%test "type_of (ScalProd (ConstS 2.0, ConstV [1.0; 2.0; 3.0]))" = (type_of (ScalProd (ConstS 2.0, ConstV [1.0; 2.0; 3.0])) = Vector 3)

(* ======== Type Checking: Dot Product ======== *)
let%test "type_of (DotProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0]))" = (type_of (DotProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0])) = Scalar)
let%test "type_of (DotProd (ConstV [1.0; 2.0; 3.0], ConstV [4.0; 5.0; 6.0]))" = (type_of (DotProd (ConstV [1.0; 2.0; 3.0], ConstV [4.0; 5.0; 6.0])) = Scalar)

(* ======== Type Checking: Magnitude ======== *)
let%test "type_of (Mag (ConstV [3.0; 4.0]))" = (type_of (Mag (ConstV [3.0; 4.0])) = Scalar)

(* ======== Type Checking: Angle Calculation ======== *)
let%test "type_of (Angle (ConstV [1.0; 0.0], ConstV [0.0; 1.0]))" = (type_of (Angle (ConstV [1.0; 0.0], ConstV [0.0; 1.0])) = Scalar)

(* ======== Type Checking: Conditionals ======== *)
let%test "type_of (Cond (T, ConstS 2.0, ConstS 3.0))" = (type_of (Cond (T, ConstS 2.0, ConstS 3.0)) = Scalar)
let%test "type_of (Cond (T, ConstV [1.0; 2.0], ConstV [3.0; 4.0]))" = (type_of (Cond (T, ConstV [1.0; 2.0], ConstV [3.0; 4.0])) = Vector 2)

(* ======== Deeply Nested Cases ======== *)
let%test "type_of (Add (Add (ConstS 1.0, ConstS 2.0), ConstS 3.0))" = (type_of (Add (Add (ConstS 1.0, ConstS 2.0), ConstS 3.0)) = Scalar)
let%test "type_of (Angle (Inv (ConstV [1.0; 0.0]), Inv (ConstV [0.0; 1.0])))" = (type_of (Angle (Inv (ConstV [1.0; 0.0]), Inv (ConstV [0.0; 1.0]))) = Scalar)

(* ======== Type Checking: IsZero Expression ======== *)
let%test "type_of IsZero(T)" = (type_of (IsZero T) = Bool)
let%test "type_of IsZero(ConstS 0.0)" = (type_of (IsZero (ConstS 0.0)) = Bool)
let%test "type_of IsZero(ConstV [0.0; 0.0; 0.0])" = (type_of (IsZero (ConstV [0.0; 0.0; 0.0])) = Bool)

(* ======== Type Checking: Conditional Expression ======== *)
let%test "type_of Cond(IsZero(ConstS 0.0), ConstS 2.0, ConstS 3.0)" = (type_of (Cond (IsZero (ConstS 0.0), ConstS 2.0, ConstS 3.0)) = Scalar)
let%test "type_of Cond(IsZero(ConstS 1.0), ConstV [1.0; 2.0], ConstV [3.0; 4.0])" = 
  (type_of (Cond (IsZero (ConstS 1.0), ConstV [1.0; 2.0], ConstV [3.0; 4.0])) = Vector 2)


(* ==================== Type Checking Exception Tests ==================== *)

let%test "eval (Add (ConstS 1.0, ConstV [2.0])) raises Wrong (Add (ConstS 1.0, ConstV [2.0]))" =
  (try
    let _ = eval (Add (ConstS 1.0, ConstV [2.0])) in false
  with
  | Wrong e -> e = Add (ConstS 1.0, ConstV [2.0])
  | _ -> false)

let%test "eval (Add (T, ConstS 3.0)) raises Wrong (Add (T, ConstS 3.0))" =
  (try
    let _ = eval (Add (T, ConstS 3.0)) in false
  with
  | Wrong e -> e = Add (T, ConstS 3.0)
  | _ -> false)

let%test "eval (Add (ConstV [1.0; 2.0], ConstV [3.0])) raises Wrong (Add (ConstV [1.0; 2.0], ConstV [3.0]))" =
  (try
    let _ = eval (Add (ConstV [1.0; 2.0], ConstV [3.0])) in false
  with
  | Wrong e -> e = Add (ConstV [1.0; 2.0], ConstV [3.0])
  | _ -> false)

let%test "eval (ScalProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0])) raises Wrong (ScalProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0]))" =
  (try
    let _ = eval (ScalProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0])) in false
  with
  | Wrong e -> e = ScalProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0])
  | _ -> false)

let%test "eval (DotProd (Add (ConstV [1.0; 2.0], ConstV [3.0]), ConstV [4.0; 5.0])) raises Wrong (DotProd (Add (ConstV [1.0; 2.0], ConstV [3.0]), ConstV [4.0; 5.0]))" =
  (try
    let _ = eval (DotProd (Add (ConstV [1.0; 2.0], ConstV [3.0]), ConstV [4.0; 5.0])) in false
  with
  | Wrong _ -> true
  | _ -> false)

let%test "eval (DotProd (ConstV [1.0; 2.0], ConstV [3.0])) raises Wrong (DotProd (ConstV [1.0; 2.0], ConstV [3.0]))" =
  (try
    let _ = eval (DotProd (ConstV [1.0; 2.0], ConstV [3.0])) in false
  with
  | Wrong e -> e = DotProd (ConstV [1.0; 2.0], ConstV [3.0])
  | _ -> false)

let%test "eval (DotProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0; 5.0])) raises Wrong (DotProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0; 5.0]))" =
  (try
    let _ = eval (DotProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0; 5.0])) in false
  with
  | Wrong e -> e = DotProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0; 5.0])
  | _ -> false)

let%test "eval (Mag (T)) raises Wrong (Mag (T))" =
  (try
    let _ = eval (Mag (T)) in false
  with
  | Wrong e -> e = Mag (T)
  | _ -> false)

let%test "eval (Mag (Add (ConstS 2.0, ConstV [3.0; 4.0]))) raises Wrong (Mag (Add (ConstS 2.0, ConstV [3.0; 4.0])))" =
  (try
    let _ = eval (Mag (Add (ConstS 2.0, ConstV [3.0; 4.0]))) in false
  with
  | Wrong _ -> true
  | _ -> false)

let%test "eval (Angle (ConstS 2.0, ConstV [1.0; 2.0])) raises Wrong (Angle (ConstS 2.0, ConstV [1.0; 2.0]))" =
  (try
    let _ = eval (Angle (ConstS 2.0, ConstV [1.0; 2.0])) in false
  with
  | Wrong e -> e = Angle (ConstS 2.0, ConstV [1.0; 2.0])
  | _ -> false)

  let%test "eval (Angle (ConstS 1.0, ConstS 2.0)) raises Wrong (Angle (ConstS 1.0, ConstS 2.0))" =
  (try
    let _ = eval (Angle (ConstS 1.0, ConstS 2.0)) in false
  with
  | Wrong e -> e = Angle (ConstS 1.0, ConstS 2.0)
  | _ -> false)

let%test "eval (Angle (DotProd (ConstV [1.0; 2.0], ConstV [3.0]), ConstV [4.0; 5.0])) raises Wrong (Angle (DotProd (ConstV [1.0; 2.0], ConstV [3.0]), ConstV [4.0; 5.0]))" =
  (try
    let _ = eval (Angle (DotProd (ConstV [1.0; 2.0], ConstV [3.0]), ConstV [4.0; 5.0])) in false
  with
  | Wrong _ -> true
  | _ -> false)

let%test "eval (Cond (ConstS 3.0, ConstV [1.0; 2.0], ConstV [3.0; 4.0])) raises Wrong (Cond (ConstS 3.0, ConstV [1.0; 2.0], ConstV [3.0; 4.0]))" =
  (try
    let _ = eval (Cond (ConstS 3.0, ConstV [1.0; 2.0], ConstV [3.0; 4.0])) in false
  with
  | Wrong e -> e = Cond (ConstS 3.0, ConstV [1.0; 2.0], ConstV [3.0; 4.0])
  | _ -> false)

let%test "eval (Cond (T, Add (ConstS 2.0, ConstS 3.0), ConstV [1.0; 2.0])) raises Wrong (Cond (T, Add (ConstS 2.0, ConstS 3.0), ConstV [1.0; 2.0]))" =
  (try
    let _ = eval (Cond (T, Add (ConstS 2.0, ConstS 3.0), ConstV [1.0; 2.0])) in false
  with
  | Wrong e -> e = Cond (T, Add (ConstS 2.0, ConstS 3.0), ConstV [1.0; 2.0])
  | _ -> false)

let%test "eval (Cond (ConstS 3.0, ConstV [1.0; 2.0], ConstV [3.0; 4.0])) raises Wrong (Cond (ConstS 3.0, ConstV [1.0; 2.0], ConstV [3.0; 4.0]))" =
  (try
    let _ = eval (Cond (ConstS 3.0, ConstV [1.0; 2.0], ConstV [3.0; 4.0])) in false
  with
  | Wrong e -> e = Cond (ConstS 3.0, ConstV [1.0; 2.0], ConstV [3.0; 4.0])
  | _ -> false)

let%test "eval (Cond (IsZero (ConstS 1.0), ConstS 2.0, ConstV [3.0; 4.0])) raises Wrong (Cond (IsZero (ConstS 1.0), ConstS 2.0, ConstV [3.0; 4.0]))" =
  (try
    let _ = eval (Cond (IsZero (ConstS 1.0), ConstS 2.0, ConstV [3.0; 4.0])) in false
  with
  | Wrong e -> e = Cond (IsZero (ConstS 1.0), ConstS 2.0, ConstV [3.0; 4.0])
  | _ -> false)

let%test "eval (Angle (ConstS 2.0, ConstV [1.0; 2.0])) raises Wrong (Angle (ConstS 2.0, ConstV [1.0; 2.0]))" =
  (try
    let _ = eval (Angle (ConstS 2.0, ConstV [1.0; 2.0])) in false
  with
  | Wrong e -> e = Angle (ConstS 2.0, ConstV [1.0; 2.0])
  | _ -> false)

let%test "eval (Angle (ConstV [1.0; 2.0], ConstS 3.0)) raises Wrong (Angle (ConstV [1.0; 2.0], ConstS 3.0))" =
  (try
    let _ = eval (Angle (ConstV [1.0; 2.0], ConstS 3.0)) in false
  with
  | Wrong e -> e = Angle (ConstV [1.0; 2.0], ConstS 3.0)
  | _ -> false)

(* Test Cases for Boolean Expressions in eval function *)

(* ==================== Basic Boolean Tests ==================== *)
let%test "eval T" = (eval T = B true)
let%test "eval F" = (eval F = B false)

(* ==================== IsZero Tests ==================== *)
let%test "eval (IsZero (ConstS 0.0))" = (eval (IsZero (ConstS 0.0)) = B true)
let%test "eval (IsZero (ConstS 1.0))" = (eval (IsZero (ConstS 1.0)) = B false)
let%test "eval (IsZero (ConstS (-1.0)))" = (eval (IsZero (ConstS (-1.0))) = B false)
let%test "eval (IsZero (ConstS 1e-10))" = (eval (IsZero (ConstS 1e-10)) = B false)
let%test "eval (IsZero (Add (ConstS (-2.0), ConstS 2.0)))" = (eval (IsZero (Add (ConstS (-2.0), ConstS 2.0))) = B true)
let%test "eval (IsZero (ScalProd (ConstS 0.0, ConstS 5.0)))" = (eval (IsZero (ScalProd (ConstS 0.0, ConstS 5.0))) = B true)

(* ==================== Cond (Conditional) Tests ==================== *)
let%test "eval (Cond (T, T, F))" = (eval (Cond (T, T, F)) = B true)
let%test "eval (Cond (F, F, T))" = (eval (Cond (F, F, T)) = B true)
let%test "eval (Cond (IsZero (ConstS 0.0), IsZero (ConstS 1.0), IsZero (ConstS (-1.0))))" = 
  (eval (Cond (IsZero (ConstS 0.0), IsZero (ConstS 1.0), IsZero (ConstS (-1.0)))) = B false)
let%test "eval (Cond (Cond (T, F, T), Cond (F, T, F), Cond (T, T, F)))" = (eval (Cond (Cond (T, F, T), Cond (F, T, F), Cond (T, T, F))) = B true)
let%test "eval (Cond(Inv(F), ScalProd(T, Add(F, T)), Add(F, F)))" = (eval (Cond(Inv(F), ScalProd(T, Add(F, T)), Add(F, F))) = B true)
let%test "eval (Cond(Add(ScalProd(T, Inv(F)), Inv(Add(F, F))), ScalProd(Add(T, Inv(ScalProd(T, F))), Inv(Add(F, T))), Inv(Inv(T))))" = 
  (eval (Cond(Add(ScalProd(T, Inv(F)), Inv(Add(F, F))), ScalProd(Add(T, Inv(ScalProd(T, F))), Inv(Add(F, T))), Inv(Inv(T)))) = B false)
let%test "eval (Cond (Add(T, F), Cond (Inv(F), ScalProd(T, T), Add(F, F)), Inv(T)))" = 
  (eval (Cond (Add(T, F), Cond (Inv(F), ScalProd(T, T), Add(F, F)), Inv(T))) = B true)

(* ==================== Scalar Product Tests ==================== *)
let%test "eval (ScalProd (Cond (Add(T, F), Cond (Inv(F), ScalProd(T, T), Add(F, F)), Inv(T)), F))" =
  (eval (ScalProd (Cond (Add(T, F), Cond (Inv(F), ScalProd(T, T), Add(F, F)), Inv(T)), F)) = B false)
let%test "eval (ScalProd (Add(T, F), Inv(Add(T, T))))" = 
  (eval (ScalProd (Add(T, F), Inv(Add(T, T)))) = B false)   
let%test "eval (Inv (ScalProd (Add(T, F), Add(F, F))))" = 
  (eval (Inv (ScalProd (Add(T, F), Add(F, F)))) = B true)
let%test "eval (ScalProd (T, Add(F, Inv(F))))" = 
  (eval (ScalProd (T, Add(F, Inv(F)))) = B true)
let%test "eval (ScalProd (Add(T, ScalProd(F, T)), Inv(Add(F, Inv(T)))))" = 
  (eval (ScalProd (Add(T, ScalProd(F, T)), Inv(Add(F, Inv(T))))) = B true)

(* Add Operations *)
let%test "eval (Add(T, F))" = (eval (Add(T, F)) = B true)
let%test "eval (Add(T, ScalProd(F, Inv(T))))" = (eval (Add(T, ScalProd(F, Inv(T)))) = B true)
let%test "eval (Add(ScalProd(Cond(T, F, T), Add(F, T)), Inv(ScalProd(Add(T, F), Cond(F, T, F)))))" = 
  (eval (Add(ScalProd(Cond(T, F, T), Add(F, T)), Inv(ScalProd(Add(T, F), Cond(F, T, F))))) = B true)
let%test "eval (Add(ScalProd(T, F), Inv(F)))" = (eval (Add(ScalProd(T, F), Inv(F))) = B true)

(* Inverse operation *)
let%test "eval (Inv(T))" = (eval (Inv(T)) = B false)
let%test "eval (Inv(Add(Inv(T), Inv(F))))" = (eval (Inv(Add(Inv(T), Inv(F)))) = B false)
let%test "eval (Inv(ScalProd(Inv(T), Inv(F))))" = (eval (Inv(ScalProd(Inv(T), Inv(F)))) = B true)

(* ==================== Edge Cases with Multiple Negations ==================== *)
let%test "eval (Inv(Inv(Inv(T))))" = (eval (Inv(Inv(Inv(T)))) = B false)
let%test "eval (Inv(Inv(Inv(Inv(F)))))" = (eval (Inv(Inv(Inv(Inv(F))))) = B false)

(* TestCases for Scalar Operations in eval function *)

(* ==================== Scalar Constant Tests ==================== *)
let%test "eval (ConstS 5.0)" = (eval (ConstS 5.0) = S 5.0)
let%test "eval (ConstS (-3.14))" = (eval (ConstS (-3.14)) = S (-3.14))
let%test "eval (ConstS 0.0)" = (eval (ConstS 0.0) = S 0.0)
let%test "eval (ConstS 1e-10)" = (eval (ConstS 1e-10) = S 1e-10)
let%test "eval (ConstS 1e10)" = (eval (ConstS 1e10) = S 1e10)

(* ==================== Scalar Addition Tests ==================== *)
let%test "eval (Add(ConstS 2.0, ConstS 3.0))" = (eval (Add(ConstS 2.0, ConstS 3.0)) = S 5.0)
let%test "eval (Add(ConstS 5.0, ConstS (-2.5)))" = (eval (Add(ConstS 5.0, ConstS (-2.5))) = S 2.5)
let%test "eval (Add(Add(ConstS 1.0, ConstS 2.0), ConstS 3.0))" = 
  (eval (Add(Add(ConstS 1.0, ConstS 2.0), ConstS 3.0)) = S 6.0)
let%test "eval (Add(ConstS 0.0, ConstS 0.0))" = (eval (Add(ConstS 0.0, ConstS 0.0)) = S 0.0)
let%test "eval (Add(Add(ConstS 2.5, ConstS (-1.5)), Add(ConstS 3.0, ConstS (-4.0))))" = 
  (eval (Add(Add(ConstS 2.5, ConstS (-1.5)), Add(ConstS 3.0, ConstS (-4.0)))) = S 0.0)

(* ==================== Scalar Negation Tests ==================== *)
let%test "eval (Inv(ConstS 4.0))" = (eval (Inv(ConstS 4.0)) = S (-4.0))
let%test "eval (Inv(ConstS (-2.5)))" = (eval (Inv(ConstS (-2.5))) = S 2.5)
let%test "eval (Inv(Inv(ConstS 3.14)))" = (eval (Inv(Inv(ConstS 3.14))) = S 3.14)
let%test "eval (Inv(Add(ConstS 2.0, ConstS 3.0)))" = (eval (Inv(Add(ConstS 2.0, ConstS 3.0))) = S (-5.0))
let%test "eval (Inv(Inv(Inv(ConstS 10.0))))" = (eval (Inv(Inv(Inv(ConstS 10.0)))) = S (-10.0))

(* ==================== Scalar Multiplication Tests ==================== *)
let%test "eval (ScalProd(ConstS 2.0, ConstS 3.0))" = (eval (ScalProd(ConstS 2.0, ConstS 3.0)) = S 6.0)
let%test "eval (ScalProd(ScalProd(ConstS 2.0, Inv(ConstS 4.0)), ConstS 3.0))" = 
  (eval (ScalProd(ScalProd(ConstS 2.0, Inv(ConstS 4.0)), ConstS 3.0)) = S (-24.0))
let%test "eval (ScalProd(Inv(ConstS 3.0), Mag(ConstV [4.0; 0.0; 3.0])))" = 
  (eval (ScalProd(Inv(ConstS 3.0), Mag(ConstV [4.0; 0.0; 3.0]))) = S (-15.0))
let%test "eval (ScalProd(Cond(IsZero(ConstS 0.0), Add(ConstS 2.0, ConstS 3.0), ConstS (-1.0)), ScalProd(Mag(ConstV [3.0; 4.0]), ConstS 2.0)))" = 
  (eval (ScalProd(Cond(IsZero(ConstS 0.0), Add(ConstS 2.0, ConstS 3.0), ConstS (-1.0)), ScalProd(Mag(ConstV [3.0; 4.0]), ConstS 2.0))) = S 50.0)
let%test "eval (ScalProd(Add(ScalProd(Cond(T, ConstS 2.0, ConstS 0.0), Add(ConstS 3.0, Inv(ConstS 1.0))), ScalProd(Mag(ConstV [4.0; 3.0]), Inv(ConstS 2.0))), Inv(Add(ConstS 5.0, Inv(ConstS 3.0)))))" = 
  (eval (ScalProd(Add(ScalProd(Cond(T, ConstS 2.0, ConstS 0.0), Add(ConstS 3.0, Inv(ConstS 1.0))), ScalProd(Mag(ConstV [4.0; 3.0]), Inv(ConstS 2.0))), Inv(Add(ConstS 5.0, Inv(ConstS 3.0))))) = S (12.0))

(* ==================== Magnitude Tests for Scalars ==================== *)
let%test "eval (Mag(Add(ConstS 3.0, ConstS (-4.0))))" = 
  (eval (Mag(Add(ConstS 3.0, ConstS (-4.0)))) = S 1.0)
let%test "eval (Mag(Cond(IsZero(ConstS 0.0), ConstS 5.0, ConstS (-3.0))))" = 
  (eval (Mag(Cond(IsZero(ConstS 0.0), ConstS 5.0, ConstS (-3.0)))) = S 5.0)
let%test "eval (Mag(Add(ScalProd(ConstS 3.0, ConstS (-2.0)), Inv(ConstS 1.5))))" = 
  (eval (Mag(Add(ScalProd(ConstS 3.0, ConstS (-2.0)), Inv(ConstS 1.5)))) = S 7.5)
let%test "eval (Mag(Add(Mag(ConstV [3.0; 4.0]), ScalProd(ConstS 0.5, Mag(ConstV [1.0; 2.0; 2.0])))))" = 
  (eval (Mag(Add(Mag(ConstV [3.0; 4.0]), ScalProd(ConstS 0.5, Mag(ConstV [1.0; 2.0; 2.0]))))) = S 6.5)

(* ==================== Vector Representation Tests ==================== *)
let%test "eval (ConstV [1.0; 2.0; 3.0])" = 
  (eval (ConstV [1.0; 2.0; 3.0]) = V [1.0; 2.0; 3.0])
let%test "eval (ConstV [-1.0; -2.0; -3.0])" = 
  (eval (ConstV [-1.0; -2.0; -3.0]) = V [-1.0; -2.0; -3.0])
let%test "eval (ConstV [0.0; 3.0; -5.0])" = 
  (eval (ConstV [0.0; 3.0; -5.0]) = V [0.0; 3.0; -5.0])
let%test "eval (ConstV [7.0; -4.0; 1.0])" = 
  (eval (ConstV [7.0; -4.0; 1.0]) = V [7.0; -4.0; 1.0])

(* ==================== Vector Addition Tests ==================== *)
let%test "eval (Add(ConstV [1.0; 2.0; 3.0], ConstV [4.0; 5.0; 6.0]))" = 
  (eval (Add(ConstV [1.0; 2.0; 3.0], ConstV [4.0; 5.0; 6.0])) = V [5.0; 7.0; 9.0])
let%test "eval (Add(Add(ConstV [1.0; 2.0; 3.0], ConstV [4.0; 5.0; 6.0]), ConstV [7.0; 8.0; 9.0]))" = 
  (eval (Add(Add(ConstV [1.0; 2.0; 3.0], ConstV [4.0; 5.0; 6.0]), ConstV [7.0; 8.0; 9.0])) = V [12.0; 15.0; 18.0])
let%test "eval (Add(Cond(IsZero(ConstS 0.0), ConstV [1.0; 2.0; 3.0], ConstV [4.0; 5.0; 6.0]), ConstV [7.0; 8.0; 9.0]))" = 
  (eval (Add(Cond(IsZero(ConstS 0.0), ConstV [1.0; 2.0; 3.0], ConstV [4.0; 5.0; 6.0]), ConstV [7.0; 8.0; 9.0])) = V [8.0; 10.0; 12.0])
let%test "eval (Add(ScalProd(ConstS 2.0, Add(ConstV [1.0; 1.0; 1.0], ConstV [2.0; 2.0; 2.0])), Add(ConstV [3.0; 3.0; 3.0], ConstV [4.0; 4.0; 4.0])))" = 
  (eval (Add(ScalProd(ConstS 2.0, Add(ConstV [1.0; 1.0; 1.0], ConstV [2.0; 2.0; 2.0])), Add(ConstV [3.0; 3.0; 3.0], ConstV [4.0; 4.0; 4.0]))) = V [13.0; 13.0; 13.0])

(* ==================== Vector Inversion Tests ==================== *)
let%test "eval (Inv(ConstV [1.0; 2.0; 3.0]))" = 
  (eval (Inv(ConstV [1.0; 2.0; 3.0])) = V [-1.0; -2.0; -3.0])
let%test "eval (Inv(ScalProd(ConstS 2.0, ConstV [3.0; 4.0; 5.0])))" = 
  (eval (Inv(ScalProd(ConstS 2.0, ConstV [3.0; 4.0; 5.0]))) = V [-6.0; -8.0; -10.0])

(* ==================== Dot Product Tests ==================== *)
let%test "eval (DotProd(ConstV [1.0; 2.0; 3.0], ConstV [4.0; 5.0; 6.0]))" = 
  (eval (DotProd(ConstV [1.0; 2.0; 3.0], ConstV [4.0; 5.0; 6.0])) = S 32.0)
let%test "eval (DotProd(ScalProd(ConstS 2.0, ConstV [1.0; 1.0; 1.0]), ConstV [3.0; 3.0; 3.0]))" = 
  (eval (DotProd(ScalProd(ConstS 2.0, ConstV [1.0; 1.0; 1.0]), ConstV [3.0; 3.0; 3.0])) = S 18.0)
let%test "eval (DotProd(Add(ConstV [1.0; 1.0; 1.0], ConstV [2.0; 2.0; 2.0]), Cond(IsZero(ConstS 0.0), ConstV [3.0; 3.0; 3.0], ConstV [4.0; 4.0; 4.0])))" = 
  (eval (DotProd(Add(ConstV [1.0; 1.0; 1.0], ConstV [2.0; 2.0; 2.0]), Cond(IsZero(ConstS 0.0), ConstV [3.0; 3.0; 3.0], ConstV [4.0; 4.0; 4.0]))) = S 27.0)

(* ==================== Vector Magnitude with Nested Operations ==================== *)
let%test "eval (Mag(Add(ConstV [1.0; 2.0; 3.0], ScalProd(ConstS 2.0, ConstV [4.0; 5.0; 6.0]))))" = 
  (eval (Mag(Add(ConstV [1.0; 2.0; 3.0], ScalProd(ConstS 2.0, ConstV [4.0; 5.0; 6.0])))) = S 21.2132034355964265)
let%test "eval (Mag(Cond(IsZero(ConstS 0.0), ScalProd(ConstS 3.0, ConstV [1.0; 2.0; 3.0]), ConstV [4.0; 5.0; 6.0])))" = 
  (eval (Mag(Cond(IsZero(ConstS 0.0), ScalProd(ConstS 3.0, ConstV [1.0; 2.0; 3.0]), ConstV [4.0; 5.0; 6.0]))) = S 11.2249721603218244)

(* ==================== Angle Operation with Nested Conditions ==================== *)
let%test "eval (Angle(Cond(IsZero(ConstS 0.0), ConstV [1.0; 0.0; 0.0], ConstV [0.0; 1.0; 0.0]), ConstV [0.0; 0.0; 1.0]))" = 
  (eval (Angle(Cond(IsZero(ConstS 0.0), ConstV [1.0; 0.0; 0.0], ConstV [0.0; 1.0; 0.0]), ConstV [0.0; 0.0; 1.0])) = S (Float.pi /. 2.0))
let%test "eval (Angle(Cond(IsZero(ConstS 0.0), Cond(IsZero(ConstS 1.0), ConstV [1.0; 1.0; 0.0], ConstV [0.0; 1.0; 0.0]), ConstV [0.0; 0.0; 1.0]), ConstV [1.0; 0.0; 0.0]))" = 
  (eval (Angle(Cond(IsZero(ConstS 0.0), Cond(IsZero(ConstS 1.0), ConstV [1.0; 1.0; 0.0], ConstV [0.0; 1.0; 0.0]), ConstV [0.0; 0.0; 1.0]), ConstV [1.0; 0.0; 0.0])) = S (Float.pi /. 2.0))
let%test "eval (Angle(Cond(IsZero(ConstS 0.0), Cond(IsZero(ConstS 1.0), Add(ConstV [1.0; 1.0; 0.0], ConstV [2.0; 2.0; 0.0]), ConstV [0.0; 1.0; 0.0]), ConstV [1.0; 0.0; 1.0]), ConstV [0.0; 1.0; 0.0]))" = 
  (eval (Angle(Cond(IsZero(ConstS 0.0), Cond(IsZero(ConstS 1.0), Add(ConstV [1.0; 1.0; 0.0], ConstV [2.0; 2.0; 0.0]), ConstV [0.0; 1.0; 0.0]), ConstV [1.0; 0.0; 1.0]), ConstV [0.0; 0.0; 1.0])) = S (Float.pi /. 2.0))

(* ==================== IsZero Operation for Vectors ==================== *)
let%test "eval (IsZero(ConstV [0.0; 0.0; 0.0]))" = 
  (eval (IsZero(ConstV [0.0; 0.0; 0.0])) = B true)
let%test "eval (IsZero(Cond(IsZero(ConstS 0.0), ConstV [1.0; 2.0; 3.0], ConstV [4.0; 5.0; 6.0])))" = 
  (eval (IsZero(Cond(IsZero(ConstS 0.0), ConstV [1.0; 2.0; 3.0], ConstV [4.0; 5.0; 6.0]))) = B false)

(* ==================== Cond Operation for Vector Operations ==================== *)
let%test "eval (Cond(IsZero(ConstS 0.0), ConstV [1.0; 2.0; 3.0], ConstV [4.0; 5.0; 6.0]))" = 
  (eval (Cond(IsZero(ConstS 0.0), ConstV [1.0; 2.0; 3.0], ConstV [4.0; 5.0; 6.0])) = V [1.0; 2.0; 3.0])
let%test "eval (Cond(IsZero(ConstS 1.0), Add(ConstV [1.0; 1.0; 1.0], ConstV [2.0; 2.0; 2.0]), ConstV [3.0; 3.0; 3.0]))" = 
  (eval (Cond(IsZero(ConstS 1.0), Add(ConstV [1.0; 1.0; 1.0], ConstV [2.0; 2.0; 2.0]), ConstV [3.0; 3.0; 3.0])) = V [3.0; 3.0; 3.0])
let%test "eval (Cond(IsZero(ConstS 0.0), ScalProd(ConstS 2.0, ConstV [1.0; 1.0; 1.0]), ConstV [0.0; 0.0; 0.0]))" = 
  (eval (Cond(IsZero(ConstS 0.0), ScalProd(ConstS 2.0, ConstV [1.0; 1.0; 1.0]), ConstV [0.0; 0.0; 0.0])) = V [2.0; 2.0; 2.0])

(* Invalid Testcase for eval function *)

let%test "eval (Add (T, ConstS 1.0)) raises Wrong (Add (T, ConstS 1.0))" =
  (try
    let _ = eval (Add (T, ConstS 1.0)) in false
  with
  | Wrong e -> e = Add (T, ConstS 1.0)
  | _ -> false)

let%test "eval (Add (ConstV [1.0; 2.0], ConstV [3.0])) raises Wrong (Add (ConstV [1.0; 2.0], ConstV [3.0]))" =
  (try
    let _ = eval (Add (ConstV [1.0; 2.0], ConstV [3.0])) in false
  with
  | Wrong e -> e = Add (ConstV [1.0; 2.0], ConstV [3.0])
  | _ -> false)

let%test "eval (DotProd (ConstS 2.0, ConstV [1.0; 2.0])) raises Wrong (DotProd (ConstS 2.0, ConstV [1.0; 2.0]))" =
  (try
    let _ = eval (DotProd (ConstS 2.0, ConstV [1.0; 2.0])) in false
  with
  | Wrong e -> e = DotProd (ConstS 2.0, ConstV [1.0; 2.0])
  | _ -> false)

let%test "eval (Angle (ConstS 1.0, ConstS 2.0)) raises Wrong (Angle (ConstS 1.0, ConstS 2.0))" =
  (try
    let _ = eval (Angle (ConstS 1.0, ConstS 2.0)) in false
  with
  | Wrong e -> e = Angle (ConstS 1.0, ConstS 2.0)
  | _ -> false)

let%test "eval (Cond (ConstS 1.0, ConstS 2.0, ConstS 3.0)) raises Wrong (Cond (ConstS 1.0, ConstS 2.0, ConstS 3.0))" =
  (try
    let _ = eval (Cond (ConstS 1.0, ConstS 2.0, ConstS 3.0)) in false
  with
  | Wrong e -> e = Cond (ConstS 1.0, ConstS 2.0, ConstS 3.0)
  | _ -> false)

(* Deeply Nested Invalid Cases *)
let%test "eval (DotProd (Add (ConstV [1.0; 2.0], ConstV [3.0]), ConstV [4.0; 5.0])) raises Wrong (DotProd (Add (ConstV [1.0; 2.0], ConstV [3.0]), ConstV [4.0; 5.0]))" =
  (try
    let _ = eval (DotProd (Add (ConstV [1.0; 2.0], ConstV [3.0]), ConstV [4.0; 5.0])) in false
  with
  | Wrong _ -> true
  | _ -> false)

let%test "eval (ScalProd (Add (T, F), ConstV [1.0; 2.0])) raises Wrong (ScalProd (Add (T, F), ConstV [1.0; 2.0]))" =
  (try
    let _ = eval (ScalProd (Add (T, F), ConstV [1.0; 2.0])) in false
  with
  | Wrong e -> e = ScalProd (Add (T, F), ConstV [1.0; 2.0])
  | _ -> false)

let%test "eval (Mag (Add (ConstS 2.0, ConstV [3.0; 4.0]))) raises Wrong (Mag (Add (ConstS 2.0, ConstV [3.0; 4.0])))" =
  (try
    let _ = eval (Mag (Add (ConstS 2.0, ConstV [3.0; 4.0]))) in false
  with
  | Wrong _ -> true
  | _ -> false)

let%test "eval (Angle (DotProd (ConstV [1.0; 2.0], ConstV [3.0]), ConstV [4.0; 5.0])) raises Wrong (Angle (DotProd (ConstV [1.0; 2.0], ConstV [3.0]), ConstV [4.0; 5.0]))" =
  (try
    let _ = eval (Angle (DotProd (ConstV [1.0; 2.0], ConstV [3.0]), ConstV [4.0; 5.0])) in false
  with
  | Wrong _ -> true
  | _ -> false)

let%test "eval (Cond (T, Add (ConstS 2.0, ConstS 3.0), ConstV [1.0; 2.0])) raises Wrong (Cond (T, Add (ConstS 2.0, ConstS 3.0), ConstV [1.0; 2.0]))" =
  (try
    let _ = eval (Cond (T, Add (ConstS 2.0, ConstS 3.0), ConstV [1.0; 2.0])) in false
  with
  | Wrong e -> e = Cond (T, Add (ConstS 2.0, ConstS 3.0), ConstV [1.0; 2.0])
  | _ -> false)

let%test "eval (ScalProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0])) raises Wrong (ScalProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0]))" =
  (try
    let _ = eval (ScalProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0])) in false
  with
  | Wrong e -> e = ScalProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0])
  | _ -> false)