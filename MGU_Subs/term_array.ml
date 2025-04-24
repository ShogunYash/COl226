(* Term module implementation using arrays *)

exception NEGATIVE_ARITY
exception NOT_UNIFIABLE

module Term = struct
  (* Type definitions *)
  type variable = string
  type symbol = string * int
  type term = 
    | V of variable 
    | Node of symbol * (term array)

  (* A signature is represented as an array of symbols with their arities. *)
  type signature = symbol array

  (* Check that a signature is valid:
     - every arity is nonnegative,
     - no two symbols share the same string.
  *)
  let check_sig (sig_arr: signature) : bool =
    (* Check for non-negative arities *)
    let arity_ok = Array.for_all (fun (_, arity) -> arity >= 0) sig_arr in
    
    (* Check for duplicate symbols *)
    let symbol_names = Array.map fst sig_arr in
    let len = Array.length symbol_names in
    let has_duplicates = 
      let sorted = Array.copy symbol_names in
      Array.sort String.compare sorted;
      let has_dup = ref false in
      if len > 1 then
        for i = 1 to len - 1 do
          if sorted.(i) = sorted.(i-1) then has_dup := true
        done;
      !has_dup
    in
    
    arity_ok && not has_duplicates

  (* Lookup the arity of a symbol in a signature *)
  let find_arity (sig_arr: signature) (sym: string) : int option =
    try
      let found = ref false in
      let result = ref 0 in
      for i = 0 to Array.length sig_arr - 1 do
        let (s, arity) = sig_arr.(i) in
        if s = sym then begin
          found := true;
          result := arity
        end
      done;
      if !found then Some !result else None
    with Not_found -> None
  
  (* Check that a term is well-formed with respect to a valid signature. *)
  let rec wfterm (sig_arr: signature) (t: term) : bool =
    match t with
    | V _ -> true
    | Node ((s, arity), children) ->
        match find_arity sig_arr s with
        | Some expected_arity ->
            arity = expected_arity &&
            Array.length children = arity &&
            Array.for_all (wfterm sig_arr) children
        | None -> false

  (* Return the height of term t *)
  let rec ht (t: term) : int =
    match t with
    | V _ -> 0
    | Node (_, children) ->
        if Array.length children = 0 then 1
        else 1 + Array.fold_left max 0 (Array.map ht children)

  (* Return the number of nodes in term t *)
  let rec size (t: term) : int =
    match t with
    | V _ -> 1
    | Node (_, children) ->
        1 + Array.fold_left (+) 0 (Array.map size children)

  (* Return the set of variables appearing in term t *)
  module VarSet = Set.Make(String)
  
  let rec vars (t: term) : VarSet.t =
    match t with
    | V v -> VarSet.singleton v
    | Node (_, children) ->
        Array.fold_left 
          (fun acc child -> VarSet.union acc (vars child))
          VarSet.empty children

  (* A substitution is represented as an association list from variables to terms *)
  type substitution = (variable * term) list

  (* Apply substitution sigma to term t *)
  let rec subst (sigma: substitution) (t: term) : term =
    match t with
    | V v ->
        (try List.assoc v sigma with Not_found -> V v)
    | Node (sym, children) ->
        Node (sym, Array.map (subst sigma) children)

  (* Compose substitutions *)
  let compose_subst (s1: substitution) (s2: substitution) : substitution =
    let s1' = List.map (fun (v, t) -> (v, subst s2 t)) s1 in
    let s2_filtered =
      List.filter (fun (v, _) -> not (List.exists (fun (v', _) -> v = v') s1)) s2
    in
    s1' @ s2_filtered

  (* Check whether variable v occurs anywhere in term t *)
  let rec occurs (v: variable) (t: term) : bool =
    match t with
    | V v' -> v = v'
    | Node (_, children) ->
        Array.exists (occurs v) children

  (* Compute the most general unifier of t1 and t2.
     If unification fails, raise NOT_UNIFIABLE.
  *)
  let mgu (t1: term) (t2: term) : substitution =
    let rec unify (s: substitution) (t1: term) (t2: term) : substitution =
      let t1' = subst s t1 in
      let t2' = subst s t2 in
      match t1', t2' with
      | V x, V y when x = y -> s
      | V x, t | t, V x ->
          if occurs x t then raise NOT_UNIFIABLE
          else compose_subst s [(x, t)]
      | Node ((sym1, _), args1), Node ((sym2, _), args2) ->
          if sym1 <> sym2 || Array.length args1 <> Array.length args2 then
            raise NOT_UNIFIABLE
          else
            let n = Array.length args1 in
            let result = ref s in
            for i = 0 to n - 1 do
              result := unify !result args1.(i) args2.(i)
            done;
            !result
    in
    unify [] t1 t2

  (* Edit a term: replace the subtree at position pos with new_sub *)
  let rec edit (t: term) (pos: int list) (new_sub: term) : term =
    match pos, t with
    | [], _ -> new_sub
    | i :: rest, Node (sym, children) ->
        if i < 0 || i >= Array.length children then
          failwith "edit: invalid position"
        else
          let new_children = Array.copy children in
          new_children.(i) <- edit new_children.(i) rest new_sub;
          Node (sym, new_children)
    | _, V _ -> failwith "edit: invalid position (encountered variable)"

  (* In-place substitution: replace variables in a term by given terms *)
  let subst_in_place (sigma: substitution) (t: term) : unit =
    let rec replace (t: term) : unit =
      match t with
      | V _ -> ()  (* Can't modify a leaf in place *)
      | Node (_, children) ->
          for i = 0 to Array.length children - 1 do
            match children.(i) with
            | V v ->
                (try
                  let new_t = List.assoc v sigma in
                  children.(i) <- new_t
                with Not_found -> ())
            | Node _ as sub_t -> replace sub_t
          done
    in
    replace t
        
  (* Convert a term to a string representation *)
  let rec string_of_term (t: term) : string =
    match t with
    | V x -> x
    | Node ((sym, _), children) ->
        if Array.length children = 0 then
          sym
        else
          "(" ^ sym ^ "[" ^ 
          String.concat "; " (Array.to_list (Array.map string_of_term children)) ^ 
          "])"
  
  (* Convert a substitution to a string representation *)
  let string_of_sigma (sigma: substitution) : string =
    let pair_to_string (x, t) = x ^ " -> " ^ string_of_term t in
    "[" ^ String.concat "; " (List.map pair_to_string sigma) ^ "]"
  
  (* Test a most general unification *)
  let test_mgu (n: int) (t1: term) (t2: term) : unit =
    try
      let sigma = mgu t1 t2 in
      Printf.printf "Test Case: %d\nUnification successful.\nSigma: %s\n\n" n (string_of_sigma sigma)
    with NOT_UNIFIABLE ->
      Printf.printf "Test Case: %d\nNot unifiable.\n\n" n
  
  (* Check if two substitutions are equivalent *)
  let same_sigma (sigma1: substitution) (sigma2: substitution) : bool =
    let sort_and_equal s1 s2 =
      List.sort (fun (x1, _) (x2, _) -> String.compare x1 x2) s1 =
      List.sort (fun (x1, _) (x2, _) -> String.compare x1 x2) s2
    in
    sort_and_equal sigma1 sigma2
  
  (* Test if a signature is valid *)
  let test_sig (n: int) (sig_arr: signature) : unit =
    try
      let result = check_sig sig_arr in
      if result then
        Printf.printf "Test Case: %d\nValid Signature\n\n" n
      else
        Printf.printf "Test Case: %d\nInvalid Signature\n\n" n
    with NEGATIVE_ARITY ->
      Printf.printf "Test Case: %d\nNegative Arity\n\n" n
  
  (* Test if a term is well-formed *)
  let test_wfterm (n: int) (sig_arr: signature) (t: term) : unit =
    try
      let result = wfterm sig_arr t in
      if result then
        Printf.printf "Test Case: %d\nValid Term\n\n" n
      else
        Printf.printf "Test Case: %d\nInvalid Term\n\n" n
    with NEGATIVE_ARITY ->
      Printf.printf "Test Case: %d\nInvalid Term\n\n" n
  
  (* Test height, size, and variables of a term *)
  let test_ht_size_vars (n: int) (t: term) : unit =
    let vars_list = VarSet.elements (vars t) in
    let vars_str = String.concat ", " vars_list in
    Printf.printf "TestCase %d\nHeight: %d\nSize: %d\nVars: %s\n\n" n (ht t) (size t) vars_str
  
  (* Test composition of substitutions *)
  let test_compo (n: int) (sub1: substitution) (sub2: substitution) : unit =
    Printf.printf "Testcase %d\n%s\n\n" n (string_of_sigma (compose_subst sub1 sub2))
  
  (* Test substitution application *)
  let test_subst (n: int) (sub: substitution) (t: term) : unit =
    Printf.printf "Testcase %d\n%s\n\n" n (string_of_term (subst sub t))
  
  (* Test the edit function *)
  let test_edit (n: int) (t: term) (pos: int list) (new_sub: term) : unit =
    try
      let result = edit t pos new_sub in
      Printf.printf "Test Case: %d\nEdit successful.\nOriginal: %s\nPosition: %s\nNew subtree: %s\nResult: %s\n\n" 
        n (string_of_term t) (String.concat "." (List.map string_of_int pos)) 
        (string_of_term new_sub) (string_of_term result)
    with Failure msg ->
      Printf.printf "Test Case: %d\nEdit failed: %s\n\n" n msg

  (* Test the in-place substitution function *)
  let test_subst_in_place (n: int) (t: term) (sigma: substitution) : unit =
    let t_copy = subst [] t in  (* Create a deep copy of t *)
    Printf.printf "Test Case: %d\nIn-place substitution\nOriginal: %s\nSubstitution: %s\n" 
      n (string_of_term t_copy) (string_of_sigma sigma);
    subst_in_place sigma t_copy;
    Printf.printf "Result: %s\n\n" (string_of_term t_copy)
  
  (* Print a term with indentation *)
  let print_term (t: term) : unit =
    let rec print_t indent t =
      match t with
      | V s -> Printf.printf "%s%s\n" indent s
      | Node ((sym, _), children) ->
          Printf.printf "%s%s\n" indent sym;
          Array.iter (print_t (indent ^ "  ")) children
    in
    print_t "" t
end
