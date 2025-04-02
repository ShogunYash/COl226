(* main.ml - Read input file, parse it, type check it, and execute it *)
open Ast
open Lexer
open Parser
open Ast_type_checker
open Interpreter  (* Import the interpreter module *)

(* Check that required modules are available *)
let () =
  try ignore (Hashtbl.hash 0) with Not_found -> 
    failwith "Required Str module not available. Install with 'opam install str'"

(* Parse a string and return the AST *)
let parse_string str =
  let lexbuf = Lexing.from_string str in
  try
    Parser.program Lexer.token lexbuf
  with
  | Lexer.Lexical_error msg ->
      failwith ("Lexical error: " ^ msg)
  | Parsing.Parse_error ->
      let pos = lexbuf.Lexing.lex_curr_p in
      failwith (Printf.sprintf "Syntax error at line %d, character %d"
                 pos.Lexing.pos_lnum
                 (pos.Lexing.pos_cnum - pos.Lexing.pos_bol))

(* Helper function to determine if a message is an error *)
let is_error_message msg =
  String.length msg >= 10 && String.sub msg 0 10 = "Type error" ||
  String.length msg >= 6 && String.sub msg 0 6 = "Error:"

(* Parse, type check, and interpret a string, then print results *)
let parse_type_check_and_interpret str =
  try
    let ast = parse_string str in
    let ast_string = string_of_program ast in
    
    Printf.printf "Input:\n%s\n\n" str;
    Printf.printf "AST:\n%s\n\n" ast_string;
    
    (* Type check the AST *)
    let result = type_check ast in
    if is_error_message result then
      Printf.printf "Type Check: ❌ %s\n\n" result
    else begin
      Printf.printf "Type Check: ✅ %s\n\n" result;
      (* If type check passes, interpret the program *)
      Printf.printf "Executing program:\n";
      interpret ast;
      Printf.printf "\nExecution complete.\n\n"
    end
  with
  | Failure msg -> Printf.printf "Error: %s\n\n" msg

(* Parse, type check a string, then print results *)
let parse_type_check_and_print str =
  try
    let ast = parse_string str in
    let ast_string = string_of_program ast in
    
    Printf.printf "Input:\n%s\n\n" str;
    Printf.printf "AST:\n%s\n\n" ast_string;
    
    (* Type check the AST *)
    let result = type_check ast in
    if is_error_message result then
      Printf.printf "Type Check: ❌ %s\n\n" result
    else
      Printf.printf "Type Check: ✅ %s\n\n" result
  with
  | Failure msg -> Printf.printf "Error parsing: %s\n\n" msg

(* Parse a string, convert to AST representation, and print it *)
let parse_and_print str =
  try
    let ast = parse_string str in
    let ast_string = string_of_program ast in
    Printf.printf "Input:\n%s\n\nAST:\n%s\n\n" str ast_string
  with
  | Failure msg -> Printf.printf "Error parsing: %s\n\n" msg

(* Parse a source file and return the AST *)
let parse_file filename =
  (* First read the file into lines *)
  let read_lines file =
    let ic = open_in file in
    let rec aux acc =
      try
        let line = input_line ic in
        aux (line :: acc)
      with End_of_file ->
        close_in ic;
        List.rev acc
    in
    aux []
  in
  
  let lines = read_lines filename in
  
  (* Now parse the file *)
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  
  (* Set position info including filename *)
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with 
                              Lexing.pos_fname = filename };
  
  try
    let ast = Parser.program Lexer.token lexbuf in
    close_in ic;
    ast
  with
  | Parsing.Parse_error ->
      let pos = lexbuf.Lexing.lex_curr_p in
      let line_num = pos.Lexing.pos_lnum in
      let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
      close_in ic;
      
      (* Get the actual line content *)
      let line_content = 
        if line_num > 0 && line_num <= List.length lines then
          List.nth lines (line_num - 1)
        else "<<line content not available>>"
      in
      
      (* Create pointer *)
      let pointer = String.make col ' ' ^ "^" in
      
      failwith (Printf.sprintf "Syntax error at line %d, column %d:\n%s\n%s"
                line_num col line_content pointer)

(* Parse and type check a source file *)
let parse_and_check_file filename =
  let ast = parse_file filename in
  let result = type_check ast in
  if is_error_message result then begin
    Printf.printf "Type Check: ❌ %s\n" result;
    exit 1
  end else begin
    Printf.printf "Type Check: ✅ %s\n" result;
    ast
  end

(* Parse, type check, and interpret a source file *)
let parse_check_and_interpret_file filename =
  let ast = parse_file filename in
  let result = type_check ast in
  if is_error_message result then begin
    Printf.printf "Type Check: ❌ %s\n" result;
    exit 1
  end else begin
    Printf.printf "Type Check: ✅ %s\n" result;
    Printf.printf "Executing program:\n";
    interpret ast;
    Printf.printf "\nExecution complete.\n";
  end

(* Run test cases *)
let run_tests ~with_type_check ~with_interpret =
  print_endline "Running test cases...\n";
  
  let print_func = 
    if with_interpret then parse_type_check_and_interpret 
    else if with_type_check then parse_type_check_and_print 
    else parse_and_print 
  in
  
  (* Simple statements *)
  print_func "x := 10;";
  
  (* Arithmetic operations *)
  print_func "y := 5 + 10;";
  print_func "z := 20 /- 5;";
  
  (* Conditional statement *)
  print_func "x := 15;\n if x < 10 then { Print(x); } else { Print(0); } end";
  
  (* For loop *)
  print_func "sum := -1000; for i := 1 to 10 do { sum := sum + i; } end";
  
  (* Vector operations *)
  print_func "v := 3\n[6,7,8];\nv := v[0];";
  
  (* Function calls *)
  print_func "v:=1,1\n[[1]];\ndim_v := dim(v);";
  
  (* Multiple statements *)
  print_func "x := 5; y := 10; z := x + y;";

  (* Comment parse *)
  print_func "// This is a comment";
  print_func "/* This is a comment  Another comment  x:= 10;*/ \"Check this\";";
  
  (* Custom inputs *)
  print_func "v:=3\n[1,2,3]\n;\nPrint(v);\nelem := v[1];\nPrint(elem);";
  print_func "x:= 58;\nif x < 20 then \n { Print(x); } \n else { Print(); } \n end";
  print_func "x:= 10;\nif x < 20 then \n if x < 5 then \n { Print(x); } \n else Print(0); \n end \n else { Print(0); } \n end";
  print_func "Print(0);";
  print_func "Print(\"WorkingPeople\");";
  print_func "x := 10 + 7 / 2;";
  print_func "x := 78;\nsum := 0;\nj :=0;\nfor i := x to j do \n { sum := sum + i; \n x := 10 + 19; } \n end";
  print_func "/-1;";
  print_func "x := -10.9;";
  print_func "m:=10;\nx := /-m;";
  print_func "Print();";
  print_func "V := 1,1\n[[1]];\nrow_access(V,0);";
  (*  Testcases Completed  *)
  print_endline "Test cases completed."

(* Test cases specifically for the type checker *)
let run_type_check_tests () =
  print_endline "Running type checker test cases...\n";
  
  (* Helper function for running type check tests *)
  let test_type_check code expected_result =
    try
      let ast = parse_string code in
      let result = type_check ast in
      let success = (result = expected_result) || 
                    (expected_result = "error" && is_error_message result) in
      Printf.printf "Test: %s\nResult: %s\nExpected: %s\nStatus: %s\n\n"
        (if String.length code > 50 then String.sub code 0 47 ^ "..." else code)
        result
        (if expected_result = "error" then "Error expected" else expected_result)
        (if success then "✅ PASS" else "❌ FAIL")
    with
    | Failure msg -> 
        let success = expected_result = "error" in
        Printf.printf "Test: %s\nResult: Error: %s\nExpected: %s\nStatus: %s\n\n"
          (if String.length code > 50 then String.sub code 0 47 ^ "..." else code)
          msg
          (if expected_result = "error" then "Error expected" else expected_result)
          (if success then "✅ PASS" else "❌ FAIL")
  in
  
  (* Basic declarations and type consistency *)
  test_type_check "let x := 5;" "Program type checks successfully";
  test_type_check "let x := 5; x := 10;" "Program type checks successfully";
  test_type_check "let x := 5; x := 3.14;" "error"; (* Type mismatch *)
  
  (* Variable redeclaration checks *)
  test_type_check "let x := 5; let x := 10;" "error"; (* Redeclaration not allowed *)
  test_type_check "let x := true; let y := 10;" "Program type checks successfully";
  
  (* Loop iterator variable tests *)
  test_type_check "for i := 1 to 5 do { let x := i; } end" "Program type checks successfully";
  test_type_check "let i := 10; for i := 1 to 5 do { Print(i); } end" "error"; (* Iterator shadows existing variable *)
  test_type_check "for i := 1 to 5 do {} end; let i := 10;" "Program type checks successfully"; (* i no longer in scope *)
  
  (* Nested scopes *)
  test_type_check "if true then { let x := 5; } else { let y := 10; } end" "Program type checks successfully";
  test_type_check "let x := 5; if x > 0 then { let y := 10; } end; Print(y);" "error"; (* y not in scope *)
  
  (* Type checking for operations *)
  test_type_check "let x := 5 + 10;" "Program type checks successfully";
  test_type_check "let x := 5 + 3.14;" "error"; (* Type mismatch in addition *)
  test_type_check "let v1 := 3\n[1,2,3]; let v2 := 3\n[4,5,6]; let v3 := v1 + v2;" "Program type checks successfully";
  test_type_check "let v1 := 3\n[1,2,3]; let v2 := 2\n[4,5]; let v3 := v1 + v2;" "error"; (* Vector dimension mismatch *)
  
  (* Matrix operations *)
  test_type_check "let m1 := 2,2\n[[1,2],[3,4]]; let m2 := 2,2\n[[5,6],[7,8]]; let m3 := m1 + m2;" "Program type checks successfully";
  test_type_check "let m1 := 2,2\n[[1,2],[3,4]]; let m2 := 3,2\n[[5,6],[7,8],[9,10]]; let m3 := m1 + m2;" "error"; (* Matrix dimension mismatch *)
  
  (* Mixed operations *)
  test_type_check "let x := 5; let y := true; let z := x + y;" "error"; (* Cannot add int and bool *)
  test_type_check "let x := 5; let y := true; if y then { let z := x + 10; } end" "Program type checks successfully";
  
  (* Let declaration and assignment semantics *)
  test_type_check "let x := 10; let y := x + 5;" "Program type checks successfully";
  test_type_check "x := 10;" "error"; (* Assignment to undeclared variable *)
  test_type_check "let x := 10; y := x + 5;" "error"; (* Assignment to undeclared variable *)
  
  (* Function application and type checking *)
  test_type_check "let v := 3\n[1,2,3]; let m := mag(v);" "error"; (* mag not a function, should use Magnitude(v) *)
  test_type_check "let v := 3\n[1,2,3]; let m := Magnitude(v);" "Program type checks successfully";
  
  (* Vector/Matrix access *)
  test_type_check "let v := 3\n[1,2,3]; let x := v[0];" "Program type checks successfully";
  test_type_check "let v := 3\n[1,2,3]; let x := v[true];" "error"; (* Index must be int *)
  test_type_check "let m := 2,2\n[[1,2],[3,4]]; let x := m[0,1];" "Program type checks successfully";
  
  print_endline "Type checker test cases completed."

(* Main function *)
let () =
  let process_file = ref true in
  let run_test_cases = ref false in
  let with_type_check = ref false in
  let type_check_only = ref false in
  let interpret = ref false in
  let run_type_tests = ref false in  (* New flag for running type checker tests *)
  
  Arg.parse [
    ("-test", Arg.Set run_test_cases, "Run built-in test cases");
    ("-nofile", Arg.Clear process_file, "Don't process input.txt");
    ("-typecheck", Arg.Set with_type_check, "Enable type checking for tests");
    ("-check-only", Arg.Set type_check_only, "Only perform type checking on input file");
    ("-interpret", Arg.Set interpret, "Interpret the program after type checking");
    ("-test-type", Arg.Set run_type_tests, "Run type checker specific tests");  (* New option *)
  ] (fun _ -> ()) "Matrix/Vector Language Parser and Interpreter";
  
  (* Run type checker tests if requested *)
  if !run_type_tests then run_type_check_tests();
  
  if !run_test_cases then run_tests ~with_type_check:!with_type_check ~with_interpret:!interpret;
  
  if !process_file then
    try
      (* Parse input file *)
      let input_file = "input.txt" in
      let output_file = "output.txt" in
      
      Printf.printf "Parsing file: %s\n" input_file;
      
      if !interpret then
        parse_check_and_interpret_file input_file
      else if !type_check_only || !with_type_check then
        let ast = parse_and_check_file input_file in
        if not !type_check_only then begin
          (* Convert AST to string representation *)
          Printf.printf "Converting AST to string representation\n";
          let ast_string = string_of_program ast in
          
          (* Write AST to output file *)
          Printf.printf "Writing AST to: %s\n" output_file;
          let oc = open_out output_file in
          output_string oc ast_string;
          close_out oc;
          
          print_endline "Processing complete. AST written to output.txt"
        end else
          print_endline "Type checking complete."
      else begin
        let ast = parse_file input_file in
        (* Convert AST to string representation *)
        Printf.printf "Converting AST to string representation\n";
        let ast_string = string_of_program ast in
        
        (* Write AST to output file *)
        Printf.printf "Writing AST to: %s\n" output_file;
        let oc = open_out output_file in
        output_string oc ast_string;
        close_out oc;
        
        print_endline "Processing complete. AST written to output.txt"
      end
    with
    | Sys_error msg ->
        prerr_endline ("System error: " ^ msg);
        exit 1
    | Failure msg ->
        prerr_endline ("Error: " ^ msg);
        exit 1
    | e ->
        prerr_endline ("Unexpected error: " ^ Printexc.to_string e);
        exit 1

