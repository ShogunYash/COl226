{
  exception Lexical_error of string  
  open Str
  open Parser

  let line_num = ref 1
  let parse_Imatrix matrix_str =
    (* Remove outer brackets *)
    let clean_str = String.sub matrix_str 1 (String.length matrix_str - 2) in
    
    (* Split into rows by looking for the pattern ],[  *)
    let row_pattern = Str.regexp "\\],\\[" in
    let rows = Str.split row_pattern clean_str in
    
    (* Clean up the first and last row which might still have brackets *)
    let clean_rows = List.map (fun row ->
      Str.global_replace (Str.regexp "\\[\\|\\]") "" row
    ) rows in
    
    (* For each row, split by commas and convert to integers *)
    List.map (fun row ->
      let values = Str.split (Str.regexp ",") row in
      List.map int_of_string values
    ) clean_rows

  let parse_Fmatrix matrix_str =
    (* Remove outer brackets *)
    let clean_str = String.sub matrix_str 1 (String.length matrix_str - 2) in
    
    (* Split into rows by looking for the pattern ],[  *)
    let row_pattern = Str.regexp "\\],\\[" in
    let rows = Str.split row_pattern clean_str in
    
    (* Clean up the first and last row which might still have brackets *)
    let clean_rows = List.map (fun row ->
      Str.global_replace (Str.regexp "\\[\\|\\]") "" row
    ) rows in
    
    (* For each row, split by commas and convert to integers *)
    List.map (fun row ->
      let values = Str.split (Str.regexp ",") row in
      List.map float_of_string values
    ) clean_rows

}

(* Regular expression macros *)
let whitespace = [' ' '\t' '\r']
let newline = '\r' | '\n' | "\r\n" | "\\n"
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let alnum = alpha | digit | '_' 
let id = alpha alnum*
let file = alnum* | '.' | alpha*
let int_literal = digit+
let sign = ['+' '-']?
let exp = ('e'|'E') sign digit+
let float_literal = digit+ '.' digit* exp? | digit+ exp | digit* '.' digit+ exp?

rule token = parse
  (* Standard tokens; flushing pending tokens is handled in get_all_tokens *)  
  | whitespace+ { token lexbuf }
  | newline+ {  incr line_num; token lexbuf }
  | "//" [^'\n']* { token lexbuf }
  | "/*"        { comment 0 lexbuf }
  (* | "Input(" (file as name) ")" { IFILE name } *)
  (* | "Print(" (file as name) ")" { PFILE name } *)
  | "Input"     { INPUT }
  | "Print"     { PRINT }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "for"       { FOR }
  | "to"        { TO }
  | "while"     { WHILE }
  | "do"        { DO }
  | "end"       { END }
  | "+"         { IPLUS }
  | "/-"        { IMINUS }
  | "*"         { ITIMES }
  | "/"         { IDIVIDE }
  | "mod"       { IMODULO }
  | "mod_float" { FMODULO }
  | "+."        { FPLUS }
  | "-."        { FMINUS }
  | "*."        { FTIMES }
  | "/."        { FDIVIDE }
  | "pow"        { POWER } 
  | "abs"       { ABS }
  | ":="        { ASSIGN }
  | "="         { EQ }
  | "~="        { NEQ }
  | "<"         { LT }
  | ">"         { GT }
  | "<="        { LEQ }
  | ">="        { GEQ }
  | "&&"        { AND }
  | "||"        { OR }
  | "~"         { NOT }
  | "^"         { XOR }
  | "int_to_float"    { INT_TO_FLOAT }
  | "crt_intvec"      { CRTIV }
  | "crt_fltvec"      { CRTFV }
  | "crt_intmat"      { CRTIM }
  | "crt_fltmat"      { CRTFM }
  | "trp"       { TRANSPOSE }
  | "det"       { DET }
  | "dim"       { DIMENSION }
  | "mag"       { MAGNITUDE }
  | "row_access"{ ROW_ACCESS }
  | "angle"     { ANGLE }
  | "("         { LPAREN }
  | ")"         { RPAREN }
  | "["         { LBRACKET }
  | "]"         { RBRACKET }
  | "{"         { LBRACE }
  | "}"         { RBRACE }
  | ";"         { SEMICOLON }
  | ","         { COMMA }
  | "true"      { BOOL_LITERAL true }
  | "false"     { BOOL_LITERAL false }
  | id as name {  ID name }
  | digit+ as size newline '['(sign digit+ (','sign digit+)* as values)?']'{
    let size1 = int_of_string size in
    let list_values = match values with
      | None -> []
      | Some v -> List.map int_of_string (String.split_on_char ',' v)
    in
    IVECTOR (size1, list_values)
  }
  | digit+ as size newline '['(sign float_literal (','sign float_literal)* as values)?']'{
    let size1 = int_of_string size in
    let list_values = match values with
      | None -> []
      | Some v -> List.map float_of_string (String.split_on_char ',' v)
    in
    FVECTOR (size1, list_values)
  }
  | digit+ as row ',' (digit+ as col) newline ('[' ('['(sign digit+ (','sign digit+)*)']') (',' '['(sign digit+ (','sign digit+)*)']')* ']' as matrix){
    let row1 = int_of_string row in
    let col1 = int_of_string col in
    let matrix_values = parse_Imatrix matrix in
    IMATRIX (row1, col1, matrix_values)
  }
  | digit+ as row ',' (digit+ as col) newline ('[' ('['(sign float_literal (','sign float_literal)*)']') (',' '['(sign float_literal (','sign float_literal)*)']')* ']' as matrix){
    let row1 = int_of_string row in
    let col1 = int_of_string col in
    let matrix_values = parse_Fmatrix matrix in
    FMATRIX (row1, col1, matrix_values)
  }
  | sign float_literal as f {  FLOAT_LITERAL (float_of_string f) }
  | sign int_literal as i { INT_LITERAL (int_of_string i) }
  | '\"' [^'\"']* '\"' as s {
    STRING_LITERAL (String.sub s 1 (String.length s - 2))
  }
  | '\"' [^'\"']* eof { 
    raise (Lexical_error (Printf.sprintf "Unclosed string literal at line %d" !line_num))
  }
  (* Add this error handling rule *)
  | eof  { EOF }
  (* | _ {raise (SyntaxError ("Lexer - Illegal character: " ^ Lexing.lexeme lexbuf)) } *)

  | _ as c { 
      raise (Lexical_error (Printf.sprintf "Unexpected character '%c' at line %d" c !line_num)) 
  }
  (* | _ as c { 
    raise (Lexical_error (Printf.sprintf 
      "Unexpected character '%c' (hex: 0x%02x) at line %d" 
      c (Char.code c) !line_num)) 
} *)
and comment level = parse
  | eof {  EOF }
  | "/*" { comment (level + 1) lexbuf }
  | "*/" { if level = 0 then token lexbuf else comment (level - 1) lexbuf }
  | newline { incr line_num; comment level lexbuf }
  | _ { comment level lexbuf }