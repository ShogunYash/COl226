{
  open Token
  exception Lexical_error of string  
  let line_num = ref 1
  let file_context = ref false  (* file context flag *)
  let debug_print_identifier name =
      Printf.printf "Parsing identifier: %s (file_context: %b)\n" name !file_context
    ;;
}

(* Regular expression macros *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let alnum = alpha | digit | '_' 
let whitespace = [' ' '\t' '\r']
let newline = '\n'
let id = alpha alnum*
let file = alnum*?'.'alpha

let sign = ['+' '-']?
let exp = ('e'|'E') sign digit+
let int_literal = digit+
let float_literal = digit+ '.' digit* exp? | digit+ exp | digit* '.' digit+ exp?

rule token = parse
  (* Standard tokens; flushing pending tokens is handled in get_all_tokens *)  
  | whitespace { token lexbuf }
  | newline {  incr line_num; token lexbuf }
  | "//" [^'\n']* { token lexbuf }
  | "/*" {  comment 0 lexbuf }
  | "Input" {  file_context := true; INPUT }
  | "Print" {  file_context := true; PRINT }
  | "if" {  IF }
  | "then" {  THEN }
  | "else" {  ELSE }
  | "for" {  FOR }
  | "to" {  TO }
  | "while" {  WHILE }
  | "do" {  DO }
  | "end" {  END }
  | "+" {  IPLUS }
  | "-" {  IMINUS }
  | "*" {  ITIMES }
  | "/" {  IDIVIDE }
  | "mod" {  IMODULO }
  | "+." {  FPLUS }
  | "-." {  FMINUS }
  | "*." {  FTIMES }
  | "/." {  FDIVIDE }
  | "mod_float" {  FMODULO }
  | "abs" {  ABS }
  | ":=" {  ASSIGN }
  | "=" {  EQ }
  | "~=" {  NEQ }
  | "<" {  LT }
  | ">" {  GT }
  | "<=" {  LEQ }
  | ">=" {  GEQ }
  | "&&" {  AND }
  | "||" {  OR }
  | "~" {  NOT }
  | "'" {  TRANSPOSE }
  | "det" {  DET }
  | "dim" {  DIMENSION }
  | "mag" {  MAGNITUDE }
  | "angle" {  ANGLE }
  | "(" {  LPAREN }
  | ")" {  file_context := false; RPAREN }
  | "[" {  LBRACKET }
  | "]" {  RBRACKET }
  | "{" {  LBRACE }
  | "}" {  RBRACE }
  | ";" {  SEMICOLON }
  | "," {COMMA }
  | "INT_MAX" {  INT_MAX }
  | "INT_MIN" {  INT_MIN }
  | "epsilon" {  EPSILON }
  | "true" {  BOOL_LITERAL true }
  | "false" {  BOOL_LITERAL false }
  | id as name {
    if !file_context then (
      let result = File name in
      file_context := false;
      result
    ) else(
      ID name)
  }
  | float_literal as f {  FLOAT_LITERAL (float_of_string f) }
  | int_literal as i {
    let value = int_of_string i in
    INT_LITERAL value
   }
  | file as name { File name }
  | '\"' [^'\"']* '\"' as s {
    STRING_LITERAL (String.sub s 1 (String.length s - 2))
  }
  | '\"' {  STRING_LITERAL "\"" }
  | eof {  EOF }

and comment level = parse
  | eof {  EOF }
  | "/*" { comment (level + 1) lexbuf }
  | "*/" { if level = 0 then token lexbuf else comment (level - 1) lexbuf }
  | newline {  file_context := false ;incr line_num; comment level lexbuf }
  | _ { comment level lexbuf }

{ 
  let tokenize_string str =
    let lexbuf = Lexing.from_string str in
    line_num := 1;
    let rec get_all_tokens tokens =
        match token lexbuf with
        | EOF -> List.rev (EOF :: tokens)
        | t -> get_all_tokens (t :: tokens)
    in
    get_all_tokens []
  
  let tokenize_file filename =
    let in_channel = open_in filename in
    let lexbuf = Lexing.from_channel in_channel in
    line_num := 1;
    let rec get_all_tokens tokens =
        match token lexbuf with
        | EOF ->
            close_in in_channel;
            List.rev (EOF :: tokens)
        | t -> get_all_tokens (t :: tokens)
    in
    try
      let result = get_all_tokens [] in
      close_in in_channel;
      result
    with e ->
      close_in in_channel;
      raise e
  
  let print_tokens_from_string str =
    let tokens = tokenize_string str in
    List.iter (fun t -> print_endline (token_to_string t)) tokens
  
  let print_tokens_from_file filename =
    let tokens = tokenize_file filename in
    List.iter (fun t -> print_endline (token_to_string t)) tokens
}