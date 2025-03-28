(* Function to convert a list of integers to a string representation for debugging *)
let ilist_to_string size values =
  "Ivector(" ^ string_of_int size ^ ", [" ^ String.concat "; " (List.map string_of_int values) ^ "])"

(* Function to convert a list of floats to a string representation for debugging *)
let flist_to_string size values =
  "Fvector(" ^ string_of_int size ^ ", [" ^ String.concat "; " (List.map string_of_float values) ^ "])"

(* Function to convert a list of lists of integers to a string representation for debugging *)
let ilistlist_to_string rows cols values =
  "Imatrix(" ^ string_of_int rows ^ ", " ^ string_of_int cols ^ ", [" ^
  String.concat "; " (List.map (fun row -> "[" ^ String.concat "; " (List.map string_of_int row) ^ "]") values)
  ^ "])"

(* Function to convert a list of lists of floats to a string representation for debugging *)
let flistlist_to_string rows cols values =
  "Fmatrix(" ^ string_of_int rows ^ ", " ^ string_of_int cols ^ ", [" ^
  String.concat "; " (List.map (fun row -> "[" ^ String.concat "; " (List.map string_of_float row) ^ "]") values)
  ^ "])"
;;

type token =
  (* Keywords for input/output *)
  | INPUT    (* Input command *)
  | PRINT    (* Print command *)
  | File of string (* Filename for Input/Print *)

  (* Control flow keywords *)
  | IF       (* if condition *)
  | THEN     (* then branch *)
  | ELSE     (* else branch *)
  | FOR      (* for loop *)
  | TO       (* range indicator in for loops *)
  | WHILE    (* while loop *)
  | DO       (* loop body start *)
  | END      (* block end marker *)

  (* Basic operators *)
  | IPLUS     (* + addition for Integer and IMatrix and IVector *)
  | FPLUS     (* +. addition for float and FMatrix and FVector *) 
  | IMINUS    (* - subtraction for Integer and IMatrix and IVector *)
  | FMINUS    (* -. subtraction for float and FMatrix and FVector *)
  | ITIMES    (* * multiplication for Integer and IMatrix and IVector *)
  | FTIMES    (* *. multiplication for float and FMatrix and FVector *)
  | IDIVIDE   (* / division for Integer and IMatrix and IVector *)
  | FDIVIDE   (* /. division for float and FMatrix and FVector *)
  | IMODULO   (* mod remainder for Integer *)
  | FMODULO   (* mod_float remainder for float *)
  | ABS       (* abs absolute value for Integer, float, and I||FVector and I||FMatrix *)
  | ASSIGN    (* := assignment *)

  (* Comparison operators *)
  | EQ       (* == equality *)
  | NEQ      (* ~= inequality *)
  | LT       (* < less than *)
  | GT       (* > greater than *)
  | LEQ      (* <= less than or equal *)
  | GEQ      (* >= greater than or equal *)

  (* Logical operators *)
  | AND      (* && logical and *)
  | OR       (* || logical or *)
  | NOT      (* ~ logical not *)

  (* Matrix/Vector specific operations *)
  | TRANSPOSE (* ' matrix transpose *)
  | DET       (* determinant function *)
  | DIMENSION (* dimension function *)
  | MAGNITUDE (* magnitude function *)
  | ANGLE     (* angle function *)

  (* Delimiters *)
  | LPAREN    (* ( left parenthesis *)
  | RPAREN    (* ) right parenthesis *)
  | LBRACKET  (* [ left bracket for vectors/matrices *)
  | RBRACKET  (* ] right bracket for vectors/matrices *)
  | LBRACE    (* { left brace for code blocks *)
  | RBRACE    (* } right brace for code blocks *)
  | SEMICOLON (* ; statement separator *)
  | COMMA     (* , list separator *)

  (* Special constants *)
  | INT_MAX   (* Maximum integer value *)
  | INT_MIN   (* Minimum integer value *)
  | EPSILON   (* Small floating-point value for comparisons *)

  (* Literals and identifiers *)
  | BOOL_LITERAL of bool     (* Boolean constants *)
  | ID of string             (* Variable identifiers *)
  | INT_LITERAL of int       (* Integer constants *)
  | FLOAT_LITERAL of float   (* Float constants *)
  | STRING_LITERAL of string (* String literals for filenames *)

  (* Vector tokens with size information *)
  | Ivector of int * int list     (* Size and Integer vector *)
  | Fvector of int * float list   (* Size and Float vector *)

  (* Matrix tokens with size information *)
  | Imatrix of int * int * int list list      (* Rows, Columns, and Integer matrix in row major format *)
  | Fmatrix of int * int * float list list   (* Rows, Columns, and Float matrix in row major format *)

  (* End of file marker *)
  | EOF
;;


(* Function to convert a token to a string representation for debugging *)
let token_to_string = function
  | INPUT -> "INPUT"
  | PRINT -> "PRINT"
  | File s -> "File("^s^")"
  | IF -> "IF"
  | THEN -> "THEN"
  | ELSE -> "ELSE"
  | FOR -> "FOR"
  | TO -> "TO"
  | WHILE -> "WHILE"
  | DO -> "DO"
  | END -> "END"
  | IPLUS -> "IPLUS"
  | FPLUS -> "FPLUS"
  | IMINUS -> "IMINUS"
  | FMINUS -> "FMINUS"
  | ITIMES -> "ITIMES"
  | FTIMES -> "FTIMES"
  | IDIVIDE -> "IDIVIDE"
  | FDIVIDE -> "FDIVIDE"
  | IMODULO -> "IMODULO"
  | FMODULO -> "FMODULO"
  | ABS -> "ABS"
  | ASSIGN -> "ASSIGN"
  | EQ -> "EQ"
  | NEQ -> "NEQ"
  | LT -> "LT"
  | GT -> "GT"
  | LEQ -> "LEQ"
  | GEQ -> "GEQ"
  | AND -> "AND"
  | OR -> "OR"
  | NOT -> "NOT"
  | TRANSPOSE -> "TRANSPOSE"
  | DET -> "DET"
  | DIMENSION -> "DIMENSION"
  | MAGNITUDE -> "MAGNITUDE"
  | ANGLE -> "ANGLE"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LBRACKET -> "LBRACKET"
  | RBRACKET -> "RBRACKET"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | SEMICOLON -> "SEMICOLON"
  | COMMA -> "COMMA"
  | INT_MAX -> "INT_MAX"
  | INT_MIN -> "INT_MIN"
  | EPSILON -> "EPSILON"
  | BOOL_LITERAL b -> "BOOL_LITERAL(" ^ (if b then "true" else "false") ^ ")"
  | ID s -> "ID(" ^ s ^ ")"
  | INT_LITERAL i -> "INT_LITERAL(" ^ string_of_int i ^ ")"
  | FLOAT_LITERAL f -> "FLOAT_LITERAL(" ^ string_of_float f ^ ")"
  | STRING_LITERAL s -> "STRING_LITERAL(\"" ^ s ^ "\")"
  | Ivector (size, values) -> ilist_to_string size values
  | Fvector (size, values) -> flist_to_string size values
  | Imatrix (rows, cols, values) -> ilistlist_to_string rows cols values
  | Fmatrix (rows, cols, values) -> flistlist_to_string rows cols values
  | EOF -> "EOF"

(* Function to print a list of tokens for debugging *)
let print_token_list tokens =
  List.iter (fun t -> print_endline (token_to_string t)) tokens