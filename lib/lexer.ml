type token =
  | Unit
  | Integer of int
  | Float of float
  | Boolean of bool
  | Char of char
  | String of string
  | Identifier of string
  | Comment of string
  | Assignment
  | AddInteger
  | SubInteger
  | MulInteger
  | DivInteger
  | AddFloat
  | SubFloat
  | MulFloat
  | DivFloat
  | Concat
  | Let
  | Const
  | Function
  | Main
  | Recursive
  | Enum
  | Struct
  | Import
  | Module
  | Comma
  | Semicolon
  | Colon
  | Cons
  | Range
  | LeftParenthesis
  | RightParenthesis
  | LeftBracket
  | RightBracket
  | LeftBrace
  | RightBrace
  | LeftChevron
  | RightChevron
  | Match
  | Arrow
  | Wildcard

let token_to_string token =
  match token with
  | Unit -> "Unit"
  | Integer n -> Printf.sprintf "Integer(%d)" n
  | Float f -> Printf.sprintf "Float(%f)" f
  | Boolean b -> Printf.sprintf "Boolean(%b)" b
  | Char c -> Printf.sprintf "Char(%c)" c
  | String s -> Printf.sprintf "String(%s)" s
  | Identifier s -> Printf.sprintf "Identifier(%s)" s
  | Comment s -> Printf.sprintf "Comment(%s)" s
  | Assignment -> "Assignment"
  | AddInteger -> "AddInt"
  | SubInteger -> "SubInt"
  | MulInteger -> "MulInt"
  | DivInteger -> "DivInt"
  | AddFloat -> "AddFloat"
  | SubFloat -> "SubFloat"
  | MulFloat -> "MulFloat"
  | DivFloat -> "DivFloat"
  | Concat -> "Concat"
  | Let -> "Let"
  | Const -> "Const"
  | Function -> "Fun"
  | Main -> "Main"
  | Recursive -> "Recursive"
  | Enum -> "Enum"
  | Struct -> "Struct"
  | Import -> "Import"
  | Module -> "Module"
  | Comma -> "Comma"
  | Cons -> "Cons"
  | Range -> "Range"
  | Semicolon -> "Semicolon"
  | Colon -> "Colon"
  | LeftParenthesis -> "LeftParenthesis"
  | RightParenthesis -> "RightParenthesis"
  | LeftBracket -> "LeftBracket"
  | RightBracket -> "RightBracket"
  | LeftBrace -> "LeftBrace"
  | RightBrace -> "RightBrace"
  | LeftChevron -> "LeftChevron"
  | RightChevron -> "RightChevron"
  | Match -> "Match"
  | Arrow -> "Arrow"
  | Wildcard -> "Wildcard"
;;

let rec print_tokens tokens =
  match tokens with
  | [] -> ()
  | hd :: tl ->
    print_endline (token_to_string hd);
    print_tokens tl
;;

let chars_to_string chars = List.rev chars |> List.to_seq |> String.of_seq

let rec collect_identifier chars acc =
  let string_to_token s =
    match s with
    | "_" -> Wildcard
    | "true" -> Boolean true
    | "false" -> Boolean false
    | "let" -> Let
    | "const" -> Const
    | "fun" -> Function
    | "main" -> Main
    | "rec" -> Recursive
    | "enum" -> Enum
    | "struct" -> Struct
    | "import" -> Import
    | "module" -> Module
    | "match" -> Match
    | _ -> Identifier s
  in
  match chars with
  | [] -> [], string_to_token (chars_to_string acc)
  | hd :: tl ->
    (match hd with
     | 'a' .. 'z' | 'A' .. 'Z' | '_' | '.' | '0' .. '9' ->
       collect_identifier tl (hd :: acc)
     | _ -> chars, string_to_token (chars_to_string acc))
;;

let rec collect_number chars acc =
  let chars_to_float chars = Float.of_string (chars_to_string chars) in
  let chars_to_integer chars = Int.of_float (chars_to_float chars) in
  let rec collect_float chars acc =
    match chars with
    | [] -> [], Float (chars_to_float acc)
    | hd :: tl ->
      (match hd with
       | '0' .. '9' -> collect_float tl (hd :: acc)
       | _ -> chars, Float (chars_to_float acc))
  in
  match chars with
  | [] -> [], Integer (chars_to_integer acc)
  | hd :: tl ->
    (match hd with
     | '0' .. '9' -> collect_number tl (hd :: acc)
     | '.' -> collect_float tl (hd :: acc)
     | _ -> chars, Integer (chars_to_integer acc))
;;

let collect_unit chars =
  match chars with
  | [] -> failwith "Parenthesis never closed"
  | ')' :: tl -> tl, Unit
  | _ -> chars, LeftParenthesis
;;

let rec collect_string chars acc =
  match chars with
  | [] -> failwith "String never closed"
  | hd :: tl ->
    (match hd with
     | '"' -> tl, String (chars_to_string (hd :: acc))
     | _ -> collect_string tl (hd :: acc))
;;

let collect_char chars =
  match chars with
  | [] -> failwith "Char never closed"
  | c :: '\'' :: tl -> tl, Char c
  | '\\' :: c :: '\'' :: tl ->
    (match c with
     | 'n' -> tl, Char '\n'
     | 't' -> tl, Char '\t'
     | 'r' -> tl, Char '\r'
     | '\\' -> tl, Char '\\'
     | '\'' -> tl, Char '\''
     | _ -> failwith "Unknown escape character")
  | _ -> failwith "Char too long"
;;

let collect_cons chars =
  match chars with
  | [] -> [], Colon
  | ':' :: tl -> tl, Cons
  | _ -> chars, Colon
;;

let collect_add chars =
  match chars with
  | [] -> failwith "Add never used"
  | '.' :: tl -> tl, AddFloat
  | _ -> chars, AddInteger
;;

let collect_sub chars =
  match chars with
  | [] -> failwith "Sub never used"
  | '.' :: tl -> tl, SubFloat
  | '>' :: tl -> tl, Arrow
  | _ -> chars, SubInteger
;;

let collect_mul chars =
  match chars with
  | [] -> failwith "Mul never used"
  | '.' :: tl -> tl, MulFloat
  | _ -> chars, MulInteger
;;

let collect_div chars =
  match chars with
  | [] -> failwith "Div never used"
  | '.' :: tl -> tl, DivFloat
  | _ -> chars, DivInteger
;;

let collect_range chars =
  match chars with
  | [] -> failwith "Missing character after ."
  | '.' :: tl -> tl, Range
  | _ -> failwith "Unsupported character after ."
;;

let rec collect_comment chars acc =
  match chars with
  | [] -> [], Comment (chars_to_string acc)
  | hd :: tl ->
    (match hd with
     | '\n' -> tl, Comment (chars_to_string acc)
     | _ -> collect_comment tl (hd :: acc))
;;

let tokenize text =
  let rec aux chars tokens =
    match chars with
    | [] -> tokens
    | hd :: tl ->
      (match hd with
       | '#' ->
         let chars, token = collect_comment tl [ hd ] in
         aux chars (token :: tokens)
       | ' ' | '\n' | '\t' | '\r' -> aux tl tokens
       | '0' .. '9' ->
         let chars, token = collect_number tl [ hd ] in
         aux chars (token :: tokens)
       | '+' ->
         let chars, token = collect_add tl in
         aux chars (token :: tokens)
       | '-' ->
         let chars, token = collect_sub tl in
         aux chars (token :: tokens)
       | '*' ->
         let chars, token = collect_mul tl in
         aux chars (token :: tokens)
       | '/' ->
         let chars, token = collect_div tl in
         aux chars (token :: tokens)
       | '^' -> aux tl (Concat :: tokens)
       | '"' ->
         let chars, token = collect_string tl [ hd ] in
         aux chars (token :: tokens)
       | '\'' ->
         let chars, token = collect_char tl in
         aux chars (token :: tokens)
       | 'a' .. 'z' | 'A' .. 'Z' | '_' ->
         let chars, token = collect_identifier tl [ hd ] in
         aux chars (token :: tokens)
       | '=' -> aux tl (Assignment :: tokens)
       | ',' -> aux tl (Comma :: tokens)
       | ';' -> aux tl (Semicolon :: tokens)
       | ':' ->
         let chars, token = collect_cons tl in
         aux chars (token :: tokens)
       | '.' ->
         let chars, token = collect_range tl in
         aux chars (token :: tokens)
       | '(' ->
         let chars, token = collect_unit tl in
         aux chars (token :: tokens)
       | ')' -> aux tl (RightParenthesis :: tokens)
       | '[' -> aux tl (LeftBracket :: tokens)
       | ']' -> aux tl (RightBracket :: tokens)
       | '{' -> aux tl (LeftBrace :: tokens)
       | '}' -> aux tl (RightBrace :: tokens)
       | '<' -> aux tl (LeftChevron :: tokens)
       | '>' -> aux tl (RightChevron :: tokens)
       | c -> failwith (Printf.sprintf "Unknown character: %c" c))
  in
  List.rev (aux (String.to_seq text |> List.of_seq) [])
;;
