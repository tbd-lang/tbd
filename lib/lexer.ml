type t =
  | Unit
  | Boolean of bool
  | Integer of int
  | String of string
  | Identifier of string
  | Let
  | Rec
  | If
  | Then
  | Else
  | Equal
  | Minus
  | Semicolon
  | LeftParenthesis
  | RightParenthesis
  | LeftBrace
  | RightBrace
  | EOF

let token_to_string token =
  match token with
  | Unit -> "Unit"
  | Boolean b -> Printf.sprintf "Boolean(%b)" b
  | Integer n -> Printf.sprintf "Integer(%d)" n
  | String s -> Printf.sprintf "String(%s)" s
  | Identifier s -> Printf.sprintf "Identifier(%s)" s
  | Let -> "Let"
  | Rec -> "Rec"
  | If -> "If"
  | Then -> "Then"
  | Else -> "Else"
  | Equal -> "Equal"
  | Minus -> "Minus"
  | Semicolon -> "Semicolon"
  | LeftParenthesis -> "LeftParenthesis"
  | RightParenthesis -> "RightParenthesis"
  | LeftBrace -> "LeftBrace"
  | RightBrace -> "RightBrace"
  | EOF -> "EOF"
;;

let rec print_tokens tokens =
  match tokens with
  | [] -> ()
  | hd :: tl ->
    print_endline (token_to_string hd);
    print_tokens tl
;;

let collect_unit chars =
  match chars with
  | ')' :: tl -> tl, Unit
  | _ -> chars, LeftParenthesis
;;

let rec collect_integer chars acc =
  match chars with
  | ('0' .. '9' as hd) :: tl -> collect_integer tl (hd :: acc)
  | _ -> chars, Integer (Utils.chars_to_int acc)
;;

let rec collect_string chars acc =
  match chars with
  | [] -> failwith "Unclosed string"
  | '"' :: tl -> tl, String (Utils.chars_to_string acc)
  | hd :: tl -> collect_string tl (hd :: acc)
;;

let rec collect_identifier chars acc =
  let string_to_token s =
    match s with
    | "true" -> Boolean true
    | "false" -> Boolean false
    | "let" -> Let
    | "rec" -> Rec
    | "if" -> If
    | "then" -> Then
    | "else" -> Else
    | _ -> Identifier s
  in
  match chars with
  | [] -> [], string_to_token (Utils.chars_to_string acc)
  | hd :: tl ->
    (match hd with
     | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' -> collect_identifier tl (hd :: acc)
     | _ -> chars, string_to_token (Utils.chars_to_string acc))
;;

let rec collect_comment chars =
  match chars with
  | ([] as tl) | '\n' :: tl -> tl
  | _ :: tl -> collect_comment tl
;;

let tokenize text =
  let rec aux chars tokens =
    match chars with
    | [] -> EOF :: tokens
    | hd :: tl ->
      (match hd with
       | ' ' | '\n' | '\t' | '\r' -> aux tl tokens
       | '0' .. '9' ->
         let chars, token = collect_integer tl [ hd ] in
         aux chars (token :: tokens)
       | '"' ->
         let chars, token = collect_string tl [] in
         aux chars (token :: tokens)
       | 'a' .. 'z' | 'A' .. 'Z' | '_' | '\'' | '@' ->
         let chars, token = collect_identifier tl [ hd ] in
         aux chars (token :: tokens)
       | '#' ->
         let chars = collect_comment tl in
         aux chars tokens
       | '=' -> aux tl (Equal :: tokens)
       | '-' -> aux tl (Minus :: tokens)
       | ';' -> aux tl (Semicolon :: tokens)
       | '(' ->
         let chars, token = collect_unit tl in
         aux chars (token :: tokens)
       | ')' -> aux tl (RightParenthesis :: tokens)
       | '{' -> aux tl (LeftBrace :: tokens)
       | '}' -> aux tl (RightBrace :: tokens)
       | c -> failwith (Printf.sprintf "Unknown character: %c" c))
  in
  List.rev (aux (Utils.string_to_chars text) [])
;;
