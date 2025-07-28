type token =
  | Unit
  | Integer of int
  | Float of float
  | Boolean of bool
  | Char of char
  | String of string
  | Identifier of string
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
  | Fun
  | Recursive
  | Enum
  | Of
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
  | Match
  | Arrow
  | Wildcard
;;


let token_to_string token =
  match token with
  | Unit -> "Unit"
  | Integer x -> Printf.sprintf "Integer(%d)" x
  | Float x -> Printf.sprintf "Float(%f)" x
  | Boolean b -> Printf.sprintf "Boolean(%b)" b
  | Char c -> Printf.sprintf "Char(%c)" c
  | String s -> Printf.sprintf "String(%s)" s
  | Identifier s -> Printf.sprintf "Identifier(%s)" s
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
  | Fun -> "Fun"
  | Recursive -> "Recursive"
  | Enum -> "Enum"
  | Of -> "Of"
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
  

let chars_to_string chars =
  List.rev chars |> List.to_seq |> String.of_seq
;;


let rec collect_identifier chars acc =
  let string_to_token s =
    match s with
    | s when String.ends_with ~suffix:"." s -> failwith "Incomplete module access"
    | s when String.equal "_" s -> Wildcard
    | "true" -> Boolean(true)
    | "false" -> Boolean(false)
    | "let" -> Let
    | "fun" -> Fun
    | "rec" -> Recursive
    | "enum" -> Enum
    | "of" -> Of
    | "struct" -> Struct
    | "import" -> Import
    | "module" -> Module
    | "match" -> Match
    | _ -> Identifier(s)
  in

  match chars with
    | [] -> [], string_to_token (chars_to_string acc)
    | hd :: tl -> match hd with
      | 'a'..'z' | 'A'..'Z' | '_' | '.' | '0'..'9' -> collect_identifier tl (hd :: acc)
      | _ -> chars, string_to_token (chars_to_string acc)
;;


let rec collect_number chars acc =
  let chars_to_float chars =
    Float.of_string (chars_to_string chars)
  in

  let chars_to_integer chars =
    Int.of_float (chars_to_float chars)
  in

  let rec collect_float chars acc =
    match chars with
    | [] -> [], Float(chars_to_float acc)
    | hd :: tl -> match hd with
      | '0'..'9' -> collect_float tl (hd :: acc)
      | _ -> chars, Float(chars_to_float acc)
  in

  match chars with
    | [] -> [], Integer(chars_to_integer acc)
    | hd :: tl -> match hd with
      | '0'..'9' -> collect_number tl (hd :: acc)
      | '.' -> collect_float tl (hd :: acc)
      | _ -> chars, Integer(chars_to_integer acc)
;;


let collect_unit chars =
  match chars with
  | [] -> failwith "Parenthesis never closed"
  | ')' :: tl -> tl, Unit
  | _ :: tl -> tl, LeftParenthesis
;;


let rec collect_string chars acc =
  match chars with
  | [] -> failwith "String never closed"
  | hd :: tl ->
    match hd with
    | '"' -> tl, String(chars_to_string (hd :: acc))
    | _ -> collect_string tl (hd :: acc)
;;


let collect_char chars = 
  match chars with
  | [] -> failwith "Char never closed"
  | c :: '\'' :: tl -> tl, Char(c)
  | '\\' :: c :: '\'' :: tl -> 
    (match c with
    | 'n' -> tl, Char('\n')
    | 't' -> tl, Char('\t')
    | 'r' -> tl, Char('\r')
    | '\\' -> tl, Char('\\')
    | '\'' -> tl, Char('\'')
    | _ -> failwith "Unknown escape character")
  | _ -> failwith "Invalid char"
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


let tokenize chars =
  let rec aux chars tokens =
    match chars with
    | [] -> tokens
    | hd :: tl ->
      (match hd with
      | ' ' | '\n' | '\t' | '\r' -> aux tl tokens
      | '0'..'9' ->
        (let chars, token = collect_number tl [hd] in
        aux chars (token :: tokens))
      | '+' ->
        (let chars, token = collect_add tl in
        aux chars (token :: tokens))
      | '-' ->
        (let chars, token = collect_sub tl in
        aux chars (token :: tokens))
      | '*' ->
        (let chars, token = collect_mul tl in
        aux chars (token :: tokens))
      | '/' ->
        (let chars, token = collect_div tl in
        aux chars (token :: tokens))
      | '^' -> aux tl (Concat :: tokens)
      | '"' ->
        (let chars, token = collect_string tl [hd] in
        aux chars (token :: tokens))
      | '\'' -> 
        (let chars, token = collect_char tl in
        aux chars (token :: tokens))
      | 'a'..'z' | 'A'..'Z' | '_' ->
        (let chars, token = collect_identifier tl [hd] in
        aux chars (token::tokens))
      | '=' -> aux tl (Assignment :: tokens)
      | ',' -> aux tl (Comma :: tokens)
      | ';' -> aux tl (Semicolon :: tokens)
      | ':' -> 
        (let chars, token = collect_cons tl in
        aux chars (token :: tokens))
      | '.' -> 
        (let chars, token = collect_range tl in
        aux chars (token :: tokens))
      | '(' ->
        (let chars, token = collect_unit tl in
        aux chars (token :: tokens))
      | ')' -> aux tl (RightParenthesis :: tokens)
      | '[' -> aux tl (LeftBracket :: tokens)
      | ']' -> aux tl (RightBracket :: tokens)
      | '{' -> aux tl (LeftBrace :: tokens)
      | '}' -> aux tl (RightBrace :: tokens)
      | c -> failwith (Printf.sprintf "Unknown character: %c" c))
    in

    List.rev (aux chars [])
;;

  
let test_cases =
  [
    "let int = { 1 };";
    "let int = { 12 };";
    "let int = { -12 };";

    "let float = { 1.0 };";
    "let float = { 12.34 };";
    "let float = { -12.34 };";

    "let add a b = { a + b };";
    "let sub a b = { a - b };";
    "let mul a b = { a * b };";
    "let div a b = { a / b };";

    "let add a b = { a +. b };";
    "let sub a b = { a -. b };";
    "let mul a b = { a *. b };";
    "let div a b = { a /. b };";

    "let concat s1 s2 = { \"Hello \" ^ \"world!\" };";

    "let string = { \"This is a string\" };";

    "let char = { 'c' };";
    "let char = { '\\n' };";

    "let bool = { true };";
    "let bool = { false };";

    "fun norm x = { Int.div x (Int.abs x) };";

    "enum Status = { success, failure of int };";

    "struct Person = { name : string, age : int };";

    "fun norm x = { match x { 0 -> 0, _ -> 1 }};";

    "let _not_wildcard = { 0 };";
    "let _ = { \"Wildcard\" };";

    "let () = { \"Unit\" }";

    "fun rec factorial n = { match n { 0 -> 1, _ -> Int.mul n (factorial (Int.sub n 1)) }};";

    "fun pop list = { match list { hd::_ -> Option.some hd, _ -> Option.none }};";

    "fun char_to_int c = { match c { '0'..'9' -> Option.some (Int.sub (Char.to_ascii c) (Char.to_ascii '0')), _ -> Option.none }}"
  ]
;;


let () =
  let run_test text =
    let chars = String.to_seq text |> List.of_seq in
    let tokens = tokenize chars in
    print_endline ("TEST: " ^ text);
    print_tokens tokens;
    print_newline ()
  in

  List.iter (fun t -> run_test t) test_cases
;;