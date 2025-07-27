type token =
  | Unit
  | Integer of int
  | Float of float
  | Boolean of bool
  | Char of char
  | String of string
  | Identifier of string
  | Assignment
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


let rec print_tokens tokens =
  match tokens with
  | [] -> ()
  | hd :: tl ->
    print_endline (token_to_string hd);
    print_tokens tl

  
let chars_to_string chars =
  List.rev chars |> List.to_seq |> String.of_seq

let collect_identifier chars tokens =
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

  let rec aux chars acc =
    match chars with
    | [] -> [], string_to_token (chars_to_string acc)
    | hd :: tl -> match hd with
      | 'a'..'z' | 'A'..'Z' | '_' | '.' | '0'..'9' -> aux tl (hd :: acc)
      | _ -> chars, string_to_token (chars_to_string acc)
  in

  let chars, token = aux chars [] in
  chars, token :: tokens


let rec collect_number chars tokens =
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

  let rec collect_integer chars acc =
    match chars with
    | [] -> [], Integer(chars_to_integer acc)
    | hd :: tl -> match hd with
      | '0'..'9' | '-' -> collect_integer tl (hd :: acc)
      | '.' -> collect_float tl (hd :: acc)
      | _ -> chars, Integer(chars_to_integer acc)
  in

  let chars, token = collect_integer chars [] in
  chars, token :: tokens
  

let collect_unit chars tokens =
  match chars with
  | [] -> [], tokens
  | '(' :: ')' :: tl -> tl, Unit :: tokens
  | _ :: tl -> tl, LeftParenthesis :: tokens


let collect_string chars tokens =
  let rec aux chars close acc =
    match chars with
    | [] -> failwith "String never closed"
    | hd :: tl ->
      match hd, close with
      | '"', false -> aux tl true (hd :: acc)
      | '"', true -> tl, String(chars_to_string (hd :: acc))
      | _ -> aux tl close (hd :: acc)
  in

  let chars, token = aux chars false [] in
  chars, token :: tokens


let peek chars =
  match chars with
  | [] ->  None
  | hd :: tl -> Some(hd, tl)


let peek2 chars =
  match chars with
  | [] | [_] -> None
  | e1 :: e2 :: tl -> Some(e1, e2, tl)


let tokenize chars =
  let rec aux chars tokens =
    match chars with
    | [] -> tokens
    | hd :: tl ->
      (match hd with
      | ' ' | '\n' | '\t' | '\r' -> aux tl tokens
      | '-' ->
        (match peek tl with
        | None -> failwith "Missing character after '-'"
        | Some ('>', tl) -> aux tl (Arrow :: tokens)
        | Some('0'..'9', _) ->
          (let chars, tokens = collect_number chars tokens in
          aux chars tokens)
        | _ -> failwith "Unsupported character found after '-'"
        )
      | '0'..'9' ->
        (let chars, tokens = collect_number chars tokens in
        aux chars tokens)
      | '"' ->
        (let chars, tokens = collect_string chars tokens in
        aux chars tokens)
      | '\'' -> 
        (match peek2 tl with
        | None -> failwith "Missing chars after '"
        | Some(c, '\'', tl) -> aux tl (Char(c) :: tokens)
        | _ -> failwith "' never closed")
      | 'a'..'z' | 'A'..'Z' | '_' ->
        (let chars, tokens = collect_identifier chars tokens in
        aux chars tokens)
      | '=' -> aux tl (Assignment :: tokens)
      | ',' -> aux tl (Comma :: tokens)
      | ';' -> aux tl (Semicolon :: tokens)
      | ':' -> 
        (match peek tl with
        | None -> failwith "Missing character after ':'"
        | Some(':', tl) -> aux tl (Cons :: tokens)
        | Some(_, _) -> aux tl (Colon :: tokens))
      | '.' -> 
        (match peek tl with
        | None -> failwith "Missing character after '.'"
        | Some('.', tl) -> aux tl (Range :: tokens)
        | _ -> failwith "Missing character after '.'")
      | '(' ->
        (let chars, tokens = collect_unit chars tokens in
        aux chars tokens)
      | ')' -> aux tl (RightParenthesis :: tokens)
      | '[' -> aux tl (LeftBracket :: tokens)
      | ']' -> aux tl (RightBracket :: tokens)
      | '{' -> aux tl (LeftBrace :: tokens)
      | '}' -> aux tl (RightBrace :: tokens)
      | c -> failwith (Printf.sprintf "Unknown character: %c" c))
    in

    List.rev (aux chars [])

  
let test_cases =
  [
    "let int = { 1 };";
    "let int = { 12 };";
    "let int = { -12 };";

    "let float = { 1.0 };";
    "let float = { 12.34 };";
    "let float = { -12.34 };";

    "let string = { \"This is a string\" };";

    "let char = { 'c' };";

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


let () =
  let run_test text =
    let chars = String.to_seq text |> List.of_seq in
    let tokens = tokenize chars in
    print_endline ("TEST: " ^ text);
    print_tokens tokens;
    print_newline ()
  in

  List.iter (fun t -> run_test t) test_cases