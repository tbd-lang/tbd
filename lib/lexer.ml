type t =
  | Integer of int
  | String of string
  | Identifier of string
  | Let
  | Equal
  | EOF

let token_to_string token =
  match token with
  | Integer n -> Printf.sprintf "Integer(%d)" n
  | String s -> Printf.sprintf "String(%s)" s
  | Identifier s -> Printf.sprintf "Identifier(%s)" s
  | Let -> "Let"
  | Equal -> "Equal"
  | EOF -> "EOF"
;;

let rec print_tokens tokens =
  match tokens with
  | [] -> ()
  | hd :: tl ->
    print_endline (token_to_string hd);
    print_tokens tl
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
    | "let" -> Let
    | _ -> Identifier s
  in
  match chars with
  | [] -> [], string_to_token (Utils.chars_to_string acc)
  | hd :: tl ->
    (match hd with
     | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' -> collect_identifier tl (hd :: acc)
     | _ -> chars, string_to_token (Utils.chars_to_string acc))
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
       | '=' -> aux tl (Equal :: tokens)
       | c -> failwith (Printf.sprintf "Unknown character: %c" c))
  in
  List.rev (aux (Utils.string_to_chars text) [])
;;
