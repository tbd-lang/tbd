type expr =
  | Unit
  | Integer of int
  | Float of float
  | Boolean of bool
  | Char of char
  | String of string
  | Identifier of string
  | Call of string * expr list
  | Block of stmt list * expr

and stmt =
  | Import of string
  | Const of string * expr
  | Function of string * string list * expr
  | Let of string * expr

let rec string_of_expr expr =
  match expr with
  | Unit -> "Unit"
  | Integer n -> Printf.sprintf "Integer(%d)" n
  | Float f -> Printf.sprintf "Float(%f)" f
  | Boolean b -> Printf.sprintf "Boolean(%b)" b
  | Char c -> Printf.sprintf "Char(%c)" c
  | String s -> Printf.sprintf "String(%s)" s
  | Identifier name -> Printf.sprintf "Identifier(%s)" name
  | Call (name, args) ->
    Printf.sprintf
      "Call(%s, %s)"
      name
      (string_of_string_list (List.map (fun a -> string_of_expr a) args))
  | Block (stmts, expr) ->
    Printf.sprintf
      "Block(%s, %s)"
      (string_of_string_list (List.map (fun s -> string_of_stmt s) stmts))
      (string_of_expr expr)

and string_of_stmt stmt =
  match stmt with
  | Import name -> Printf.sprintf "Import(%s)" name
  | Const (name, expr) -> Printf.sprintf "Const(%s, %s)" name (string_of_expr expr)
  | Function (name, args, expr) ->
    Printf.sprintf
      "Function(%s, %s, %s)"
      name
      (string_of_string_list args)
      (string_of_expr expr)
  | Let (name, expr) -> Printf.sprintf "Let(%s, %s)" name (string_of_expr expr)

and string_of_string_list l =
  let rec aux l acc =
    match l with
    | [] -> "[" ^ acc
    | [ hd ] -> aux [] (hd ^ acc)
    | hd :: tl -> aux tl (", " ^ hd ^ acc)
  in
  aux l "]"
;;

let rec collect_call tokens =
  let rec collect_args tokens args =
    match tokens with
    | Lexer.RightBracket :: tl -> tl, args
    | Lexer.Comma :: tl -> collect_args tl args
    | _ ->
      let tl, expr = parse_expr tokens in
      collect_args tl (expr :: args)
  in
  match tokens with
  | Lexer.Identifier name :: Lexer.LeftParenthesis :: tl ->
    (match tl with
     | _ ->
       let tl, args = collect_args tl [] in
       tl, Call (name, args))
  | _ -> failwith "invalid Call syntax"

and collect_block _ = failwith "collect_block not implemented"

and parse_expr tokens =
  match tokens with
  | Lexer.Unit :: tl -> tl, Unit
  | Lexer.Integer n :: tl -> tl, Integer n
  | Lexer.Float f :: tl -> tl, Float f
  | Lexer.Boolean b :: tl -> tl, Boolean b
  | Lexer.Char c :: tl -> tl, Char c
  | Lexer.String s :: tl -> tl, String s
  | Lexer.Identifier _ :: Lexer.LeftParenthesis :: _ -> collect_call tokens
  | Lexer.Identifier name :: tl -> tl, Identifier name
  | Lexer.LeftBrace :: _ -> collect_block tokens
  | Lexer.Semicolon :: tl -> tl, Unit
  | _ -> failwith "invalid expression syntax"

and collect_const tokens =
  match tokens with
  | Lexer.Const :: Lexer.Identifier name :: tl ->
    let tl, expr = parse_expr tl in
    tl, Const (name, expr)
  | _ -> failwith "invalid Const syntax"

and collect_function _ = failwith "collect_function not implemented"
and collect_let _ = failwith "collect_let not implemented"

and parse_stmt tokens =
  match tokens with
  | Lexer.Import :: Lexer.Identifier name :: Lexer.Semicolon :: tl -> tl, Import name
  | Lexer.Const :: _ -> collect_const tokens
  | Lexer.Function :: _ -> collect_function tokens
  | Lexer.Let :: _ -> collect_let tokens
  | _ -> failwith "invalid statement syntax"
;;

(*
   let debug s =
  print_endline ("DEBUG: " ^ s);
  print_newline ()
;;

let rec collect_block tokens stmts =
  match tokens with
  | [] -> failwith "Unclosed block"
  | Lexer.LeftBrace :: tl -> collect_block tl stmts
  | Lexer.RightBrace :: tl -> tl, Block (stmts, Unit)
  | Lexer.Function :: tl ->
    let tokens, stmt = collect_function tl in
    collect_block tokens (stmt :: stmts)
  | Lexer.Let :: tl ->
    let tokens, stmt = collect_let tl in
    collect_block tokens (stmt :: stmts)
  | _ ->
    let tokens, expr = parse_expr tokens in
    (match tokens with
     | [] -> failwith "unlcosed block"
     | Lexer.RightBrace :: tl -> tl, Block (stmts, expr)
     | _ -> failwith "unsupported stuff after expression")

and parse_expr tokens =
  match tokens with
  | [] -> tokens, Unit
  | Lexer.Unit :: tl -> tl, Unit
  | Lexer.Integer n :: tl -> tl, Integer n
  | Lexer.Float f :: tl -> tl, Float f
  | Lexer.Boolean b :: tl -> tl, Boolean b
  | Lexer.Char c :: tl -> tl, Char c
  | Lexer.String s :: tl -> tl, String s
  | Lexer.Identifier name :: tl -> tl, Identifier name
  | _ -> failwith "Unsupported expression"

and string_list_to_string l =
  let rec aux l acc =
    match l with
    | [] -> "[" ^ acc
    | [ hd ] -> aux [] (hd ^ acc)
    | hd :: tl -> aux tl (", " ^ hd ^ acc)
  in
  aux l "]"



and collect_import tokens =
  match tokens with
  | [] -> failwith "Unclosed import"
  | Lexer.Identifier name :: tl -> tl, Import name
  | _ -> failwith "Unsupported import syntax"

and collect_const tokens =
  match tokens with
  | [] -> failwith "Unclosed const"
  | Lexer.Identifier name :: Lexer.Assignment :: tl ->
    (match tl with
     | [] -> failwith "Unclosed const"
     | Lexer.Integer n :: tl' -> tl', Const (name, Integer n)
     | Lexer.Float f :: tl' -> tl', Const (name, Float f)
     | Lexer.Boolean b :: tl' -> tl', Const (name, Boolean b)
     | Lexer.Char c :: tl' -> tl', Const (name, Char c)
     | Lexer.String s :: tl' -> tl', Const (name, String s)
     | _ -> failwith "Unsupported const type")
  | _ -> failwith "Unsupported const syntax"

and collect_function tokens =
  let rec collect_args tokens args =
    match tokens with
    | [] -> failwith "Unclosed function"
    | Lexer.Identifier name :: Lexer.Comma :: tl -> collect_args tl (name :: args)
    | Lexer.Identifier name :: tl -> collect_args tl (name :: args)
    | Lexer.RightParenthesis :: tl -> tl, args
    | _ -> failwith "Unsupported function argument type"
  in
  match tokens with
  | [] -> failwith "Unclosed function"
  | Lexer.Identifier name :: Lexer.Unit :: tl ->
    let tokens, expr = collect_block tl [] in
    List.iter (fun t -> debug (Lexer.token_to_string t)) tokens;
    tokens, Function (name, [], expr)
  | Lexer.Identifier name :: Lexer.LeftParenthesis :: tl ->
    let tokens, args = collect_args tl [] in
    let tokens, expr = collect_block tokens [] in
    List.iter (fun t -> debug (Lexer.token_to_string t)) tokens;
    tokens, Function (name, args, expr)
  | _ -> failwith "Unsupported function name type"

and collect_let tokens =
  match tokens with
  | [] -> failwith "Unclosed let"
  | Lexer.Identifier name :: Lexer.Assignment :: tl ->
    let tokens, expr = parse_expr tl in
    tokens, Let (name, expr)
  | _ -> failwith "Unsupported let statement"

and parse_stmt tokens stmts =
  match tokens with
  | [] -> List.rev stmts
  | Lexer.Import :: tl ->
    let tokens, expr = collect_import tl in
    parse_stmt tokens (expr :: stmts)
  | Lexer.Const :: tl ->
    let tokens, expr = collect_const tl in
    parse_stmt tokens (expr :: stmts)
  | Lexer.Function :: tl ->
    let tokens, expr = collect_function tl in
    parse_stmt tokens (expr :: stmts)
  | Lexer.Let :: tl ->
    let tokens, expr = collect_let tl in
    parse_stmt tokens (expr :: stmts)
  | Lexer.Semicolon :: tl -> parse_stmt tl []
  | Lexer.RightBrace :: tl -> parse_stmt tl []
  | _ -> failwith "Unsupported top-level expression"
;;

let rec print_ast stmts =
  match stmts with
  | [] -> ()
  | stmt :: tl ->
    print_endline (stmt_to_string stmt);
    print_ast tl
;;
*)
