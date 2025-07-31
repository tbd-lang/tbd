type expr =
  | Unit
  | Integer of int
  | Float of float
  | Boolean of bool
  | Char of char
  | String of string
  | Identifier of string

and stmt =
  | Import of string
  | Const of string * expr
  | Function of string * string list * stmt list * expr
  | Let of string * expr
  | Call of string * string list

let parse_expr tokens =
  match tokens with
  | [] -> (tokens, Unit)
  | Lexer.Unit :: tl -> (tl, Unit)
  | Lexer.Integer n :: tl -> (tl, Integer n)
  | Lexer.Float f :: tl -> (tl, Float f)
  | Lexer.Boolean b :: tl -> (tl, Boolean b)
  | Lexer.Char c :: tl -> (tl, Char c)
  | Lexer.String s :: tl -> (tl, String s)
  | Lexer.Identifier name :: tl -> (tl, Identifier name)
  | _ -> (tokens, Unit)

let string_list_to_string l =
  let rec aux l acc =
    match l with
    | [] -> "[" ^ acc
    | [ hd ] -> aux [] (hd ^ acc)
    | hd :: tl -> aux tl (", " ^ hd ^ acc)
  in
  aux l "]"

let rec expr_to_string expr =
  match expr with
  | Unit -> "Unit"
  | Integer n -> Printf.sprintf "Integer(%d)" n
  | Float f -> Printf.sprintf "Float(%f)" f
  | Boolean b -> Printf.sprintf "Boolean(%b)" b
  | Char c -> Printf.sprintf "Char(%c)" c
  | String s -> Printf.sprintf "String(%s)" s
  | Identifier name -> Printf.sprintf "Identifier(%s)" name

and stmt_to_string stmt =
  match stmt with
  | Import name -> Printf.sprintf "Import(%s)" name
  | Const (name, expr) ->
      Printf.sprintf "Const(%s, %s)" name (expr_to_string expr)
  | Function (name, args, stmts, expr) ->
      Printf.sprintf "Function(%s, %s, %s, %s)" name
        (string_list_to_string args)
        (string_list_to_string (List.map (fun s -> stmt_to_string s) stmts))
        (expr_to_string expr)
  | Let (name, expr) -> Printf.sprintf "Let(%s, %s)" name (expr_to_string expr)
  | Call (name, args) ->
      Printf.sprintf "Call(%s, %s)" name (string_list_to_string args)

let collect_import tokens =
  match tokens with
  | [] -> failwith "Unclosed import"
  | Lexer.Identifier name :: tl -> (tl, Import name)
  | _ -> failwith "Unsupported import syntax"

let collect_const tokens =
  match tokens with
  | [] -> failwith "Unclosed const"
  | Lexer.Identifier name :: Lexer.Assignment :: tl -> (
      match tl with
      | [] -> failwith "Unclosed const"
      | Lexer.Integer n :: tl' -> (tl', Const (name, Integer n))
      | Lexer.Float f :: tl' -> (tl', Const (name, Float f))
      | Lexer.Boolean b :: tl' -> (tl', Const (name, Boolean b))
      | Lexer.Char c :: tl' -> (tl', Const (name, Char c))
      | Lexer.String s :: tl' -> (tl', Const (name, String s))
      | _ -> failwith "Unsupported const type")
  | _ -> failwith "Unsupported const syntax"

let collect_function tokens =
  let rec collect_args tokens args =
    match tokens with
    | [] -> failwith "Unclosed function"
    | Lexer.Identifier name :: Lexer.Comma :: tl ->
        collect_args tl (name :: args)
    | Lexer.Identifier name :: tl -> collect_args tl (name :: args)
    | Lexer.RightParenthesis :: tl -> (tl, args)
    | _ -> failwith "Unsupported function argument type"
  in

  let rec collect_stmts tokens stmts =
    match tokens with
    | [] -> failwith "Unclosed function"
    | Lexer.RightBrace :: tl -> (tl, stmts)
    | _ :: tl -> collect_stmts tl []
  in

  match tokens with
  | [] -> failwith "Unclosed function"
  | Lexer.Identifier name :: Lexer.Unit :: Lexer.LeftBrace :: tl ->
      let tokens, expr = parse_expr tl in
      let tokens, stmts = collect_stmts tokens [] in
      (tokens, Function (name, stmts, stmts, expr))
  | Lexer.Identifier name :: Lexer.LeftParenthesis :: tl ->
      let tokens, args = collect_args tl [] in
      let tokens, stmts = collect_stmts tokens [] in
      let tokens, expr = parse_expr tokens in
      (tokens, Function (name, args, stmts, expr))
  | _ -> failwith "Unsupported function name type"

let rec parse_stmt tokens stmts =
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
  | _ -> failwith "Unsupported top-level expression"

let rec print_ast stmts =
  match stmts with
  | [] -> ()
  | stmt :: tl ->
      print_endline (stmt_to_string stmt);
      print_ast tl
