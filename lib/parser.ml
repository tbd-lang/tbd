type expr =
  | Unit
  | Integer of int
  | Float of float
  | Boolean of bool
  | Char of char
  | String of string
  | Identifier of string


and stmt =
  | Const of string * expr
  | Function of string * string list * expr
;;


let parse_expr tokens =
  match tokens with
  | [] -> failwith "Unfinished expression"
  | Lexer.Unit :: tl -> tl, Unit
  | Lexer.Integer n :: tl -> tl, Integer n
  | Lexer.Float f :: tl -> tl, Float f
  | Lexer.Boolean b :: tl -> tl, Boolean b
  | Lexer.Char c :: tl -> tl, Char c
  | Lexer.String s :: tl -> tl, String s
  | Lexer.Identifier name :: tl -> tl, Identifier name
  | _ -> failwith "Unsupported expression type"
;;


let string_list_to_string l =
  let rec aux l acc =
    match l with
    | [] -> "[" ^ acc
    | [hd] -> aux [] (hd ^ acc)
    | hd :: tl -> aux tl (", " ^ hd ^ acc)
  in aux l "]"
;;


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
  | Const (name, expr) -> Printf.sprintf "Const(%s, %s)" name (expr_to_string expr)
  | Function (name, args, expr) ->
    Printf.sprintf "Function(%s, %s, %s)" name (string_list_to_string args) (expr_to_string expr)
;;


let collect_const tokens =
  match tokens with
  | [] -> failwith "Unclosed const"
  | Lexer.Identifier name :: Lexer.Assignment :: tl -> 
    (match tl with
    | [] -> failwith "Unclosed const"
    | Lexer.Integer n :: tl' -> tl', Const(name, Integer n)
    | Lexer.Float f :: tl' -> tl', Const(name, Float f)
    | Lexer.Boolean b :: tl' -> tl', Const(name, Boolean b)
    | Lexer.Char c :: tl' -> tl', Const(name, Char c)
    | Lexer.String s :: tl' -> tl', Const(name, String s)
    | _ -> failwith "Unsupported const type")
  | _ -> failwith "Unsupported const syntax"
;;


let collect_function tokens =
  let rec collect_args tokens args =
    match tokens with
    | [] -> failwith "Unclosed function"
    | Lexer.Assignment :: tl -> tl, args
    | Lexer.Identifier name :: tl -> collect_args tl (name :: args)
    | _ -> failwith "Unsupported function argument type"
  in 

  match tokens with
  | [] -> failwith "Unclosed function"
  | Lexer.Identifier name :: tl ->
    (let tokens, args = collect_args tl [] in
    let tokens, expr = parse_expr tokens in
    tokens, Function (name, args, expr))
  | _ -> failwith "Unsupported function name type"
;;


let rec gen_ast tokens stmts =
  match tokens with
  | [] -> List.rev stmts
  | Lexer.Const :: tl ->
    (let tokens, expr = collect_const tl in
    gen_ast tokens (expr :: stmts))
  | Lexer.Function :: tl ->
    (let tokens, expr = collect_function tl in
    gen_ast tokens (expr :: stmts))
  | _ -> failwith "Unsupported top-level expression"
;;


let rec print_ast stmts =
  match stmts with
  | [] -> ()
  | stmt :: tl ->
    (print_endline (stmt_to_string stmt);
    print_ast tl)
;;