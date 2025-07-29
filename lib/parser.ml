type expr =
  | Unit
  | Integer of int
  | Float of float
  | Boolean of bool
  | Char of char
  | String of string
  | Identifier of string
  | Block of expr list
  | Let of string * expr
  | Function of string * string list * expr
  | RecFunction of string * string list * expr
  | BinOp of string * expr * expr
  | Call of expr * expr list
;;


let rec expr_to_string e =
  let string_list_to_string l =
    let rec aux l acc =
      match l with
      | [] -> acc ^ "]"
      | [hd] -> aux [] ((acc ^ hd))
      | hd :: tl -> aux tl ((acc ^ hd ^ ", "))
    in aux l "["
  in 

  let expr_list_to_string l =
    let rec aux l acc =
      match l with
      | [] -> acc ^ "]"
      | [hd] -> aux [] ((acc ^ (expr_to_string hd)))
      | hd :: tl -> aux tl ((acc ^ (expr_to_string hd) ^ ", "))
    in aux l "["
  in

  match e with
  | Unit -> "Unit"
  | Integer x -> Printf.sprintf "Integer(%d)" x
  | Float x -> Printf.sprintf "Float(%f)" x
  | Boolean b -> Printf.sprintf "Boolean(%b)" b
  | Char c -> Printf.sprintf "Char(%c)" c
  | String s -> Printf.sprintf "String(%s)" s
  | Identifier s -> Printf.sprintf "Identifier(%s)" s
  | Block es -> Printf.sprintf "Block(%s)" (expr_list_to_string es)
  | Let (s, e) -> Printf.sprintf "Let(%s, %s)" s (expr_to_string e)
  | Function (s, args, e) -> Printf.sprintf "Function(%s, %s, %s)" s (string_list_to_string args) (expr_to_string e)
  | RecFunction (s, args, e) -> Printf.sprintf "RecFunction(%s, %s, %s)" s (string_list_to_string args) (expr_to_string e)
  | BinOp (s, e1, e2) -> Printf.sprintf "BinOp(%s, %s, %s)" s (expr_to_string e1) (expr_to_string e2)
  | Call (e, es) -> Printf.sprintf "Call(%s, %s)" (expr_to_string e) (expr_list_to_string es)
;;


let gen_ast tokens =
  let rec parse_expr tokens acc =
    match tokens with
    | [] -> Block (List.rev acc)
    | Lexer.Let :: Lexer.Identifier name :: Lexer.Assignment :: tl -> parse_expr tl (Let (name, Unit) :: acc)
    | Lexer.Fun :: Lexer.Identifier name :: tl -> parse_expr tl (Function (name, [], Unit) :: acc)
    | Lexer.Unit :: tl -> parse_expr tl (Unit :: acc)
    | Lexer.Integer x :: tl -> parse_expr tl (Integer x :: acc)
    | Lexer.Float f :: tl -> parse_expr tl (Float f :: acc)
    | Lexer.Boolean b :: tl -> parse_expr tl (Boolean b :: acc)
    | Lexer.Char c :: tl -> parse_expr tl (Char c :: acc)
    | Lexer.String s :: tl -> parse_expr tl (String s :: acc)
    | Lexer.Identifier s :: tl -> parse_expr tl (Identifier s :: acc)
    | _ :: tl -> parse_expr tl (Unit :: acc)
  in
  parse_expr tokens []
;;


let print_ast e =
  print_string (expr_to_string e);
  print_newline ()
;;