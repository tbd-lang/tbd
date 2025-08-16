type expr =
  | Boolean of bool
  | Integer of int
  | String of string
  | Var of string
  | Call of expr * expr
  | Fun of string * expr

and stmt = Let of string * expr

let rec string_of_expr expr =
  match expr with
  | Boolean b -> Printf.sprintf "Boolean(%b)" b
  | Integer n -> Printf.sprintf "Integer(%d)" n
  | String s -> Printf.sprintf "String(%s)" s
  | Var name -> Printf.sprintf "Var(%s)" name
  | Call (expr1, expr2) ->
    Printf.sprintf "Call(%s, %s)" (string_of_expr expr1) (string_of_expr expr2)
  | Fun (name, expr2) -> Printf.sprintf "Fun(%s, %s)" name (string_of_expr expr2)

and string_of_stmt stmt =
  match stmt with
  | Let (name, expr) -> Printf.sprintf "Let(%s, %s)" name (string_of_expr expr)

and collect_var_or_call tokens =
  let tokens, name =
    match tokens with
    | Lexer.Identifier name :: tl -> tl, name
    | _ -> failwith "Invalid call/var name"
  in
  let rec collect_args tokens acc =
    match tokens with
    | [ Lexer.EOF ] | Lexer.Let :: _ -> tokens, List.rev acc
    | Lexer.Integer n :: tl -> collect_args tl (Integer n :: acc)
    | Lexer.String s :: tl -> collect_args tl (String s :: acc)
    | Lexer.Identifier id :: tl -> collect_args tl (Var id :: acc)
    | _ -> failwith "Unsupported argument type"
  in
  let tokens, args = collect_args tokens [] in
  match args with
  | [] -> tokens, Var name
  | _ ->
    (* Start with the function name as a Var *)
    let call_expr = List.fold_left (fun f arg -> Call (f, arg)) (Var name) args in
    tokens, call_expr

and collect_expr tokens =
  match tokens with
  | Lexer.Boolean b :: tl -> tl, Boolean b
  | Lexer.String s :: tl -> tl, String s
  | Lexer.Identifier _ :: _ ->
    let tokens, expr = collect_var_or_call tokens in
    tokens, expr
  | _ -> failwith "Invalid expr"

and collect_let tokens =
  let tokens, name =
    match tokens with
    | Lexer.Identifier name :: tl -> tl, name
    | _ -> failwith "Invalid let syntax: expected identifier after let"
  in
  let rec collect_params tokens params =
    match tokens with
    | Lexer.Identifier p :: tl -> collect_params tl (params @ [ p ])
    | Lexer.Equal :: tl -> tl, params
    | _ -> failwith "Invalid let syntax: expected parameter or '='"
  in
  let tokens, params = collect_params tokens [] in
  let tokens, body = collect_expr tokens in
  let expr = List.fold_right (fun p acc -> Fun (p, acc)) params body in
  tokens, Let (name, expr)

and collect_stmt tokens =
  match tokens with
  | Lexer.Let :: tl ->
    let tokens, stmt = collect_let tl in
    tokens, stmt
  | _ -> failwith "Invalid statement syntax"
;;

let gen_ast tokens =
  let rec aux tokens stmts =
    match tokens with
    | [] | [ Lexer.EOF ] -> List.rev stmts
    | Lexer.Let :: _ ->
      let tokens, stmt = collect_stmt tokens in
      aux tokens (stmt :: stmts)
    | [ x ] ->
      failwith (Printf.sprintf "Invalid toplevel statement: %s" (Lexer.token_to_string x))
    | hd :: _ ->
      failwith
        (Printf.sprintf "Invalid toplevel statement: %s" (Lexer.token_to_string hd))
  in
  aux tokens []
;;

let rec print_ast stmts =
  match stmts with
  | [] -> ()
  | stmt :: tl ->
    print_endline (string_of_stmt stmt);
    print_ast tl
;;
