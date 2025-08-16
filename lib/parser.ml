type expr =
  | Unit
  | Boolean of bool
  | Integer of int
  | String of string
  | Var of string
  | Call of expr * expr
  | Fun of string * expr
  | Match of expr * expr * expr

and stmt = Let of string option * expr

let rec string_of_expr expr =
  match expr with
  | Unit -> "Unit"
  | Boolean b -> Printf.sprintf "Boolean(%b)" b
  | Integer n -> Printf.sprintf "Integer(%d)" n
  | String s -> Printf.sprintf "String(%s)" s
  | Var name -> Printf.sprintf "Var(%s)" name
  | Call (expr1, expr2) ->
    Printf.sprintf "Call(%s, %s)" (string_of_expr expr1) (string_of_expr expr2)
  | Fun (name, expr2) -> Printf.sprintf "Fun(%s, %s)" name (string_of_expr expr2)
  | Match (cond, yes, no) ->
    Printf.sprintf
      "Match(%s, %s, %s)"
      (string_of_expr cond)
      (string_of_expr yes)
      (string_of_expr no)

and string_of_stmt stmt =
  match stmt with
  | Let (name, expr) ->
    Printf.sprintf
      "Let(%s, %s)"
      (match name with
       | Some name -> name
       | None -> "None")
      (string_of_expr expr)

and collect_var_or_call tokens =
  let tokens, name =
    match tokens with
    | Lexer.Identifier name :: tl -> tl, name
    | _ -> failwith "Invalid call/var name"
  in
  let rec collect_args tokens acc =
    match tokens with
    | [ Lexer.EOF ] | Lexer.Let :: _ | Lexer.LeftBrace :: _ -> tokens, List.rev acc
    | Lexer.RightParenthesis :: tl -> tl, List.rev acc
    | Lexer.Integer n :: tl -> collect_args tl (Integer n :: acc)
    | Lexer.String s :: tl -> collect_args tl (String s :: acc)
    | Lexer.Identifier id :: tl -> collect_args tl (Var id :: acc)
    | Lexer.LeftParenthesis :: tl ->
      let tokens, expr = collect_expr tl in
      collect_args tokens (expr :: acc)
    | hd :: _ ->
      failwith (Printf.sprintf "Unsupported argument type: %s" (Lexer.token_to_string hd))
    | _ -> failwith "Unsupported argument type"
  in
  let tokens, args = collect_args tokens [] in
  match args with
  | [] -> tokens, Var name
  | _ ->
    (* Start with the function name as a Var *)
    let call_expr = List.fold_left (fun f arg -> Call (f, arg)) (Var name) args in
    tokens, call_expr

and collect_match tokens =
  let tokens, cond = collect_expr tokens in
  match tokens with
  | Lexer.LeftBrace :: Lexer.Boolean true :: Lexer.Arrow :: tl1 ->
    let tokens, e1 = collect_expr tl1 in
    (match tokens with
     | Lexer.Comma :: Lexer.Boolean false :: Lexer.Arrow :: tl2 ->
       let tokens, e2 = collect_expr tl2 in
       (match tokens with
        | Lexer.RightBrace :: tl3 -> tl3, Match (cond, e1, e2)
        | _ -> failwith "expected '}' after match")
     | _ -> failwith "expected 'false -> expr' in match")
  | _ -> failwith "expected '{ true -> expr , false -> expr }'"

and collect_expr tokens =
  match tokens with
  | Lexer.RightParenthesis :: tl -> collect_expr tl
  | Lexer.LeftParenthesis :: tl ->
    let tokens, expr = collect_expr tl in
    tokens, expr
  | Lexer.Unit :: tl -> tl, Unit
  | Lexer.Boolean b :: tl -> tl, Boolean b
  | Lexer.Integer n :: tl -> tl, Integer n
  | Lexer.String s :: tl -> tl, String s
  | Lexer.Identifier _ :: _ ->
    let tokens, expr = collect_var_or_call tokens in
    tokens, expr
  | Lexer.Match :: tl ->
    let tokens, expr = collect_match tl in
    tokens, expr
  | hd :: _ -> failwith (Printf.sprintf "Invalid expr: %s" (Lexer.token_to_string hd))
  | _ -> failwith "Invalid expr"

and collect_let tokens =
  let tokens, name =
    match tokens with
    | Lexer.Unit :: tl -> tl, None
    | Lexer.Identifier name :: tl -> tl, Some name
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
