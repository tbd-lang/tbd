type expr =
  | Unit
  | Boolean of bool
  | Integer of int
  | String of string
  | Var of string
  | Call of expr * expr
  | Fun of string * expr
  | Block of stmt list * expr
  | IfThenElse of expr * expr * expr
  | Tuple of expr list

and stmt =
  | Let of string * expr
  | LetRec of string * expr

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
  | Block (stmts, expr) ->
    Printf.sprintf
      "Block(%s, %s)"
      (Utils.string_of_string_list (List.map (fun s -> string_of_stmt s) stmts))
      (string_of_expr expr)
  | IfThenElse (eif, ethen, eelse) ->
    Printf.sprintf
      "IfThenElse(%s, %s, %s)"
      (string_of_expr eif)
      (string_of_expr ethen)
      (string_of_expr eelse)
  | Tuple exprs ->
    Printf.sprintf
      "Tuple(%s)"
      (Utils.string_of_string_list (List.map (fun e -> string_of_expr e) exprs))

and string_of_stmt stmt =
  match stmt with
  | Let (name, expr) -> Printf.sprintf "Let(%s, %s)" name (string_of_expr expr)
  | LetRec (name, expr) -> Printf.sprintf "LetRec(%s, %s)" name (string_of_expr expr)

and collect_var_or_call tokens =
  let tokens, name =
    match tokens with
    | Lexer.Identifier name :: tl -> tl, name
    | _ -> failwith "Invalid call/var name"
  in
  let rec collect_args tokens acc =
    match tokens with
    | [ Lexer.EOF ]
    | Lexer.Let :: _
    | Lexer.RightBrace :: _
    | Lexer.Semicolon :: _
    | Lexer.Then :: _
    | Lexer.Comma :: _
    | Lexer.RightParenthesis :: _
    | Lexer.Else :: _ -> tokens, List.rev acc
    | Lexer.Unit :: tl -> collect_args tl (Unit :: acc)
    | Lexer.Boolean b :: tl -> collect_args tl (Boolean b :: acc)
    | Lexer.Integer n :: tl -> collect_args tl (Integer n :: acc)
    | Lexer.String s :: tl -> collect_args tl (String s :: acc)
    | Lexer.Identifier id :: tl -> collect_args tl (Var id :: acc)
    | Lexer.LeftParenthesis :: tl ->
      let tokens, expr = collect_paren tl in
      collect_args tokens (expr :: acc)
    | hd :: _ ->
      failwith (Printf.sprintf "Unsupported argument type: %s" (Lexer.token_to_string hd))
    | _ -> failwith "Unsupported argument type"
  in
  let tokens, args = collect_args tokens [] in
  match args with
  | [] -> tokens, Var name
  | _ ->
    let call_expr = List.fold_left (fun f arg -> Call (f, arg)) (Var name) args in
    tokens, call_expr

and collect_block tokens =
  let rec aux tokens stmts =
    match tokens with
    | Lexer.Let :: _ ->
      let tokens, stmt = collect_stmt tokens in
      (match Utils.advance tokens with
       | Some (Lexer.Semicolon, tl) -> aux tl (stmt :: stmts)
       | _ -> failwith "Block let requires ;")
    | _ ->
      let tokens, expr = collect_expr tokens in
      (match tokens with
       | Lexer.RightBrace :: tl -> tl, Block (List.rev stmts, expr)
       | Lexer.Semicolon :: tl -> aux tl (Let ("()", expr) :: stmts)
       | hd :: _ -> failwith (Lexer.token_to_string hd)
       | _ -> failwith "Block must end in an expression")
  in
  aux tokens []

and collect_if tokens =
  let tokens, eif = collect_expr tokens in
  let tokens, ethen =
    match tokens with
    | Lexer.Then :: tl -> collect_expr tl
    | _ -> failwith "If statement requires then"
  in
  let tokens, eelse =
    match tokens with
    | Lexer.Else :: tl -> collect_expr tl
    | _ -> failwith "If statement requires then"
  in
  tokens, IfThenElse (eif, ethen, eelse)

and collect_paren tokens =
  let tokens, first_expr = collect_expr tokens in
  match tokens with
  | Lexer.RightParenthesis :: tl -> tl, first_expr
  | Lexer.Comma :: tl ->
    let rec aux acc tokens =
      let tokens, expr = collect_expr tokens in
      match tokens with
      | Lexer.Comma :: tl -> aux (expr :: acc) tl
      | Lexer.RightParenthesis :: tl -> tl, Tuple (List.rev (expr :: acc))
      | _ -> failwith "expected , or ) in tuple"
    in
    aux [ first_expr ] tl
  | _ -> failwith "expected , or ) after (expr"

and collect_expr tokens =
  match tokens with
  | Lexer.LeftParenthesis :: tl ->
    let tokens, expr = collect_paren tl in
    tokens, expr
  | Lexer.Unit :: tl -> tl, Unit
  | Lexer.Boolean b :: tl -> tl, Boolean b
  | Lexer.Integer n :: tl -> tl, Integer n
  | Lexer.Minus :: Lexer.Integer n :: tl -> tl, Integer (-1 * n)
  | Lexer.String s :: tl -> tl, String s
  | Lexer.Identifier _ :: _ ->
    let tokens, expr = collect_var_or_call tokens in
    tokens, expr
  | Lexer.LeftBrace :: tl ->
    let tokens, expr = collect_block tl in
    tokens, expr
  | Lexer.If :: tl ->
    let tokens, expr = collect_if tl in
    tokens, expr
  | hd :: _ -> failwith (Printf.sprintf "Invalid expr: %s" (Lexer.token_to_string hd))
  | _ -> failwith "Invalid expr"

and collect_let tokens =
  let tokens, is_rec =
    match Utils.peek tokens with
    | Some Lexer.Rec ->
      let _, tokens =
        match Utils.advance tokens with
        | Some x -> x
        | _ -> failwith "could not advance when checking recursiveness"
      in
      tokens, true
    | _ -> tokens, false
  in
  let tokens, name =
    match tokens with
    | Lexer.Unit :: tl -> tl, "()"
    | Lexer.Identifier name :: tl -> tl, name
    | _ -> failwith "Invalid let syntax: expected identifier after let"
  in
  let rec collect_params tokens params =
    match tokens with
    | Lexer.Identifier p :: tl -> collect_params tl (params @ [ p ])
    | Lexer.Unit :: tl -> collect_params tl (params @ [ "()" ])
    | Lexer.Equal :: tl -> tl, params
    | _ -> failwith "Invalid let syntax: expected parameter or '='"
  in
  let tokens, params = collect_params tokens [] in
  let tokens, body = collect_expr tokens in
  let expr =
    match params with
    | [] -> body
    | _ -> List.fold_right (fun p acc -> Fun (p, acc)) params body
  in
  tokens, if is_rec then LetRec (name, expr) else Let (name, expr)

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
