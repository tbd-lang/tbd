type expr =
  | Unit
  | Integer of int
  | Float of float
  | Boolean of bool
  | Char of char
  | String of string
  | Identifier of string
  | Call of string * expr list
  | Lambda of string list * stmt list * expr
  | List of expr list

and stmt =
  | Import of string
  | Const of string * expr
  | Let of string * expr
  | Function of string * string list * stmt list * expr
  | Enum of string * string list * (string * typ list) list
  | Struct of string * string list * (string * typ) list

and typ =
  | TypeName of string
  | TypeVar of string

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
  | Lambda (args, stmts, expr) ->
    Printf.sprintf
      "Lambda(%s, %s, %s)"
      (string_of_string_list args)
      (string_of_string_list (List.map (fun s -> string_of_stmt s) stmts))
      (string_of_expr expr)
  | List l ->
    Printf.sprintf
      "List(%s)"
      (string_of_string_list (List.map (fun e -> string_of_expr e) l))

and string_of_stmt stmt =
  match stmt with
  | Import name -> Printf.sprintf "Import(%s)" name
  | Const (name, expr) -> Printf.sprintf "Const(%s, %s)" name (string_of_expr expr)
  | Let (name, expr) -> Printf.sprintf "Let(%s, %s)" name (string_of_expr expr)
  | Function (name, args, stmts, expr) ->
    Printf.sprintf
      "Function(%s, %s, %s, %s)"
      name
      (string_of_string_list args)
      (string_of_string_list (List.map (fun s -> string_of_stmt s) stmts))
      (string_of_expr expr)
  | Enum (name, type_variables, variants) ->
    let variants =
      string_of_string_list
        (List.map
           (fun (n, ts) ->
              Printf.sprintf
                "Variant(%s, %s)"
                n
                (string_of_string_list (List.map (fun t -> string_of_typ t) ts)))
           variants)
    in
    Printf.sprintf "Enum(%s, %s, %s)" name (string_of_string_list type_variables) variants
  | Struct (name, type_variables, fields) ->
    let fields =
      string_of_string_list
        (List.map
           (fun (n, t) -> Printf.sprintf "Field(%s, %s)" n (string_of_typ t))
           fields)
    in
    Printf.sprintf "Struct(%s, %s, %s)" name (string_of_string_list type_variables) fields

and string_of_typ typ =
  match typ with
  | TypeName s -> Printf.sprintf "TypeName(%s)" s
  | TypeVar s -> Printf.sprintf "TypeVar(%s)" s

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
    | Lexer.RightParenthesis :: tl -> tl, args
    | Lexer.Comma :: tl -> collect_args tl args
    | _ ->
      let tl, expr = parse_expr tokens in
      collect_args tl (expr :: args)
  in
  match tokens with
  | Lexer.Identifier name :: Lexer.LeftParenthesis :: tl ->
    let tl, args = collect_args tl [] in
    tl, Call (name, args)
  | _ -> failwith "invalid Call syntax"

and collect_lambda tokens =
  let rec collect_args tokens args =
    match tokens with
    | Lexer.RightParenthesis :: tl -> tl, args
    | Lexer.Comma :: tl -> collect_args tl args
    | Lexer.Identifier arg :: tl -> collect_args tl (arg :: args)
    | _ -> failwith "invalid Lambda argument type"
  in
  let rec collect_body tokens stmts =
    let rec consume_body tokens =
      match tokens with
      | Lexer.RightBrace :: tl -> tl
      | _ :: tl -> consume_body tl
      | _ -> failwith "unclosed Lambda body"
    in
    match tokens with
    | Lexer.LeftBrace :: tl -> collect_body tl stmts
    | Lexer.Let :: _ | Lexer.Function :: Lexer.Identifier _ :: _ ->
      let tl, stmt = parse_stmt tokens in
      collect_body tl (stmt :: stmts)
    | Lexer.RightBrace :: tl -> tl, stmts, Unit
    | _ ->
      let tl, expr = parse_expr tokens in
      let tl = consume_body tl in
      tl, stmts, expr
  in
  match tokens with
  | Lexer.Function :: Lexer.Unit :: tl ->
    let tl, stmts, expr = collect_body tl [] in
    tl, Lambda ([], stmts, expr)
  | Lexer.Function :: Lexer.LeftParenthesis :: tl ->
    let tl, args = collect_args tl [] in
    let tl, stmts, expr = collect_body tl [] in
    tl, Lambda (args, stmts, expr)
  | _ -> failwith "invalid Lambda syntax"

and collect_list tokens exprs =
  match tokens with
  | Lexer.LeftBracket :: tl -> collect_list tl []
  | Lexer.RightBracket :: tl -> tl, List exprs
  | Lexer.Comma :: tl -> collect_list tl exprs
  | _ ->
    let tl, expr = parse_expr tokens in
    collect_list tl (expr :: exprs)

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
  | Lexer.Semicolon :: tl -> tl, Unit
  | Lexer.Function :: Lexer.LeftParenthesis :: _ -> collect_lambda tokens
  | Lexer.LeftBracket :: _ -> collect_list tokens []
  | _ -> failwith "invalid expression syntax"

and collect_function tokens =
  let rec collect_args tokens args =
    match tokens with
    | Lexer.RightParenthesis :: tl -> tl, args
    | Lexer.Comma :: tl -> collect_args tl args
    | Lexer.Identifier arg :: tl -> collect_args tl (arg :: args)
    | _ -> failwith "invalid Function argument type"
  in
  let rec collect_body tokens stmts =
    let rec consume_body tokens =
      match tokens with
      | Lexer.RightBrace :: tl -> tl
      | _ :: tl -> consume_body tl
      | _ -> failwith "unclosed Function body"
    in
    match tokens with
    | Lexer.LeftBrace :: tl -> collect_body tl stmts
    | Lexer.Let :: _ | Lexer.Function :: Lexer.Identifier _ :: _ ->
      let tl, stmt = parse_stmt tokens in
      collect_body tl (stmt :: stmts)
    | Lexer.RightBrace :: tl -> tl, stmts, Unit
    | _ ->
      let tl, expr = parse_expr tokens in
      let tl = consume_body tl in
      tl, stmts, expr
  in
  match tokens with
  | Lexer.Function :: Lexer.Identifier name :: Lexer.Unit :: tl ->
    let tl, stmts, expr = collect_body tl [] in
    tl, Function (name, [], stmts, expr)
  | Lexer.Function :: Lexer.Identifier name :: Lexer.LeftParenthesis :: tl ->
    let tl, args = collect_args tl [] in
    let tl, stmts, expr = collect_body tl [] in
    tl, Function (name, args, stmts, expr)
  | _ -> failwith "invalid Function syntax"

and collect_let tokens =
  match tokens with
  | Lexer.Let :: Lexer.Identifier name :: Lexer.Assignment :: tl ->
    let tl, expr = parse_expr tl in
    tl, Let (name, expr)
  | _ -> failwith "invalid Let syntax"

and parse_stmt tokens =
  match tokens with
  | Lexer.Function :: _ -> collect_function tokens
  | Lexer.Let :: _ -> collect_let tokens
  | _ -> failwith "invalid statement syntax"

and collect_const tokens =
  match tokens with
  | Lexer.Const :: Lexer.Identifier name :: Lexer.Assignment :: tl ->
    let tl, expr = parse_expr tl in
    tl, Const (name, expr)
  | _ -> failwith "invalid Const syntax"

and collect_enum tokens =
  let rec collect_type_vars tokens type_vars =
    match tokens with
    | Lexer.RightChevron :: tl -> tl, type_vars
    | Lexer.Comma :: tl -> collect_type_vars tl type_vars
    | Lexer.Identifier type_arg :: tl -> collect_type_vars tl (type_arg :: type_vars)
    | _ -> failwith "invalid type variable type"
  in
  let rec collect_variant_types type_vars tokens types =
    match tokens with
    | Lexer.LeftParenthesis :: tl -> collect_variant_types type_vars tl types
    | Lexer.RightParenthesis :: tl -> tl, types
    | Lexer.Comma :: tl -> collect_variant_types type_vars tl types
    | Lexer.Identifier type_name :: tl ->
      let t =
        match List.exists (fun t -> String.equal t type_name) type_vars with
        | true -> TypeVar type_name
        | false -> TypeName type_name
      in
      collect_variant_types type_vars tl (t :: types)
    | _ -> failwith "invalid variant type variable type"
  in
  let rec collect_variants type_vars tokens variants =
    match tokens with
    | Lexer.LeftBrace :: tl | Lexer.Comma :: tl -> collect_variants type_vars tl variants
    | Lexer.Identifier variant :: tl ->
      (match tl with
       | Lexer.LeftParenthesis :: _ ->
         let tl, args = collect_variant_types type_vars tl [] in
         collect_variants type_vars tl ((variant, args) :: variants)
       | _ -> collect_variants type_vars tl ((variant, []) :: variants))
    | Lexer.RightBrace :: tl -> tl, variants
    | _ -> failwith "invalid variant type"
  in
  match tokens with
  | Lexer.Enum :: Lexer.Identifier enum :: Lexer.LeftChevron :: tl ->
    let tl, type_vars = collect_type_vars tl [] in
    let tl, variants = collect_variants type_vars tl [] in
    tl, Enum (enum, type_vars, variants)
  | Lexer.Enum :: Lexer.Identifier enum :: tl ->
    let tl, variants = collect_variants [] tl [] in
    tl, Enum (enum, [], variants)
  | _ -> failwith "invalid Enum syntax"

and collect_struct tokens =
  let rec collect_type_vars tokens type_vars =
    match tokens with
    | Lexer.RightChevron :: tl -> tl, type_vars
    | Lexer.Comma :: tl -> collect_type_vars tl type_vars
    | Lexer.Identifier type_arg :: tl -> collect_type_vars tl (type_arg :: type_vars)
    | _ -> failwith "invalid type variable type"
  in
  let rec collect_fields type_vars tokens fields =
    match tokens with
    | Lexer.LeftBrace :: tl | Lexer.Comma :: tl -> collect_fields type_vars tl fields
    | Lexer.Identifier field :: Lexer.Colon :: Lexer.Identifier type_name :: tl ->
      let t =
        match List.exists (fun t -> String.equal t type_name) type_vars with
        | true -> TypeVar type_name
        | false -> TypeName type_name
      in
      collect_fields type_vars tl ((field, t) :: fields)
    | Lexer.RightBrace :: tl -> tl, fields
    | _ -> failwith "invalid field type"
  in
  match tokens with
  | Lexer.Struct :: Lexer.Identifier name :: Lexer.LeftChevron :: tl ->
    let tl, type_vars = collect_type_vars tl [] in
    let tl, fields = collect_fields type_vars tl [] in
    tl, Struct (name, type_vars, fields)
  | Lexer.Struct :: Lexer.Identifier name :: tl ->
    let tl, fields = collect_fields [] tl [] in
    tl, Struct (name, [], fields)
  | _ -> failwith "invalid Struct syntax"

and parse_toplevel tokens =
  match tokens with
  | Lexer.Import :: Lexer.Identifier name :: tl -> tl, Import name
  | Lexer.Const :: _ -> collect_const tokens
  | Lexer.Function :: _ -> collect_function tokens
  | Lexer.Enum :: _ -> collect_enum tokens
  | Lexer.Struct :: _ -> collect_struct tokens
  | _ -> failwith "invalid toplevel syntax"
;;

let rec print_ast stmts =
  match stmts with
  | [] -> ()
  | stmt :: tl ->
    print_endline (string_of_stmt stmt);
    print_ast tl
;;

let rec gen_ast tokens exprs =
  match tokens with
  | [] -> exprs
  | _ ->
    let tokens, expr = parse_toplevel tokens in
    gen_ast tokens (expr :: exprs)
;;
