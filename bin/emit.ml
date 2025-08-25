open Ast

let rec emit_pattern pat =
  match pat with
  | PBool true -> "true"
  | PBool false -> "false"
  | PChar c -> "'" ^ String.make 1 c ^ "'"
  | PInt n -> string_of_int n
  | PFloat f -> string_of_float f
  | PString s -> "\"" ^ s ^ "\""
  | PVar v -> v
  | PWildcard -> "_"
  | PTuple ps -> "(" ^ String.concat ", " (List.map emit_pattern ps) ^ ")"
  | PConstr (name, args) ->
    name
    ^
      (match args with
      | [] -> ""
      | _ ->
        " ("
        ^ String.concat
            ","
            (List.map
               (fun p ->
                  match p with
                  | PTuple _ -> "(" ^ emit_pattern p ^ ")"
                  | _ -> emit_pattern p)
               args)
        ^ ")")
  | PCons (l, r) -> emit_pattern l ^ " :: " ^ emit_pattern r
  | PEmptyList -> "[]"
  | PList items ->
    let rec collect items acc =
      match items with
      | [] -> acc ^ "]"
      | [ x ] -> collect [] (acc ^ emit_pattern x)
      | hd :: tl -> collect tl (acc ^ emit_pattern hd ^ "; ")
    in
    collect items "["

and emit_expr expr =
  match expr with
  | Unit -> "()"
  | Bool b -> string_of_bool b
  | Char c -> "'" ^ String.make 1 c ^ "'"
  | Int n -> string_of_int n
  | Float n -> string_of_float n
  | String s -> "\"" ^ s ^ "\""
  | Tuple items ->
    let rec collect items acc =
      match items with
      | [] -> acc ^ ")"
      | [ x ] -> collect [] (acc ^ emit_expr x)
      | hd :: tl -> collect tl (acc ^ emit_expr hd ^ ", ")
    in
    collect items "("
  | List items ->
    let rec collect items acc =
      match items with
      | [] -> acc ^ "]"
      | [ x ] -> collect [] (acc ^ emit_expr x)
      | hd :: tl -> collect tl (acc ^ emit_expr hd ^ "; ")
    in
    collect items "["
  | Array items ->
    let rec collect items acc =
      match items with
      | [] -> acc ^ "|]"
      | [ x ] -> collect [] (acc ^ emit_expr x)
      | hd :: tl -> collect tl (acc ^ emit_expr hd ^ "; ")
    in
    collect items "[|"
  | Var name -> name
  | Let (name, e1, e2) -> "let " ^ name ^ " = " ^ emit_expr e1 ^ " in\n" ^ emit_expr e2
  | Fun (name, params, body, next) ->
    let params =
      match params with
      | [] -> [ "()" ]
      | _ -> params
    in
    "let "
    ^ name
    ^ " "
    ^ String.concat " " params
    ^ " = "
    ^ emit_expr body
    ^ " in\n"
    ^ emit_expr next
  | FunRec (funs, next) ->
    (match funs with
     | [] -> ""
     | (name, params, body) :: rest_funs ->
       let first =
         let params =
           match params with
           | [] -> [ "()" ]
           | _ -> params
         in
         "let rec " ^ name ^ " " ^ String.concat " " params ^ " = " ^ emit_expr body
       in
       let rest =
         List.map
           (fun (name, params, body) ->
              let params =
                match params with
                | [] -> [ "()" ]
                | _ -> params
              in
              "and " ^ name ^ " " ^ String.concat " " params ^ " = " ^ emit_expr body)
           rest_funs
       in
       String.concat "\n" (first :: rest))
    ^ " in\n"
    ^ emit_expr next
  | Lambda (params, body) ->
    let params =
      match params with
      | [] -> [ "()" ]
      | _ -> params
    in
    "(fun " ^ String.concat " " params ^ " -> " ^ emit_expr body ^ ")\n"
  | Call (expr, args) ->
    let args =
      match args with
      | [] -> [ Unit ]
      | _ -> args
    in
    "("
    ^ emit_expr expr
    ^ " "
    ^ String.concat " " (List.map (fun arg -> "(" ^ emit_expr arg ^ ")") args)
    ^ ")"
  | Constr (name, []) -> name
  | Constr (name, args) ->
    (match args with
     | [ arg ] -> name ^ " " ^ emit_expr arg
     | args -> name ^ " (" ^ String.concat ", " (List.map emit_expr args) ^ ")")
  | Seq (e1, e2) -> emit_expr e1 ^ "; " ^ emit_expr e2
  | Parens e -> "(" ^ emit_expr e ^ ")"
  | If (cond, e1, e2) ->
    "if " ^ emit_expr cond ^ " then " ^ emit_expr e1 ^ " else " ^ emit_expr e2
  | Match (e, cases) ->
    "match "
    ^ emit_expr e
    ^ " with\n"
    ^ String.concat
        "\n"
        (List.map (fun (p, rhs) -> "| " ^ emit_pattern p ^ " -> " ^ emit_expr rhs) cases)
  | Add (a, b) -> emit_expr a ^ " + " ^ emit_expr b
  | Sub (a, b) -> emit_expr a ^ " - " ^ emit_expr b
  | Mul (a, b) -> emit_expr a ^ " * " ^ emit_expr b
  | Div (a, b) -> emit_expr a ^ " / " ^ emit_expr b
  | FAdd (a, b) -> emit_expr a ^ " +. " ^ emit_expr b
  | FSub (a, b) -> emit_expr a ^ " -. " ^ emit_expr b
  | FMul (a, b) -> emit_expr a ^ " *. " ^ emit_expr b
  | FDiv (a, b) -> emit_expr a ^ " /. " ^ emit_expr b
  | Concat (a, b) -> emit_expr a ^ " ^ " ^ emit_expr b
  | Cons (a, b) -> emit_expr a ^ " :: " ^ emit_expr b
  | Equal (a, b) -> emit_expr a ^ " = " ^ emit_expr b
  | NotEqual (a, b) -> emit_expr a ^ " <> " ^ emit_expr b
  | Gt (a, b) -> emit_expr a ^ " > " ^ emit_expr b
  | Gte (a, b) -> emit_expr a ^ " >= " ^ emit_expr b
  | Lt (a, b) -> emit_expr a ^ " < " ^ emit_expr b
  | Lte (a, b) -> emit_expr a ^ " <= " ^ emit_expr b
  | And (a, b) -> emit_expr a ^ " && " ^ emit_expr b
  | Or (a, b) -> emit_expr a ^ " || " ^ emit_expr b

and emit_typ typ =
  match typ with
  | TVar name -> " '" ^ name ^ " "
  | TTuple typs -> "(" ^ String.concat " * " (List.map emit_typ typs) ^ ")"
  | TConstr (name, typ) ->
    (match typ with
     | Some typ -> "| " ^ name ^ " of " ^ emit_typ typ
     | None -> "| " ^ name)
  | TApp (name, args) ->
    let lname = String.uncapitalize_ascii name in
    (match args with
     | [] -> "" ^ lname
     | [ t ] -> emit_typ t ^ " " ^ lname
     | ts -> "(" ^ String.concat ", " (List.map emit_typ ts) ^ ") " ^ lname)
  | TRecord fields ->
    "{ "
    ^ String.concat "; " (List.map (fun (n, t) -> n ^ " : " ^ emit_typ t) fields)
    ^ " }"

and emit_decl decl =
  match decl with
  | DFun (name, params, body) ->
    if name = "main"
    then "let () = " ^ emit_expr body
    else (
      let params =
        match params with
        | [] -> [ "()" ]
        | _ -> params
      in
      "let " ^ name ^ " " ^ String.concat " " params ^ " = " ^ emit_expr body)
  | DFunRec funs ->
    (match funs with
     | [] -> ""
     | (name, params, body) :: rest_funs ->
       let first =
         let params =
           match params with
           | [] -> [ "()" ]
           | _ -> params
         in
         "let rec " ^ name ^ " " ^ String.concat " " params ^ " = " ^ emit_expr body
       in
       let rest =
         List.map
           (fun (name, params, body) ->
              let params =
                match params with
                | [] -> [ "()" ]
                | _ -> params
              in
              "and " ^ name ^ " " ^ String.concat " " params ^ " = " ^ emit_expr body)
           rest_funs
       in
       String.concat "\n" (first :: rest))
  | DExtern (name, params, code) ->
    "let " ^ name ^ " " ^ String.concat " " params ^ " = " ^ code
  | DModule (name, decls) ->
    "module "
    ^ name
    ^ " = struct\n"
    ^ String.concat "\n" (List.map emit_decl decls)
    ^ "\nend"
  | DTypVariant (name, typvars, typs) ->
    "type "
    ^ (match typvars with
       | [] -> ""
       | _ -> "(" ^ String.concat ", " (List.map (fun s -> "'" ^ s) typvars) ^ ") ")
    ^ name
    ^ " = "
    ^ String.concat "" (List.map emit_typ typs)
  | DTypAlias (name, typvars, typ, typvars2) ->
    "type "
    ^ (match typvars with
       | [] -> ""
       | _ -> "(" ^ String.concat ", " (List.map (fun s -> "'" ^ s) typvars) ^ ") ")
    ^ name
    ^ " = "
    ^ (match typvars2 with
       | [] -> ""
       | _ -> "(" ^ String.concat ", " (List.map emit_typ typvars2) ^ ") ")
    ^ typ
  | DTypeRecord (name, params, fields) ->
    let params =
      match params with
      | [] -> ""
      | ps -> "(" ^ String.concat ", " (List.map (fun v -> "'" ^ v) ps) ^ ") "
    in
    "type "
    ^ params
    ^ String.uncapitalize_ascii name
    ^ " = {\n"
    ^ String.concat ";\n" (List.map (fun (n, t) -> "  " ^ n ^ " : " ^ emit_typ t) fields)
    ^ "\n}"
;;

let emit_program program = String.concat "\n\n" (List.map emit_decl program)

open Format

let pp_program fmt prog = pp_print_string fmt (emit_program prog)
