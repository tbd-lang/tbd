open Ast

let rec emit_expr expr =
  match expr with
  | Unit -> "()"
  | Bool b -> string_of_bool b ^ " "
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
  | Seq (e1, e2) -> emit_expr e1 ^ "; " ^ emit_expr e2
  | Parens e -> "(" ^ emit_expr e ^ ")"
  | If (cond, e1, e2) ->
    "if " ^ emit_expr cond ^ " then " ^ emit_expr e1 ^ " else " ^ emit_expr e2
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
  | TInt -> " int "
  | TVar name -> " '" ^ name ^ " "
  | TTuple typs -> "(" ^ String.concat " * " (List.map emit_typ typs) ^ ")"

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
  | DTypVariant (name, typvars, variants) ->
    "type "
    ^ (match typvars with
       | [] -> ""
       | _ -> "(" ^ String.concat ", " (List.map (fun s -> "'" ^ s) typvars) ^ ") ")
    ^ name
    ^ " = "
    ^ String.concat
        ""
        (List.map
           (fun (name, typ) ->
              match typ with
              | Some typ -> "| " ^ name ^ " of " ^ emit_typ typ
              | None -> "| " ^ name)
           variants)
;;

let emit_program program = String.concat "\n\n" (List.map emit_decl program)

open Format

let pp_program fmt prog = pp_print_string fmt (emit_program prog)
