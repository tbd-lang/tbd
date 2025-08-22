open Ast

let rec emit_expr expr =
  match expr with
  | Unit -> "()"
  | Char c -> "'" ^ String.make 1 c ^ "'"
  | Int n -> string_of_int n
  | Float n -> string_of_float n
  | String s -> "\"" ^ s ^ "\""
  | Var name -> name
  | Let (name, e1, e2) -> "let " ^ name ^ " = " ^ emit_expr e1 ^ " in " ^ emit_expr e2
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
    ^ " in "
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
    ^ " in "
    ^ emit_expr next
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
  | Equal (a, b) -> emit_expr a ^ " = " ^ emit_expr b
  | NotEqual (a, b) -> emit_expr a ^ " <> " ^ emit_expr b
  | Gt (a, b) -> emit_expr a ^ " > " ^ emit_expr b
  | Gte (a, b) -> emit_expr a ^ " >= " ^ emit_expr b
  | Lt (a, b) -> emit_expr a ^ " < " ^ emit_expr b
  | Lte (a, b) -> emit_expr a ^ " <= " ^ emit_expr b

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
;;

let emit_program program = String.concat "\n\n" (List.map emit_decl program)

open Format

let pp_program fmt prog = pp_print_string fmt (emit_program prog)
