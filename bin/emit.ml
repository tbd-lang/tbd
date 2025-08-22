open Ast

let space_ = " "
let let_ = "let "
let letrec_ = "let rec "
let in_ = "in "
let eq_ = "= "
let if_ = "if "
let then_ = "then "
let else_ = "else "

let rec emit_expr expr =
  match expr with
  | Unit -> "()" ^ space_
  | Char c -> "'" ^ String.make 1 c ^ "'" ^ space_
  | Int n -> string_of_int n ^ space_
  | Float n -> string_of_float n ^ space_
  | String s -> "\"" ^ s ^ "\"" ^ space_
  | Var name -> name ^ space_
  | Let (name, expr, next) -> let_ ^ name ^ eq_ ^ emit_expr expr ^ in_ ^ emit_expr next
  | Fun (name, params, expr, next) ->
    let params = List.rev params in
    let_
    ^ name
    ^ space_
    ^ List.fold_right (fun param acc -> acc ^ param ^ space_) params ""
    ^ eq_
    ^ emit_expr expr
    ^ in_
    ^ emit_expr next
  | FunRec (name, params, expr, next) ->
    let params = List.rev params in
    letrec_
    ^ name
    ^ space_
    ^ List.fold_right (fun param acc -> acc ^ param ^ space_) params ""
    ^ eq_
    ^ emit_expr expr
    ^ in_
    ^ emit_expr next
  | Call (name, args) ->
    let args = List.rev args in
    name ^ space_ ^ List.fold_right (fun arg acc -> acc ^ emit_expr arg) args ""
  | Parens expr -> "(" ^ emit_expr expr ^ ")" ^ space_
  | If (cond, ethen, eelse) ->
    if_ ^ emit_expr cond ^ then_ ^ emit_expr ethen ^ else_ ^ emit_expr eelse
  | Add (a, b) -> emit_expr a ^ space_ ^ "+" ^ space_ ^ emit_expr b
  | Sub (a, b) -> emit_expr a ^ space_ ^ "-" ^ space_ ^ emit_expr b
  | Mul (a, b) -> emit_expr a ^ space_ ^ "*" ^ space_ ^ emit_expr b
  | Div (a, b) -> emit_expr a ^ space_ ^ "/" ^ space_ ^ emit_expr b
  | FAdd (a, b) -> emit_expr a ^ space_ ^ "+." ^ space_ ^ emit_expr b
  | FSub (a, b) -> emit_expr a ^ space_ ^ "-." ^ space_ ^ emit_expr b
  | FMul (a, b) -> emit_expr a ^ space_ ^ "*." ^ space_ ^ emit_expr b
  | FDiv (a, b) -> emit_expr a ^ space_ ^ "/." ^ space_ ^ emit_expr b
  | Equal (a, b) -> emit_expr a ^ space_ ^ "=" ^ space_ ^ emit_expr b
  | NotEqual (a, b) -> emit_expr a ^ space_ ^ "<>" ^ space_ ^ emit_expr b
  | Gt (a, b) -> emit_expr a ^ space_ ^ ">" ^ space_ ^ emit_expr b
  | Gte (a, b) -> emit_expr a ^ space_ ^ ">=" ^ space_ ^ emit_expr b
  | Lt (a, b) -> emit_expr a ^ space_ ^ "<" ^ space_ ^ emit_expr b
  | Lte (a, b) -> emit_expr a ^ space_ ^ "<=" ^ space_ ^ emit_expr b
  | PrintInt expr -> "print_endline (string_of_int " ^ emit_expr expr ^ ")" ^ space_

and emit_decl decl =
  match decl with
  | DFun (name, params, expr) ->
    (match name with
     | "main" -> let_ ^ "()" ^ space_ ^ eq_ ^ emit_expr expr
     | _ ->
       let params = List.rev params in
       let_
       ^ name
       ^ space_
       ^ List.fold_right (fun param acc -> acc ^ param ^ space_) params ""
       ^ eq_
       ^ emit_expr expr)
  | DFunRec (name, params, expr) ->
    (match name with
     | "main" -> let_ ^ "()" ^ space_ ^ eq_ ^ emit_expr expr
     | _ ->
       let params = List.rev params in
       letrec_
       ^ name
       ^ space_
       ^ List.fold_right (fun param acc -> acc ^ param ^ space_) params ""
       ^ eq_
       ^ emit_expr expr)
  | DExtern (name, params, ocaml_code) ->
    let params = List.rev params in
    let_
    ^ name
    ^ space_
    ^ List.fold_right (fun param acc -> acc ^ param ^ space_) params ""
    ^ eq_
    ^ ocaml_code
;;

let emit_program name program =
  let program =
    let program = List.rev program in
    List.fold_right (fun decl acc -> acc ^ emit_decl decl) program ""
  in
  let write_file filename content =
    let oc = open_out filename in
    try
      output_string oc content;
      flush oc;
      close_out oc
    with
    | e ->
      close_out_noerr oc;
      raise e
  in
  write_file name program
;;
