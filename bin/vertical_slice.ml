open Tbd

let tokens =
  let text = Utils.read_file "examples/vertical_slice.tbd" in
  let tokens = Lexer.tokenize text in
  Lexer.print_tokens tokens;
  tokens
;;

let ast =
  let ast = Parser.gen_ast tokens in
  Parser.print_ast ast;
  ast
;;

open Parser (* so we can use Integer, String, Var, Call, Let directly *)

type value =
  | VInt of int
  | VString of string
  | VFunc of (value -> value)

type env = (string * value) list

let rec eval_expr env = function
  | Integer n -> VInt n
  | String s -> VString s
  | Var name ->
    (match List.assoc_opt name env with
     | Some v -> v
     | None -> failwith ("Unbound variable: " ^ name))
  | Call (func_expr, arg_expr) ->
    let func_val = eval_expr env func_expr in
    let arg_val = eval_expr env arg_expr in
    (match func_val with
     | VFunc f -> f arg_val
     | _ -> failwith "Trying to call a non-function value")
;;

let exec_stmt env = function
  | Let (name, expr) ->
    let value = eval_expr env expr in
    (name, value) :: env
;;

let rec exec_program env = function
  | [] -> env
  | stmt :: rest ->
    let env' = exec_stmt env stmt in
    exec_program env' rest
;;

(* Built-in functions, in curried style *)
let builtins : env =
  [ ( "add_int"
    , VFunc
        (fun a ->
          match a with
          | VInt x ->
            VFunc
              (fun b ->
                match b with
                | VInt y -> VInt (x + y)
                | _ -> failwith "add_int expects int as second arg")
          | _ -> failwith "add_int expects int as first arg") )
  ; ( "concat_str"
    , VFunc
        (fun a ->
          match a with
          | VString x ->
            VFunc
              (fun b ->
                match b with
                | VString y -> VString (x ^ y)
                | _ -> failwith "concat_str expects string as second arg")
          | _ -> failwith "concat_str expects string as first arg") )
  ]
;;

(* Pretty-print values *)
let string_of_value = function
  | VInt n -> string_of_int n
  | VString s -> s
  | VFunc _ -> "<function>"
;;

(* Example run *)
let () =
  let final_env = exec_program builtins ast in
  List.rev final_env
  |> List.iter (fun (name, v) -> Printf.printf "%s = %s\n" name (string_of_value v))
;;
