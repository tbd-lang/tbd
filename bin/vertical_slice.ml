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

open Parser (* AST: expr and stmt *)

(* runtime values *)
type value =
  | VUnit
  | VBool of bool
  | VInt of int
  | VString of string
  | VFunc of string * (value -> value)

type env = (string * value) list

let _string_of_value v =
  match v with
  | VInt i -> string_of_int i
  | VString s -> "\"" ^ s ^ "\""
  | VBool b -> string_of_bool b
  | VUnit -> "()"
  | VFunc _ -> "<function>"
;;

let _print_env env =
  print_endline "=== ENVIRONMENT ===";
  List.iter (fun (name, v) -> Printf.printf "%s = %s\n" name (_string_of_value v)) env;
  print_endline "==================="
;;

(* evaluate expressions *)
let rec eval_expr env expr =
  match expr with
  | Unit -> VUnit
  | Boolean b -> VBool b
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
     | VFunc (_, f) -> f arg_val
     | _ -> failwith "Trying to call a non-function value")
  | Fun (param, body) ->
    (* closure: capture env, bind param at call time *)
    VFunc
      ( "<fun>"
      , fun arg ->
          let env' = (param, arg) :: env in
          eval_expr env' body )
  | Block (stmts, expr) ->
    let rec exec_block env stmts =
      match stmts with
      | [] -> env
      | stmt :: rest -> exec_block (exec_stmt env stmt) rest
    in
    let local_env = exec_block env stmts in
    eval_expr local_env expr
  | IfThenElse (cond, t_branch, f_branch) ->
    (match eval_expr env cond with
     | VBool true -> eval_expr env t_branch
     | VBool false -> eval_expr env f_branch
     | _ -> failwith "Condition must be a boolean")

(* execute statements *)
and exec_stmt env stmt =
  match stmt with
  | Let (name, expr) ->
    let value = eval_expr env expr in
    (name, value) :: env
  | LetRec (name, expr) ->
    (match expr with
     | Fun (param, body) ->
       (* recursive closure *)
       let rec f arg =
         let env' = (param, arg) :: (name, VFunc (name, f)) :: env in
         eval_expr env' body
       in
       (name, VFunc (name, f)) :: env
     | _ -> failwith "let rec must bind a function")
;;

let rec exec_program env = function
  | [] -> env
  | stmt :: rest ->
    let env' = exec_stmt env stmt in
    exec_program env' rest
;;

(* pretty-print values *)
let _string_of_value = function
  | VUnit -> "()"
  | VBool b -> string_of_bool b
  | VInt n -> string_of_int n
  | VString s -> s
  | VFunc (label, _) -> label
;;

(* built-in functions *)
let builtins : env =
  [ ( "@add_int"
    , VFunc
        ( "int -> int -> int"
        , fun a ->
            match a with
            | VInt x ->
              VFunc
                ( "int -> int"
                , fun b ->
                    match b with
                    | VInt y -> VInt (x + y)
                    | _ -> failwith "add_int expects int" )
            | _ -> failwith "add_int expects int" ) )
  ; ( "@sub_int"
    , VFunc
        ( "int -> int -> int"
        , fun a ->
            match a with
            | VInt x ->
              VFunc
                ( "int -> int"
                , fun b ->
                    match b with
                    | VInt y -> VInt (x - y)
                    | _ -> failwith "@sub_int expects int" )
            | _ -> failwith "@sub_int expects int" ) )
  ; ( "@mul_int"
    , VFunc
        ( "int -> int -> int"
        , fun a ->
            match a with
            | VInt x ->
              VFunc
                ( "int -> int"
                , fun b ->
                    match b with
                    | VInt y -> VInt (x * y)
                    | _ -> failwith "@mul_int expects int" )
            | _ -> failwith "@mul_int expects int" ) )
  ; ( "@div_int"
    , VFunc
        ( "int -> int -> int"
        , fun a ->
            match a with
            | VInt x ->
              VFunc
                ( "int -> int"
                , fun b ->
                    match b with
                    | VInt 0 -> failwith "@div_int: division by zero"
                    | VInt y -> VInt (x / y)
                    | _ -> failwith "@div_int expects int" )
            | _ -> failwith "@div_int expects int" ) )
  ; ( "@mod_int"
    , VFunc
        ( "int -> int -> int"
        , fun a ->
            match a with
            | VInt x ->
              VFunc
                ( "int -> int"
                , fun b ->
                    match b with
                    | VInt 0 -> failwith "@mod_int: division by zero"
                    | VInt y -> VInt (x mod y)
                    | _ -> failwith "@mod_int expects int" )
            | _ -> failwith "@mod_int expects int" ) )
  ; ( "@pow_int"
    , VFunc
        ( "int -> int -> int"
        , fun a ->
            match a with
            | VInt x ->
              VFunc
                ( "int -> int"
                , fun b ->
                    match b with
                    | VInt y ->
                      if y < 0
                      then failwith "@pow_int: negative exponent not supported"
                      else VInt (int_of_float (float_of_int x ** float_of_int y))
                    | _ -> failwith "@pow_int expects int" )
            | _ -> failwith "@pow_int expects int" ) )
  ; ( "@concat_str"
    , VFunc
        ( "string -> string -> string"
        , fun a ->
            match a with
            | VString x ->
              VFunc
                ( "string -> string"
                , fun b ->
                    match b with
                    | VString y -> VString (x ^ y)
                    | _ -> failwith "concat_str expects string" )
            | _ -> failwith "concat_str expects string" ) )
  ; ( "@int_to_str"
    , VFunc
        ( "int -> string"
        , fun a ->
            match a with
            | VInt x -> VString (string_of_int x)
            | _ -> failwith "int_to_str expects int" ) )
  ; ( "@str_to_int"
    , VFunc
        ( "string -> int"
        , fun a ->
            match a with
            | VString s -> VInt (int_of_string s)
            | _ -> failwith "str_to_int expects string" ) )
  ; ( "@int_equal"
    , VFunc
        ( "int -> int -> bool"
        , fun a ->
            match a with
            | VInt x ->
              VFunc
                ( "int -> bool"
                , fun b ->
                    match b with
                    | VInt y -> VBool (x = y)
                    | _ -> failwith "int_equal expects int" )
            | _ -> failwith "int_equal expects int" ) )
  ; ( "@int_compare"
    , VFunc
        ( "int -> int -> int"
        , fun a ->
            match a with
            | VInt x ->
              VFunc
                ( "int -> int"
                , fun b ->
                    match b with
                    | VInt y -> VInt (Int.compare x y)
                    | _ -> failwith "int_compare expects int" )
            | _ -> failwith "int_compare expects int" ) )
  ; ( "@read_line"
    , VFunc
        ( "string -> string"
        , fun a ->
            match a with
            | VString prompt ->
              print_string prompt;
              (* show prompt without newline *)
              flush stdout;
              (* force it to appear *)
              let input = read_line () in
              VString input
            | _ -> failwith "@read_line expects a string" ) )
  ; ( "@print_line"
    , VFunc
        ( "string -> unit"
        , fun a ->
            match a with
            | VString msg ->
              print_endline msg;
              VUnit
            | _ -> failwith "@print_line expects a string" ) )
  ]
;;

(* run the program produced by your parser (ast : stmt list) *)
(* let () =
  let final_env = exec_program builtins ast in
  List.rev final_env
  |> List.iter (fun (name, v) -> Printf.printf "%s = %s\n" name (string_of_value v))
;; *)

let () =
  let _ = exec_program builtins ast in
  ()
;;
