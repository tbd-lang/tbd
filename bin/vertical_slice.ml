open Tbd

let tokens =
  let text = Utils.read_file "examples/vertical_slice.tbd" in
  let tokens = Lexer.tokenize text in
  (* Lexer.print_tokens tokens; *)
  tokens
;;

let ast =
  let ast = Parser.gen_ast tokens in
  (* Parser.print_ast ast; *)
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

(* evaluate expressions *)
let rec eval_expr env = function
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
  | Match (cond_expr, then_expr, else_expr) ->
    (match eval_expr env cond_expr with
     | VBool true -> eval_expr env then_expr
     | VBool false -> eval_expr env else_expr
     | _ -> failwith "match expects a boolean condition")
;;

(* execute statements *)
let exec_stmt env = function
  | Let (Some name, expr) ->
    let value = eval_expr env expr in
    (name, value) :: env
  | Let (None, expr) ->
    let _ = eval_expr env expr in
    env
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
            | _ -> failwith "@read_line expects a string" ) )
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
