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
  | VTuple of value list

type env = (string * value) list

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
  | Tuple exprs -> VTuple (List.map (eval_expr env) exprs)

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

(* built-in functions *)
let builtins : env =
  [ ( "Int_add"
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
                    | _ -> failwith "Int_add expects int" )
            | _ -> failwith "Int_add expects int" ) )
  ; ( "Int_sub"
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
                    | _ -> failwith "Int_sub expects int" )
            | _ -> failwith "Int_sub expects int" ) )
  ; ( "Int_mul"
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
                    | _ -> failwith "Int_mul expects int" )
            | _ -> failwith "Int_mul expects int" ) )
  ; ( "Int_div"
    , VFunc
        ( "int -> int -> int"
        , fun a ->
            match a with
            | VInt x ->
              VFunc
                ( "int -> int"
                , fun b ->
                    match b with
                    | VInt y -> VInt (x / y)
                    | _ -> failwith "Int_div expects int" )
            | _ -> failwith "Int_div expects int" ) )
  ; ( "Int_mod"
    , VFunc
        ( "int -> int -> int"
        , fun a ->
            match a with
            | VInt x ->
              VFunc
                ( "int -> int"
                , fun b ->
                    match b with
                    | VInt y -> VInt (x mod y)
                    | _ -> failwith "Int_mod expects int" )
            | _ -> failwith "Int_mod expects int" ) )
  ; ( "Int_equal"
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
                    | _ -> failwith "Int_equal expects int" )
            | _ -> failwith "Int_equal expects int" ) )
  ; ( "Int_compare"
    , VFunc
        ( "int -> int -> int"
        , fun a ->
            match a with
            | VInt x ->
              VFunc
                ( "int -> int"
                , fun b ->
                    match b with
                    | VInt y -> VInt (compare x y)
                    | _ -> failwith "Int_compare expects int" )
            | _ -> failwith "Int_compare expects int" ) )
    (* float operations *)
  ; ( "Float_add"
    , VFunc
        ( "float -> float -> float"
        , fun a ->
            match a with
            | VString sa ->
              (* assuming string holds float literal for now? *)
              let fa = float_of_string sa in
              VFunc
                ( "float -> float"
                , fun b ->
                    match b with
                    | VString sb -> VString (string_of_float (fa +. float_of_string sb))
                    | _ -> failwith "Float_add expects float" )
            | _ -> failwith "Float_add expects float" ) )
    (* TODO: Float_sub, Float_mul, Float_div, Float_equal, etc. *)

    (* tuples *)
  ; ( "Tuple_fst"
    , VFunc
        ( "('a * 'b) -> 'a"
        , fun a ->
            match a with
            | VTuple [ x; _ ] -> x
            | _ -> failwith "Tuple_fst expects a 2-tuple" ) )
  ; ( "Tuple_snd"
    , VFunc
        ( "('a * 'b) -> 'b"
        , fun a ->
            match a with
            | VTuple [ _; y ] -> y
            | _ -> failwith "Tuple_snd expects a 2-tuple" ) )
  ; ( "String_concat"
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
                    | _ -> failwith "String_concat expects string" )
            | _ -> failwith "String_concat expects string" ) )
  ; ( "String_compare"
    , VFunc
        ( "string -> string -> int"
        , fun a ->
            match a with
            | VString x ->
              VFunc
                ( "string -> int"
                , fun b ->
                    match b with
                    | VString y -> VInt (compare x y)
                    | _ -> failwith "String_compare expects string" )
            | _ -> failwith "String_compare expects string" ) )
  ; ( "Int_to_string"
    , VFunc
        ( "int -> string"
        , fun a ->
            match a with
            | VInt x -> VString (string_of_int x)
            | _ -> failwith "Int_to_string expects int" ) )
  ; ( "String_to_int"
    , VFunc
        ( "string -> int"
        , fun a ->
            match a with
            | VString s -> VInt (int_of_string s)
            | _ -> failwith "String_to_int expects string" ) )
  ; ( "Io_print_line"
    , VFunc
        ( "string -> unit"
        , fun a ->
            match a with
            | VString msg ->
              print_endline msg;
              VUnit
            | _ -> failwith "Io_print_line expects a string" ) )
  ; ( "Io_read_line"
    , VFunc
        ( "string -> string"
        , fun a ->
            match a with
            | VString prompt ->
              print_string prompt;
              flush stdout;
              (* ensure prompt shows *)
              let input = read_line () in
              VString input
            | _ -> failwith "Io_read_line expects a string" ) )
  ]
;;

let _ = exec_program builtins ast
