type ident = string

type expr =
  | EUnit
  | EInt of int
  | EFloat of float
  | EBlock of decl list * expr

and decl =
  | DFun of ident * ident list * expr
  | DLet of ident * expr

type program = decl list

let quote s = "\"" ^ s ^ "\""

let string_of_id_list ids =
  match ids with
  | [] -> "[]"
  | _ -> "[" ^ String.concat ", " (List.map quote ids) ^ "]"
;;

let string_of_string_list l =
  let l = List.rev l in
  let rec aux l acc =
    match l with
    | [] -> "[" ^ acc
    | [ hd ] -> aux [] (hd ^ acc)
    | hd :: tl -> aux tl (", " ^ hd ^ acc)
  in
  aux l "]"
;;

let rec string_of_expr = function
  | EUnit -> "EUnit"
  | EInt n -> "EInt(" ^ Int.to_string n ^ ")"
  | EFloat n -> "EFloat(" ^ Float.to_string n ^ ")"
  | EBlock (decls, expr) ->
    Printf.sprintf
      "Block(%s, %s)"
      (string_of_string_list (List.map string_of_decl decls))
      (string_of_expr expr)

and string_of_decl = function
  | DFun (name, params, expr) ->
    Printf.sprintf
      "Fun(%s, %s, %s)"
      (quote name)
      (string_of_id_list params)
      (string_of_expr expr)
  | DLet (name, expr) -> "ELet(" ^ quote name ^ ", " ^ string_of_expr expr ^ ")"
;;

let print_program decls =
  print_endline (string_of_string_list (List.map string_of_decl decls))
;;
