type ident = string

type expr =
  | EUnit
  | EChar of char
  | EInt of int
  | EFloat of float
  | EVar of ident
  | ECall of expr * expr list
  | EBlock of stmt list * expr

and stmt =
  | SExpr of expr
  | SLet of ident * expr
  | SFun of ident * ident list * expr

and decl = DFun of ident * ident list * expr

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
