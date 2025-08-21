type ident = string

type expr =
  | Int of int
  | Var of ident
  | Let of ident * expr * expr
  | Fun of ident * ident list * expr * expr
  | Call of ident * expr list
  | Parens of expr
  | If of expr * expr * expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Equal of expr * expr
  | PrintInt of expr

type decl = DFun of ident * ident list * expr
type program = decl list
