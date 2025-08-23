type ident = string

type expr =
  | Unit
  | Char of char
  | Int of int
  | Float of float
  | String of string
  | Tuple of expr list
  | List of expr list
  | Array of expr list
  | Var of ident
  | Let of ident * expr * expr
  | Fun of ident * ident list * expr * expr
  | FunRec of (ident * ident list * expr) list * expr
  | Call of expr * expr list
  | Seq of expr * expr
  | Parens of expr
  | If of expr * expr * expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | FAdd of expr * expr
  | FSub of expr * expr
  | FMul of expr * expr
  | FDiv of expr * expr
  | Concat of expr * expr
  | Equal of expr * expr
  | NotEqual of expr * expr
  | Gt of expr * expr
  | Gte of expr * expr
  | Lt of expr * expr
  | Lte of expr * expr

type decl =
  | DFun of ident * ident list * expr
  | DFunRec of (ident * ident list * expr) list
  | DExtern of ident * ident list * string
  | DModule of ident * decl list

type program = decl list
