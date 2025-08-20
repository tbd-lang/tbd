%{
open Ast
%}

%token <string> IDENT
%token UNIT
%token <int> INT
%token <float> FLOAT
%token FUN LPAREN RPAREN COMMA
%token LET EQ SEMI
%token LBRACE RBRACE
%token EOF

%start <program> program
%%

program:
  | decls EOF { $1 }

decl:
  | LET IDENT EQ expr SEMI { DLet($2, $4) }
  | FUN IDENT LPAREN params RPAREN expr { DFun($2, $4, $6) }
  | FUN IDENT UNIT expr { DFun($2, [], $4) }

decls:
  | { [] }
  | decl decls { $1 :: $2 }

expr:
  | UNIT { EUnit }
  | INT { EInt($1) }
  | FLOAT { EFloat($1) }
  | LBRACE decls opt_expr RBRACE { EBlock($2, $3) }

opt_expr:
  | { EUnit }
  | expr { $1 }

params:
  | { [] }
  | IDENT { [$1] }
  | params COMMA IDENT { $1 @ [$3] }
