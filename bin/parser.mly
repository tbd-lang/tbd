%{
open Ast
%}

%token <string> IDENT
%token <char> CHAR
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

decls:
  | { [] }
  | decl decls { $1 :: $2 }

decl:
  | FUN IDENT LPAREN params RPAREN expr { DFun($2, $4, $6) }
  | FUN IDENT LPAREN RPAREN expr { DFun($2, [], $5) }

params:
  | { [] }
  | IDENT { [$1] }
  | params COMMA IDENT { $1 @ [$3] }

expr:
  | call { $1 }

call:
  | call LPAREN args RPAREN { ECall($1, $3) }
  | primary { $1 }

primary:
  | LPAREN RPAREN { EUnit }
  | CHAR { EChar($1) }
  | INT { EInt($1) }
  | FLOAT { EFloat($1) }
  | IDENT { EVar($1) }
  | IDENT LPAREN args RPAREN { ECall(EVar($1), $3) }
  | LBRACE stmts final_expr RBRACE { EBlock($2, $3) }
  | LPAREN expr RPAREN { $2 }

stmts:
  | { [] }
  | stmt stmts { $1 :: $2 }

stmt:
  | LET IDENT EQ expr SEMI { SLet($2, $4) }
  | IDENT LPAREN args RPAREN SEMI { SExpr(ECall(EVar($1), $3)) }

args:
  | { [] }
  | expr { [$1] }
  | args COMMA expr { $1 @ [$3] }

final_expr:
  | { EUnit }
  | expr { $1 }