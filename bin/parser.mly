%{
open Ast
%}

%token <int> INT
%token <string> IDENT 
%token LPAREN RPAREN SEMI COMMA LBRACE RBRACE
%token EQ PLUS MINUS STAR SLASH
%token LET FUN REC IF ELSE
%token PRINTINT
%token EOF

%left PLUS MINUS
%left STAR SLASH

%start <program> program
%%

program:
  | decls EOF { $1 }
;

decls:
  | { [] }
  | decl decls { $1 :: $2 }

decl:
  | FUN IDENT LPAREN params RPAREN LBRACE expr RBRACE { DFun($2, $4, $7) }
;

expr:
  | PRINTINT LPAREN expr RPAREN { PrintInt($3) }
  | LPAREN expr RPAREN { Parens($2) }
  | INT { Int($1) }
  | IDENT { Var($1) }
  | LET IDENT EQ expr SEMI expr { Let($2, $4, $6) }
  | FUN IDENT LPAREN params RPAREN LBRACE expr RBRACE expr { Fun($2, $4, $7, $9) }
  | FUN REC IDENT LPAREN params RPAREN LBRACE expr RBRACE expr { FunRec($3, $5, $8, $10) }
  | IDENT LPAREN args RPAREN { Call($1, $3) }
  | IF expr LBRACE expr RBRACE ELSE LBRACE expr RBRACE { If($2, $4, $8) }
  | expr EQ expr { Equal($1, $3) }
  | expr PLUS expr { Add($1, $3) }
  | expr MINUS expr { Sub($1, $3) }
  | expr STAR expr { Mul($1, $3) }
  | expr SLASH expr { Div($1, $3) }
;

args:
  | { [] }
  | expr { [$1] }
  | expr COMMA args { $1 :: $3 }
;

params:
  | { [] }
  | IDENT { [$1] }
  | IDENT COMMA params { $1 :: $3 }
;
 