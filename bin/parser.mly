%{
open Ast
%}

%token <int> INT
%token <float> FLOAT
%token <string> IDENT 
%token LPAREN RPAREN SEMI COMMA LBRACE RBRACE
%token PLUS MINUS STAR SLASH EQ GT LT
%token PLUS_DOT MINUS_DOT STAR_DOT SLASH_DOT EQ_DOT
%token LET FUN REC IF ELSE
%token PRINTINT
%token EOF

%left PLUS MINUS STAR SLASH
%left PLUS_DOT MINUS_DOT STAR_DOT SLASH_DOT

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
  | FLOAT { Float($1) }
  | IDENT { Var($1) }
  | LET IDENT EQ expr SEMI expr { Let($2, $4, $6) }
  | FUN IDENT LPAREN params RPAREN LBRACE expr RBRACE expr { Fun($2, $4, $7, $9) }
  | FUN REC IDENT LPAREN params RPAREN LBRACE expr RBRACE expr { FunRec($3, $5, $8, $10) }
  | IDENT LPAREN args RPAREN { Call($1, $3) }
  | IF expr LBRACE expr RBRACE ELSE LBRACE expr RBRACE { If($2, $4, $8) }
  | expr PLUS expr { Add($1, $3) }
  | expr MINUS expr { Sub($1, $3) }
  | expr STAR expr { Mul($1, $3) }
  | expr SLASH expr { Div($1, $3) }
  | expr PLUS_DOT expr { FAdd($1, $3) }
  | expr MINUS_DOT expr { FSub($1, $3) }
  | expr STAR_DOT expr { FMul($1, $3) }
  | expr SLASH_DOT expr { FDiv($1, $3) }
  | expr EQ expr { Equal($1, $3) }
  | expr LT GT expr { NotEqual($1, $4) }
  | expr GT expr { Gt($1, $3) }
  | expr GT EQ expr { Gte($1, $4) }
  | expr LT expr { Lt($1, $3) }
  | expr LT EQ expr { Lte($1, $4) }
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
 