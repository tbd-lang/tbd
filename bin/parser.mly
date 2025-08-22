%{
open Ast
%}

%token <char> CHAR
%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> IDENT 
%token LPAREN RPAREN SEMI COMMA LBRACE RBRACE
%token PLUS MINUS STAR SLASH EQ NEQ GT GTE LT LTE
%token PLUS_DOT MINUS_DOT STAR_DOT SLASH_DOT EQ_DOT
%token LET FUN REC EXTERN MODULE IF ELSE
%token PRINTINT
%token EOF

%left EQ NEQ GT GTE LT LTE
%left EQ_DOT
%left PLUS MINUS
%left STAR SLASH
%left PLUS_DOT MINUS_DOT
%left STAR_DOT SLASH_DOT

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
  | FUN REC IDENT LPAREN params RPAREN LBRACE expr RBRACE { DFunRec($3, $5, $8) }
  | EXTERN IDENT LPAREN params RPAREN EQ STRING { DExtern($2, $4, $7) }
  | MODULE IDENT LBRACE decls RBRACE { DModule($2, $4) }
;

expr:
  | LPAREN expr RPAREN { Parens($2) }
  | LPAREN RPAREN { Unit }
  | CHAR { Char($1) }
  | INT { Int($1) }
  | MINUS INT { Parens(Sub(Int(0), Int($2))) }
  | FLOAT { Float($1) }
  | MINUS FLOAT { Parens(FSub(Float(0.0), Float($2))) }
  | STRING { String($1) }
  | IDENT { Var($1) }
  | LET IDENT EQ expr SEMI expr { Let($2, $4, $6) }
  | LET IDENT EQ LBRACE expr RBRACE SEMI expr { Let($2, $5, $8) }
  | LET LPAREN RPAREN EQ expr SEMI expr { Let("()", $5, $7) }
  | FUN IDENT LPAREN params RPAREN LBRACE expr RBRACE expr { Fun($2, $4, $7, $9) }
  | FUN REC IDENT LPAREN params RPAREN LBRACE expr RBRACE expr { FunRec($3, $5, $8, $10) }
  | expr SEMI expr { Seq($1, $3) }
  | expr LPAREN args RPAREN { Call($1, $3) }
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
  | expr NEQ expr { NotEqual($1, $3) }
  | expr GT expr { Gt($1, $3) }
  | expr GTE expr { Gte($1, $3) }
  | expr LT expr { Lt($1, $3) }
  | expr LTE expr { Lte($1, $3) }
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
 