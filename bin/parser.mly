%{
open Ast
%}

%token TRUE FALSE
%token <char> CHAR
%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> IDENT
%token <string> UIDENT
%token LPAREN RPAREN SEMI COMMA LBRACE RBRACE LBRACKET RBRACKET PIPE ARROW UNDERSCORE COLON
%token PLUS MINUS STAR SLASH CARET CONS EQ NEQ GT GTE LT LTE BAND BOR DOT
%token PLUS_DOT MINUS_DOT STAR_DOT SLASH_DOT EQ_DOT
%token LET FUN REC AND EXTERN MODULE TYPE IF ELSE MATCH
%token <string> T
%token <string> TVAR
%token EOF

%left SEMI
%right LPAREN
%left RPAREN
%right BAND BOR
%left EQ NEQ GT GTE LT LTE
%left EQ_DOT
%left PLUS MINUS
%left STAR SLASH
%left PLUS_DOT MINUS_DOT
%left STAR_DOT SLASH_DOT
%right CARET
%right CONS

%start <program> program
%%

program:
  | decls EOF { $1 }
;

decls:
  | { [] }
  | decl decls { $1 :: $2 }
;

decl:
  | FUN IDENT LPAREN params RPAREN LBRACE expr RBRACE { DFun($2, $4, $7) }
  | FUN REC IDENT LPAREN params RPAREN LBRACE expr RBRACE and_funs { DFunRec(($3, $5, $8) :: $10) }
  | EXTERN IDENT LPAREN params RPAREN EQ STRING { DExtern($2, $4, $7) }
  | MODULE UIDENT LBRACE decls RBRACE { DModule($2, $4) }
  | TYPE IDENT EQ variants { DTypVariant($2, [], $4) }
  | TYPE IDENT LPAREN typvars RPAREN EQ variants { DTypVariant($2, $4, $7) }
  | TYPE IDENT EQ IDENT { DTypAlias($2, [], $4, [])}
  | TYPE IDENT EQ IDENT LPAREN typs RPAREN { DTypAlias($2, [], $4, $6)}
  | TYPE IDENT LPAREN typvars RPAREN EQ IDENT { DTypAlias($2, $4, $7, [])}
  | TYPE IDENT LPAREN typvars RPAREN EQ IDENT LPAREN typs RPAREN { DTypAlias($2, $4, $7, $9)}
  | TYPE IDENT EQ record_fields { DTypeRecord($2, [], $4) }
  | TYPE IDENT LPAREN typvars RPAREN EQ record_fields { DTypeRecord($2, $4, $7) }
;

typvars:
  | { [] }
  | TVAR { [$1] }
  | TVAR COMMA typvars { $1 :: $3 }
;

variants:
  | { [] }
  | variant { [$1] }
  | variant PIPE variants { $1 :: $3 }
  | PIPE variant PIPE variants { $2 :: $4 }
;

variant:
  | UIDENT { TConstr($1, None) }
  | UIDENT LPAREN typs RPAREN { TConstr($1, Some(TTuple($3))) }
;

typs:
  | { [] }
  | typ { [$1] }
  | typ COMMA typs { $1 :: $3 }
;

expr:
  | LPAREN expr RPAREN { Parens($2) }
  | LPAREN RPAREN { Unit }
  | TRUE { Bool(true) }
  | FALSE { Bool(false) }
  | CHAR { Char($1) }
  | INT { Int($1) }
  | MINUS INT { Parens(Sub(Int(0), Int($2))) }
  | FLOAT { Float($1) }
  | MINUS FLOAT { Parens(FSub(Float(0.0), Float($2))) }
  | STRING { String($1) }
  | LPAREN args RPAREN { Tuple($2) }
  | LBRACKET args RBRACKET { List($2) }
  | LBRACE args RBRACE { Array($2) }
  | IDENT { Var($1) }
  | UIDENT DOT IDENT { Var($1 ^ "." ^ $3) }
  | LET IDENT EQ expr SEMI expr { Let($2, $4, $6) }
  | LET IDENT EQ LBRACE expr RBRACE SEMI expr { Let($2, $5, $8) }
  | LET LPAREN RPAREN EQ expr SEMI expr { Let("()", $5, $7) }
  | FUN IDENT LPAREN params RPAREN LBRACE expr RBRACE expr { Fun($2, $4, $7, $9) }
  | FUN REC IDENT LPAREN params RPAREN LBRACE expr RBRACE and_funs expr { FunRec(($3, $5, $8) :: $10, $11) }
  | FUN LPAREN params RPAREN LBRACE expr RBRACE { Lambda($3, $6) }
  | expr SEMI expr { Seq($1, $3) }
  | expr LPAREN args RPAREN { Call($1, $3) }
  | UIDENT { Constr($1, []) }
  | UIDENT DOT UIDENT { Constr($1 ^ "." ^ $3, []) }
  | UIDENT LPAREN args RPAREN { Constr($1, $3) }
  | IF expr LBRACE expr RBRACE ELSE LBRACE expr RBRACE { If($2, $4, $8) }
  | MATCH expr LBRACE cases RBRACE { Match($2, $4) }
  | expr PLUS expr { Add($1, $3) }
  | expr MINUS expr { Sub($1, $3) }
  | expr STAR expr { Mul($1, $3) }
  | expr SLASH expr { Div($1, $3) }
  | expr PLUS_DOT expr { FAdd($1, $3) }
  | expr MINUS_DOT expr { FSub($1, $3) }
  | expr STAR_DOT expr { FMul($1, $3) }
  | expr SLASH_DOT expr { FDiv($1, $3) }
  | expr CARET expr { Concat($1, $3) }
  | expr CONS expr { Cons($1, $3) }
  | expr EQ expr { Equal($1, $3) }
  | expr NEQ expr { NotEqual($1, $3) }
  | expr GT expr { Gt($1, $3) }
  | expr GTE expr { Gte($1, $3) }
  | expr LT expr { Lt($1, $3) }
  | expr LTE expr { Lte($1, $3) }
  | expr BAND expr { And($1, $3) }
  | expr BOR expr { Or($1, $3) }
;

cases:
  | case { [$1] }
  | case COMMA cases { $1 :: $3 }
  | case COMMA { [$1] }
;

case:
  | pat ARROW expr { ($1, $3) }
  | pat ARROW LBRACE expr RBRACE { ($1, $4) }
  
;

pat:
  | TRUE { PBool(true) }
  | FALSE { PBool(false) }
  | CHAR { PChar($1) }
  | INT { PInt($1) }
  | FLOAT { PFloat($1) }
  | STRING { PString($1) }
  | IDENT { PVar($1) }
  | UNDERSCORE { PWildcard }
  | LPAREN pats RPAREN { PTuple($2) }
  | UIDENT { PConstr($1, []) }
  | pat CONS pat { PCons($1, $3) }
  | LBRACKET RBRACKET { PEmptyList }
  | LBRACKET pats RBRACKET { PList($2) }

;

pats:
  | pat { [$1] }
  | pat COMMA pats { $1 :: $3 }
;

pat_args:
  | pat                    { [$1] }
  | pat COMMA pat_args     { $1 :: $3 }
;

typ:
  | IDENT { TApp($1, []) }
  | IDENT LPAREN typs RPAREN { TApp($1, $3) }
  | TVAR { TVar($1) }
  | LPAREN typs RPAREN { TTuple($2) }
;

record_fields:
  | LBRACE field_list RBRACE   { $2 }
;

field_list:
  | IDENT COLON typ                   { [($1, $3)] }
  | IDENT COLON typ COMMA field_list  { ($1, $3) :: $5 }
;

and_funs:
  | { [] }
  | AND FUN IDENT LPAREN params RPAREN LBRACE expr RBRACE and_funs { ($3, $5, $8) :: $10 }
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