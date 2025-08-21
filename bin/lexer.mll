{
open Parser
exception Lex_error of string
}

rule token = parse
  | ['0'-'9']+ as n { INT (int_of_string n) }
  | "let" { LET }
  | "fun" { FUN }
  | "if" { IF }
  | "else" { ELSE }
  | "print_int" { PRINTINT }
  | ['A'-'Z' 'a'-'z' '_' ] ['A'-'Z' 'a'-'z' '0'-'9' '_' ]* as id { IDENT id }
  | '=' { EQ }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { STAR }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | ';' { SEMI }
  | ',' { COMMA }
  | [' ' '\t' '\r' '\n'] { token lexbuf }
  | eof { EOF }
  | _ as c { raise (Lex_error (Printf.sprintf "Unexpected char: %c" c)) }
 