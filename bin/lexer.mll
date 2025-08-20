{
open Parser
exception Lex_error of string
}

rule token = parse
  | "()" { UNIT }
  | "'" [^ '\\'] "'" { CHAR (Lexing.lexeme_char lexbuf 1) }
  | "'" "\\n" "'" { CHAR '\n' }
  | "'" "\\t" "'" { CHAR '\t' }
  | "'" "\\r" "'" { CHAR '\r' }
  | "'" "\\b" "'" { CHAR '\b' }
  | "'" "\\'" "'" { CHAR '\'' }
  | "'" "\\\\" "'" { CHAR '\\' }
  | ['0'-'9']+ as n { INT (int_of_string n) }
  | ['0'-'9']+ ['.'] ['0'-'9']+ as n { FLOAT (float_of_string n) }
  | "let" { LET }
  | "fun" { FUN }
  | ['A'-'Z' 'a'-'z' '_' ] ['A'-'Z' 'a'-'z' '0'-'9' '_' ]* as id { IDENT id }
  | '=' { EQ }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | ',' { COMMA }
  | ';' { SEMI }
  | [' ' '\t' '\r' '\n'] { token lexbuf }
  | eof { EOF }
  | _ as c { raise (Lex_error (Printf.sprintf "Unexpected char: %c" c)) }
