{
open Parser
exception Lex_error of string
}

rule token = parse
  | '\'' (_ as c) '\'' { CHAR c }
  | '\'' ('\\' ['n' 't' 'r' '\\' '\'']) '\'' as c {
    let c =
      match c with
      | "'\\n'" -> '\n'
      | "'\\t'" -> '\t'
      | "'\\r'" -> '\r'
      | "'\\''" -> '\''
      | "'\\\\'" -> '\\'
      | _ -> failwith ("Unknown escape: " ^ c)
    in
    CHAR c
  }
  | ['0'-'9']+ as n { INT (int_of_string n) }
  | ['0'-'9']+ '.' ['0'-'9']+ as n { FLOAT (float_of_string n) }
  | "let" { LET }
  | "fun" { FUN }
  | "rec" { REC }
  | "if" { IF }
  | "else" { ELSE }
  | "print_int" { PRINTINT }
  | ['A'-'Z' 'a'-'z' '_' ] ['A'-'Z' 'a'-'z' '0'-'9' '_' ]* as id { IDENT id }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { STAR }
  | '/' { SLASH }
  | '=' { EQ }
  | "+." { PLUS_DOT }
  | "-." { MINUS_DOT }
  | "*." { STAR_DOT }
  | "/." { SLASH_DOT }
  | '>' { GT }
  | '<' { LT }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | ';' { SEMI }
  | ',' { COMMA }
  | [' ' '\t' '\r' '\n'] { token lexbuf }
  | eof { EOF }
  | _ as c { raise (Lex_error (Printf.sprintf "Unexpected char: %c" c)) }
 