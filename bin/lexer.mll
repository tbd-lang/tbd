{
open Parser
exception Lex_error of string
}

rule token = parse
  | [' ' '\t' '\r' '\n'] { token lexbuf }
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
  | "extern" { EXTERN }
  | "if" { IF }
  | "else" { ELSE }
  | '"' { string "" lexbuf }
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
  | "print_int" { PRINTINT }
  | eof { EOF }
  | _ as c { raise (Lex_error (Printf.sprintf "Unexpected char: %c" c)) }

and string acc = parse
  | "\\n"   { string (acc ^ "\n") lexbuf }
  | "\\t"   { string (acc ^ "\t") lexbuf }
  | "\\r"   { string (acc ^ "\r") lexbuf }
  | "\\\""  { string (acc ^ "\"") lexbuf }
  | "\\\\"  { string (acc ^ "\\") lexbuf }
  | [^ '"' '\\']+ as chunk { string (acc ^ chunk) lexbuf }
  | '"' { STRING acc }
  | eof { failwith "Unterminated string literal" }