let () =
  let lexbuf = Lexing.from_channel stdin in
  try
    let program = Parser.program Lexer.token lexbuf in
    Emit.emit_program "test.ml" program
  with
  | Lexer.Lex_error msg ->
    prerr_endline ("Lex error: " ^ msg);
    exit 1
  | Parser.Error ->
    let pos = lexbuf.Lexing.lex_curr_p in
    prerr_endline
      (Printf.sprintf
         "Parse error at line %d, column %d"
         pos.Lexing.pos_lnum
         (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1));
    exit 1
;;
