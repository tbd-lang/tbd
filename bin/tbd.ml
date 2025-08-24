open Lexing

let version = "0.1.0"
let stdlib_source = ""

let () =
  if Array.length Sys.argv < 2
  then (
    prerr_endline "Usage: tbd <file.tbd> | version";
    exit 1);
  if Sys.argv.(1) = "version"
  then print_endline version
  else (
    let source_file = Sys.argv.(1) in
    let base_name = Filename.remove_extension (Filename.basename source_file) in
    let ml_file = base_name ^ ".ml" in
    let exe_file = base_name in
    let chan = open_in source_file in
    let user_source = really_input_string chan (in_channel_length chan) in
    close_in chan;
    let full_source = stdlib_source ^ "\n" ^ user_source in
    let lexbuf = from_string full_source in
    try
      let prog = Parser.program Lexer.token lexbuf in
      close_in chan;
      let oc = open_out ml_file in
      let fmt = Format.formatter_of_out_channel oc in
      Emit.pp_program fmt prog;
      Format.pp_print_flush fmt ();
      close_out oc;
      let cmd = Printf.sprintf "ocamlopt -o %s %s" exe_file ml_file in
      let exit_code = Sys.command cmd in
      if exit_code = 0
      then Printf.printf "Compiled successfully: %s (and %s)\n" exe_file ml_file
      else prerr_endline "Error: compilation failed."
    with
    | Parser.Error ->
      close_in_noerr chan;
      let pos = lexbuf.lex_curr_p in
      Printf.eprintf
        "Parse error at line %d, column %d\n"
        pos.pos_lnum
        (pos.pos_cnum - pos.pos_bol + 1);
      exit 1
    | e ->
      close_in_noerr chan;
      prerr_endline ("Unexpected error: " ^ Printexc.to_string e);
      exit 1)
;;
