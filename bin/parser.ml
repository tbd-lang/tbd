open Tbd

let tests =
  [ "import path.to.module"
  ; "import path.to.module"
  ; "import path.to.module.Enum"
  ; "import path.to.module.Enum.Field"
  ; "const I = 23"
  ; "const F = 12.34"
  ; "const B = true"
  ; "fun greet1(p) {}"
  ; "fun greet2() {}"
  ; "fun greet3(a, b, c) {}"
  ; "fun fold(list) {}"
  ; "fun fold() { () }"
  ; "fun fold() { 1 }"
  ; "fun fold() { 1.2 }"
  ; "fun fold(list) { true }"
  ]
;;

let _ =
  let run_test test =
    print_endline ("TEST: " ^ test);
    let tokens = Lexer.tokenize test in
    Lexer.print_tokens tokens;
    let ast = Parser.parse_stmt tokens [] in
    match List.length ast with
    | 0 ->
      print_endline "No ast";
      print_newline ()
    | _ ->
      Parser.print_ast ast;
      print_newline ()
  in
  List.iter run_test tests
;;
