open Tbd.Lexer

let test_cases =
  [ "import path.to.module"
  ; "import path.to.module.{qualified}"
  ; "import path.to.module.{q1, q2}"
  ; "const I = 23"
  ; "const F = 12.34"
  ; "const B = true"
  ; "module name {}"
  ; "fun greet1(p) {}"
  ; "fun greet2() {}"
  ; "fun greet3(a, b, c) {}"
  ; "fun rec fold(list) {}"
  ]
;;

let () =
  let run_test text =
    let tokens = tokenize text in
    print_endline ("TEST: " ^ text);
    print_tokens tokens;
    print_newline ()
  in
  List.iter (fun t -> run_test t) test_cases
;;
