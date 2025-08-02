open Tbd

let tests =
  [ "import path.to.module"
  ; "import path.to.module.Enum"
  ; "import path.to.module.Enum.Field"
  ; "const PI = 3.14159265357932384626"
  ; "const call = add(a, b)"
  ; "const F = 12.34"
  ; "const B = true"
  ; "fun greet1(p) {}"
  ; "fun greet2() {}"
  ; "fun greet3(a, b, c) {}"
  ; "fun fold(list) {}"
  ; "fun fold() { () }"
  ; "fun fold() { 1 }"
  ; "fun fold() { 1.2 35 }"
  ; "fun fold(list) { true }"
  ; "fun io.print_line(s) { () }"
  ; "fun print(s) { io.print_line(s) }"
  ; "fun norm(x) { let abs = int.abs(x) int.div(x, abs) }"
  ; "fun anon() { fun(a, b) { () } }"
  ; "fun anon() { let x = fun(a, b) { () } x(1, 2) }"
  ; "enum Result<T, E> { Ok(T), Error(E) }"
  ; "enum Option<T> { Some(T), None }"
  ; "enum Status { Success(int), Failure }"
  ; "enum Status<T> { Success(T), Failure(string) }"
  ; "fun error() { let e = Result.Error(\"hellooo\") Result.Ok(12) }"
  ; "struct Person { name: string, age: int }"
  ; "struct Person<T> { name: string, age: T }"
  ; "fun list() { let l = [1, 2, 3] l }"
  ; "fun list() { let l = [1.0, 2.0, 3.0] l }"
  ; "fun list() { let l = ['a', 'b', 'c'] l }"
  ; "fun list() { let l = [\"a\", \"b\", \"c\"] l }"
  ; "fun list() { let l = [] l }"
  ; "fun list() { [] }"
  ; "fun list() { let l = [[], [], []] l }"
  ]
;;

let _ =
  let run_test test =
    print_endline ("TEST: " ^ test);
    let tokens = Lexer.tokenize test in
    Lexer.print_tokens tokens;
    let ast = Parser.gen_ast tokens [] in
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
