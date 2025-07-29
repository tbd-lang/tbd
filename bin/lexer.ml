open Tbd.Lexer
  
let test_cases =
  [
    "let int = { 1 };";
    "let int = { 12 };";
    "let int = { -12 };";

    "let float = { 1.0 };";
    "let float = { 12.34 };";
    "let float = { -12.34 };";

    "let add a b = { a + b };";
    "let sub a b = { a - b };";
    "let mul a b = { a * b };";
    "let div a b = { a / b };";

    "let add a b = { a +. b };";
    "let sub a b = { a -. b };";
    "let mul a b = { a *. b };";
    "let div a b = { a /. b };";

    "let concat s1 s2 = { \"Hello \" ^ \"world!\" };";

    "let string = { \"This is a string\" };";

    "let char = { 'c' };";
    "let char = { '\\n' };";

    "let bool = { true };";
    "let bool = { false };";

    "fun norm x = { Int.div x (Int.abs x) };";

    "enum Status = { success, failure of int };";

    "struct Person = { name : string, age : int };";

    "fun norm x = { match x { 0 -> 0, _ -> 1 }};";

    "let _not_wildcard = { 0 };";
    "let _ = { \"Wildcard\" };";

    "let () = { \"Unit\" }";

    "fun rec factorial n = { match n { 0 -> 1, _ -> Int.mul n (factorial (Int.sub n 1)) }};";

    "fun pop list = { match list { hd::_ -> Option.some hd, _ -> Option.none }};";

    "fun char_to_int c = { match c { '0'..'9' -> Option.some (Int.sub (Char.to_ascii c) (Char.to_ascii '0')), _ -> Option.none }}"
  
    ;"let a = 1;"

    ; "let a = { let x = 31; let y = 13; x + y};"
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