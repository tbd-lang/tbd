open Tbd
open Tbd.Parser

(*
let a = 1;
*)
(* let () =
  let _ = [
    Lexer.Let;
    Lexer.Identifier("a");
    Lexer.Assignment;
    Lexer.Integer(1);
    Lexer.Semicolon
  ] in
  let ast = Block [Let ("a", Integer(1))] in
  print_ast ast *)

;;

(* 
let a = {
  let x = 13;
  let y = 31;
  x + y
};

Let
Identifier(a)
Assignment
LeftBrace
Let
Identifier(x)
Assignment
Integer(31)
Semicolon
Let
Identifier(y)
Assignment
Integer(13)
Semicolon
Identifier(x)
AddInt
Identifier(y)
RightBrace
Semicolon
*)
let _ =
  let text =
    "let a = {
      let x = 13;
      let y = 31;
      x + y
    };"
  in
  let tokens = Lexer.tokenize text in
  Lexer.print_tokens tokens;
  let ast = gen_ast tokens in 
  print_ast ast
;;


(* (* 
let a = {
  let x = 13.1;
  let y = 31.2;
  x +. y
};
*)
let _ =
  let ast  = Block [
    Let ("a", Block [
      Let ("x", Float(13.1));
      Let ("y", Float(31.2));
      BinOp ("+.", Identifier("x"), Identifier("y"))
    ])
  ] in print_ast ast
;;


(*
fun add a b = a + b;
*)
let _ =
  let ast = Block [
    Function ("add", ["a"; "b"], BinOp ("+", Identifier("a"), Identifier("b")))
  ] in print_ast ast
;;


(*
fun norm x = x / (Int.abs x);
*)
let _ =
  let ast = Block [
    Function ("norm", ["x"], 
      BinOp ("/", Identifier("x"), Call(Identifier("Int.abs"), [Identifier("x")])
    ))
  ] in print_ast ast
;; *)