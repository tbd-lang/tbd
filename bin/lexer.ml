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

    ; "let a = { let x = 31; let y = 13; x + y};";


    "
    open core.io  # this is what bring Io module into scope


module Status = {
    enum Status = { success, failure of int }

    fun success () = Status.success;
    fun failure code = Status.failure code;

    fun new code = match code {
        0 -> Status.success,  # can write as `0 | 42 -> ...`
        42 -> Status.success,
        _ -> Status.failure code,
    }

    fun code status = match status {
        Status.success -> 0,
        Status.failure n -> m,  # trailing comma allowed and tells formatter to multi line
    };

    fun print status = match status {
        Status.success -> Io.print_line \"status: success\",
        Status.failure n -> Io.print_line \"status: failure\",
    };
}

module Position = {
    struct Position = { x : float, y : float, z : float }

    # shorthand assignment
    fun new x y z = Position { x, y, z };

    fun print position = {
        Io.print_line \"x: {position.x}\";
        Io.print_line \"y: {position.y}\";
        Io.print_line \"z: {position.z}\";
    }
}


module List = {  # this will be builtin - this is an example
    fun reverse l = {
        fun rec aux l' acc = match l' {
            [] -> acc,
            hd :: tl -> aux tl (hd :: acc),
        };
        aux l []  
        
        # no return keyword, blocks evaluate to the value 
        # of the last expr, if ; is used then they eval to unit `()`
    }
    
    fun rec fold f acc l = match l {
        [] -> reverse acc,
        hd :: tl -> fold f (f acc hd) tl,
    };
}


# constants must be primitives
const PI = 3.1415926535897932384626;

# recursive types area allowed
enum 'var LinkedList = {
    empty,
    node of 'var * 'var LinkedList,
}

fun main () = {
    let p = Position.new 1 2 3;
    let s = Status.success ();

    Position.print p;
    Status.print s;
}"
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