let print_int x = print_endline (string_of_int x)
let add a b = a + b
let get_add = add

let () =
  let x = get_add 1 2 in
  print_int x
;;
