let fib n =
  let rec inner i a b = if i = 0 then a else inner (i - 1) b (a + b) in
  inner n 0 1
;;

let () =
  let x = fib 5 in
  print_endline (string_of_int x)
;;
