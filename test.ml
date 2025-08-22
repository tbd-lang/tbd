let fib n =
  let rec inner i a b = if i = 0 then a else inner (i - 1) b (a + b) in
  inner n 0 1
;;

let add a = a + 1234
let sub a = a - 1234
let div a = a / 1234
let mul a = a * 1234
let fadd a = a +. 12.34
let fsub a = a -. 12.34
let fdiv a = a /. 12.34
let fmul a = a *. 12.34
let eq a = a = 1234
let neq a = a <> 1234
let gt a = a > 1234
let gte a = a >= 1234
let lt a = a < 1234
let lte a = a <= 1234
let eq a = a = 12.34
let neq a = a <> 12.34
let gt a = a > 12.34
let gte a = a >= 12.34
let lt a = a < 12.34
let lte a = a <= 12.34

let () =
  let c = 'c' in
  let b = 'x' in
  let x = 'a' in
  let b = ' ' in
  let c = '\n' in
  let str = "This is my kitchen\n" in
  let b = print_int 34 in
  let x = 0 - 1 in
  let c = 3 - 2 in
  let y = 0. -. 12.34 in
  let a = () in
  let x = fib 5 in
  print_int x
;;
