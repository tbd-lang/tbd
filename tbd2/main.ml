
module Math = struct
  let rec add a b = a + b
end

let () =
  let res = Math.add 1 2 in
  print_endline (string_of_int res)