let test s s = s

let () =
  let hello = "Hello " in
  let world = "World!" in
  let t = test (hello ^ world) (hello ^ world) in
  print_endline t
;;
