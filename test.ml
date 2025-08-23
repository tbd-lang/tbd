let () =
  let x = [| 1; 2; 3 + 4 |] in
  let l = [] in
  let b = 0 = 1 || (false && true) in
  let l = 'x' :: l in
  ()
;;
