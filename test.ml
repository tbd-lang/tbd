let rec add a b =
  add 1 2;
  ()
;;

let () =
  let rec a = 1 in
  let rec b = a in
  let rec c = add 1 2 in
  add 3 4;
  ()
;;
