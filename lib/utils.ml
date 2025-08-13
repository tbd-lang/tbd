let advance l =
  match l with
  | [] -> failwith "list Empty"
  | _ :: tl -> tl
;;
