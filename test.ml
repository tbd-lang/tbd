let rec iter f l =
  match l with
  | [] -> ()
  | hd :: tl ->
    f hd;
    iter f tl
;;

let reverse l =
  let rec inner l acc =
    match l with
    | [] -> acc
    | hd :: tl -> inner tl (hd :: acc)
  in
  inner l []
;;

let map f l =
  let rec inner l acc =
    match l with
    | [] -> reverse acc
    | h :: t -> inner t (f h :: acc)
  in
  inner l []
;;

let first_two l =
  match l with
  | [] -> None
  | [ _ ] -> None
  | [ _; _ ] -> None
  | one :: two :: _ -> Some (one, two)
;;

let () =
  let l = [ "I"; "love"; "you" ] in
  let x = first_two l in
  let l = map (fun s -> s ^ "!") l in
  iter (fun s -> print_endline s) l
;;
