let chars_to_string c = List.rev c |> List.to_seq |> String.of_seq
let string_to_chars s = String.to_seq s |> List.of_seq

let string_of_string_list l =
  let rec aux l acc =
    match l with
    | [] -> "[" ^ acc
    | [ hd ] -> aux [] (hd ^ acc)
    | hd :: tl -> aux tl (", " ^ hd ^ acc)
  in
  aux l "]"
;;

let string_of_string_tuple l =
  let rec aux l acc =
    match l with
    | [] -> "[" ^ acc
    | [ hd ] -> aux [] (hd ^ acc)
    | hd :: tl -> aux tl (", " ^ hd ^ acc)
  in
  aux l "]"
;;

let peek l =
  match l with
  | [] -> None
  | hd :: _ -> Some hd
;;

let advance l =
  match l with
  | [] -> None
  | hd :: tl -> Some (hd, tl)
;;

let read_file filename =
  let ic = open_in filename in
  try
    let n = in_channel_length ic in
    let s = really_input_string ic n in
    close_in ic;
    s
  with
  | e ->
    close_in_noerr ic;
    raise e
;;

let chars_to_int chars = List.rev chars |> List.to_seq |> String.of_seq |> int_of_string
