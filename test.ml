module IO = struct
  let print_int x = print_endline (string_of_int x)
end

module List = struct
  let length l = List.length l
  let hd l = List.hd l
  let tl l = List.tl l

  let reverse l =
    let rec inner l acc =
      if List.length l = 0
      then acc
      else (
        let hd = List.hd l in
        let tl = List.tl l in
        inner tl (hd :: acc))
    in
    inner l []
  ;;

  let map f l =
    let rec inner l acc =
      if List.length l = 0
      then reverse acc
      else (
        let hd = List.hd l in
        let tl = List.tl l in
        inner tl (f hd :: acc))
    in
    inner l []
  ;;

  let rec iter f l =
    if List.length l = 0
    then ()
    else (
      let hd = List.hd l in
      let tl = List.tl l in
      f hd;
      iter f tl)
  ;;
end

type u = unit
type b = bool
type c = char
type i = int
type f = float
type s = string
type t = another

let () = ()
