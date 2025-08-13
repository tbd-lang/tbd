
module Int = struct
  let rec to_string x = string_of_int x
end

module String = struct
  let rec to_int s = int_of_string s
end

module List = struct
  type 'a list =
    | Empty
    | Node of 'a * 'a list

  let rec reverse l =
    let rec aux l acc =
      match l with
      | Empty -> acc
      | Node (hd, tl) -> aux tl (Node (hd, acc))
    in
    aux l Empty

  let rec make f n =
    let rec aux n' acc =
      match n' with
      | 0 -> acc
      | _ -> 
        let elem = f n' in
        aux (n' - 1) (Node (elem, acc))
    in
    aux n Empty

  let rec fold f acc l =
    match l with
    | Empty -> acc
    | Node (hd, tl) -> fold f (f acc hd) tl

  let rec print f l =
    let rec aux l acc =
      match l with
      | Empty -> acc ^ "]"
      | Node (hd, Empty) -> aux Empty (acc ^ (f hd))
      | Node (hd, tl) -> aux tl (acc ^ (f hd) ^ ",")
    in 
    print_endline (aux l "[")

  let rec map f l =
    let rec aux l acc =
      match l with
      | Empty -> reverse acc
      | Node (hd, tl) -> 
        let next = f hd in
        aux tl (Node (next, acc))
    in
    aux l Empty

  let rec iter f l =
    match l with
    | Empty -> ()
    | Node (hd, tl) -> f hd; iter f tl

  let rec iteri f l =
    match l with
    | Empty -> ()
    | Node (hd, tl) -> f hd; iter f tl
end

module Io = struct
  let rec print = Printf.printf
end


let () =
  let a = [|1; 2; 3; 4|] in
  Io.print "%d\n" a.(0);
  a.(0) <- 42;
  Io.print "%d\n" a.(0);

  let x = Array.make 50 0 in
  Array.iter (fun x -> Io.print "%d, " x) x;

  let l = List.make (fun i -> i) 10 in
  List.print (fun x -> Int.to_string x) l;

  let r = List.reverse l in
  List.print (fun x -> Int.to_string x) r;

  let x = List.fold (fun acc x -> acc + (x * 2)) 0 l in
  Io.print "%d\n" x;

  (* List.iter (fun x -> print_endline (Int.to_string x)) l; *)