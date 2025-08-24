type ('a) list = | Empty| Node of ( 'a  *  'a  list)

let rec iter f l = match l with
| Empty -> ()
| Node (hd,tl) -> (f (hd)); (iter (f) (tl))

let () = let l = Node ("I", Node ("love", Node ("you!", Empty))) in
(iter ((fun s -> (print_endline (s)))
) (l))