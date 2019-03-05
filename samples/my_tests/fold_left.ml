let rec fold_left f acc l = match l with
  | [] -> acc
  | h::t -> fold_left f (f acc h) t in

let l = [] in
let l = 1::l in
let l = 2::l in
let l = 3::l in
let l = 4::l in

fold_left (fun a x -> a + x) 0 l
