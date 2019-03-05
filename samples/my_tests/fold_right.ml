let rec fold_right f acc l = match l with
  | [] -> acc
  | h::t -> f h (fold_right f acc t) in

let l = [] in
let l = 1::l in
let l = 2::l in
let l = 3::l in
let l = 4::l in

fold_right (fun a x -> a + x) 0 l
