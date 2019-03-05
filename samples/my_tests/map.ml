let rec map f l = match l with
  | [] -> []
  | h::t -> (f h)::(map f t) in

let l = [] in
let l = 1::l in
let l = 2::l in
let l = 3::l in
let l = 4::l in

map (fun x -> 2 * x) l
