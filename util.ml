
let (@^) = List.rev_append

let option_map f opt =
  match opt with
  | None -> None
  | Some x -> Some (f x)


let seq start len =
  let rec aux accum len' =
    if len' = 0 then
      accum
    else
      aux (start+len'-1 :: accum) (len'-1)
  in
  aux [] len

let seq_but start len i =
  let rec aux accum len' =
    if len' = 0 then
      accum
    else
      aux ((if i = start + len' -1 then -1 else start+len'-1) :: accum) (len'-1)
  in
  aux [] len

let modify n x xs =
  let rec aux i pre = function
    | [] -> pre
    | y::ys -> if i = 0 then pre @^ x :: ys else aux (i-1) (y::pre) ys
  in
  aux n [] xs

