
type t = int

let (%) n d = if n<0 then d-1+(n+1) mod d else n mod d

let of_int p n = n % p

let to_int n = n

let add p n m = (n + m) % p

let sub p n m = (n - m) % p

let neg p n = p - n

let mul p n m = (n * m) % p

let pow p n m =
  let rec aux accum m =
    if m = 0 then
      accum
    else
      aux (mul p n accum) (m-1)
  in
  aux 1 m

let inv p n =
  pow p n (p-2)

let div p n m =
  mul p n (inv p m)
