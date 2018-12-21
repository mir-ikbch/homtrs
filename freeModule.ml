
module type Ring = sig
  type t
  val zero : t
  val add : t -> t -> t
  val sub : t -> t -> t
  val neg : t -> t
  val mul : t -> t -> t
end

module type Module = sig
  type coeff
  type 'a t
  val zero : 'a t
  val add : 'a t -> 'a t -> 'a t
  val sub : 'a t -> 'a t -> 'a t
  val neg : 'a t -> 'a t
  val scl : coeff -> 'a t -> 'a t
end

module Make (R:Ring) = struct
  type coeff = R.t
  type 'a t = (coeff * 'a) list

  let zero = []

  let add xs ys =
    let rec aux accum = function
      | [] -> accum
      | (k,b)::rest ->
          let rec f pre = function
            | [] -> (k,b)::pre 
            | (k',b')::rest ->
                if b = b' then
                  pre @ (R.add k k', b) :: rest
                else
                  f ((k',b')::pre) rest
          in
          aux (f [] accum) rest
    in
    aux xs ys

  let add_nodup xs ys = List.rev_append xs ys

  let neg xs = List.rev_map (fun (k,b) -> (R.neg k,b)) xs

  let sub xs ys = add xs (neg ys)

  let scl k xs = List.rev_map (fun (k',b) -> (R.mul k k', b)) xs

  let sum xss = List.fold_left add zero xss
end
