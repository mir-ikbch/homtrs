
module type Field = sig
  type t

  val add : t -> t -> t

  val sub : t -> t -> t

  val neg : t -> t

  val mul : t -> t -> t

  val inv : t -> t

  val div : t -> t -> t

  val of_int : int -> t
end

module Make (F : Field) = struct
  let swap a n m =
    if n <> m then
      let v = Array.copy a.(n) in
      a.(n) <- Array.copy a.(m);
      a.(m) <- v

  let div_row a n x =
    Array.iteri (fun i y -> a.(n).(i) <- F.div y x) a.(n)

  let submul a n m y =
    Array.iteri (fun i x -> a.(n).(i) <- F.sub x (F.mul y a.(m).(i))) a.(n)

  let reduction n m a =
    let swap_to = ref 0 in
    for j = 0 to m-1 do
      let pivot = ref (-1) in
      for i = !swap_to to n-1 do
        if !pivot = -1 then begin
          if a.(i).(j) = F.of_int 1 then
            (swap a i !swap_to; pivot := !swap_to; swap_to := !swap_to + 1)
          else if a.(i).(j) <> F.of_int 0 then
            (div_row a i a.(i).(j); pivot := !swap_to; swap a i !swap_to; swap_to := !swap_to + 1)
        end else
          if a.(i).(j) <> F.of_int 0 then
            submul a i !pivot a.(i).(j)
      done
    done

  let rank n m a =
    let a' = Array.init n (fun i -> Array.copy a.(i)) in
    reduction n m a';
    let rec loop i =
      if i >= n || Array.for_all (fun x -> x = F.of_int 0) a'.(i) then
        i
      else
        loop (i+1)
    in
    loop 0
end
