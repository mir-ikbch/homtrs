
exception Err of string

let swap a n m =
  if n <> m then
    let v = Array.copy a.(n) in
    a.(n) <- Array.copy a.(m);
    a.(m) <- v

let swap' a j j' =
  if j <> j' then
    Array.iteri (fun i _ ->
      let y = a.(i).(j) in
      a.(i).(j) <- a.(i).(j');
      a.(i).(j') <- y
    ) a

let rec eucl a i i' j =
  if a.(i').(j) <> 0 then
    let q = a.(i).(j) / a.(i').(j) in
    Array.iteri (fun k x -> a.(i).(k) <- x - q * a.(i').(k)) a.(i);
    eucl a i' i j

let rec eucl' a j j' i =
  if a.(i).(j) <> 0 then
    let q = a.(i).(j') / a.(i).(j) in
    Array.iteri (fun k x -> a.(k).(j') <- x.(j') - q * a.(k).(j)) a;
    eucl' a j' j i

let min a b = if a < b then a else b

let rec seq st ed =
  if st > ed then
    []
  else
    st::seq (st+1) ed

let reduction n m a =
  for j = 0 to m - 1 do
    let rec aux () = 
      let rec loop () =
        for i = j + 1 to n - 1 do
          eucl a j i j;
          if a.(j).(j) = 0 && a.(i).(j) <> 0 then
            swap a j i
        done;
        if j < n - 1 then begin
          for j' = j + 1 to m - 1 do
            eucl' a j j' j;
            if a.(j).(j) = 0 && a.(j).(j') <> 0 then
              swap' a j j'
          done;
          if List.exists (fun i -> a.(i).(j) <> 0) (seq (j+1) (n-1)) then
            loop ()
        end
      in
      loop ();
      if j < min (n - 1) (m-1) then
        if a.(j).(j) <> 0 then
          let rec check i' j' =
            if a.(i').(j') mod a.(j).(j) <> 0 then begin
              Array.iteri (fun k _ -> a.(j).(k) <- a.(j).(k) + a.(i').(k)) a.(j);
              aux ()
            end else if j' >= m - 1 then begin
              if i' < n - 1 then
                check (i'+1) 0
            end else
              check i' (j'+1)
          in
          check (j+1) (j+1)
    in
    aux ()
  done

let num1s n m a =
  let a' = Array.init n (fun i -> Array.copy a.(i)) in
  reduction n m a';
  List.fold_left (fun accum i -> if abs a'.(i).(i) = 1 then accum+1 else accum) 0 (seq 0 (min (n-1) (m-1)))
