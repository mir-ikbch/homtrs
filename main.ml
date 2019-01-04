
open Printf
open Reader

let () =
  for i = 1 to Array.length Sys.argv -1 do
    let trs = read_file Sys.argv.(i) in
    let deg = Homcomp.degree trs in
    let signt = Homcomp.signt_from_trs trs in
    let m2 = Homcomp.del2til trs in
    let m1 = Homcomp.del1til trs signt in
    let m0 = Homcomp.del0til trs signt in
    if deg = 0 then
      let module M = Matrix.Make(Q) in
      let m1' = Array.map (Array.map Q.of_int) m1 in
      let ri1 = M.rank (Array.length m1) (Array.length m1.(0)) m1' in
      let rk1 = Array.length m1.(0) - ri1 in
      let sh2 = rk1 - Smith.num1s (Array.length m2) (Array.length m2.(0)) m2 in
      printf "%s\ndegree = 0\n#symbol = %d, #rule = %d, $cp = %d\ns(H2) = %d, s(H2)+s(im1) = %d\n\n"
             Sys.argv.(i) (List.length signt) (List.length trs) (List.length (Homcomp.crit_pairs trs)) sh2 (sh2+ri1)
    else if Homcomp.is_small_prime deg then
      let module F =
        struct
          type t = Farith.t
          let add = Farith.add deg
          let sub = Farith.sub deg
          let neg = Farith.neg deg
          let mul = Farith.mul deg
          let inv = Farith.inv deg
          let div = Farith.div deg
          let of_int = Farith.of_int deg
        end
      in
      let module M = Matrix.Make(F) in
      let signt = Homcomp.signt_from_trs trs in
      let m2 = Array.map (Array.map F.of_int) @@ Homcomp.del2til trs in
      let ri2 = M.rank (Array.length m2) (Array.length m2.(0)) m2 in
      let rk2 = Array.length m2.(0) - ri2 in
      let m1 = Array.map (Array.map F.of_int) @@ Homcomp.del1til trs signt in
      let ri1 = M.rank (Array.length m1) (Array.length m1.(0)) m1 in
      let rk1 = Array.length m1.(0) - ri1 in
      let m0 = Array.map (Array.map F.of_int) @@ Homcomp.del0til trs signt in
      let ri0 = M.rank (Array.length m0) (Array.length m0.(0)) m0 in
      let rk0 = Array.length m0.(0) - ri0 in
      printf "%s\ndegree = %d\n#symbol = %d, #rule = %d, #cp = %d\nrank(ker2) = %d, rank(im2) = %d, rank(ker1) = %d, rank(im1) = %d, rank(ker0) = %d, rank(im0) = %d, b2 = %d, b1 = %d, b2+rank(im1) = %d\n\n"
                Sys.argv.(i) deg (List.length signt) (List.length trs) (List.length (Homcomp.crit_pairs trs)) rk2 ri2 rk1 ri1 rk0 ri0 (rk1 - ri2) (rk0 - ri1) (rk1 - ri2 + ri1)
    else
      printf "%s\ndegree = %d\nnon applicable\n\n" Sys.argv.(i) deg
  done
