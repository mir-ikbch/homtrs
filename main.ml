
open Printf
open Reader

let baseR = ref ""

let symbolR = ref ""

let d1flag = ref false

let d2flag = ref false

let core trs =
  let deg = Homcomp.degree trs in
  let signt = Homcomp.signt_from_trs trs in
  if !baseR = "" || !symbolR = "" then begin
      let m2 = Homcomp.del2til trs in
      let m1 = Homcomp.del1til trs signt in
      if deg = 0 then begin
        let module M = Matrix.Make(Q) in
        let m1' = Array.map (Array.map Q.of_int) m1 in
        let ri1 = M.rank (Array.length m1) (Array.length m1.(0)) m1' in
        let rk1 = Array.length m1.(0) - ri1 in
        let sh2 = rk1 - Smith.num1s (Array.length m2) (Array.length m2.(0)) m2 in
        printf "degree = 0\n#symbol = %d, #rule = %d, #cp = %d\ns(H2) = %d, #rule-e(R) = %d\n\n"
               (List.length signt) (List.length trs) (List.length (Homcomp.crit_pairs trs)) sh2 (sh2+ri1);
        if !d1flag then
          (print_string "D1:\n";
          Homcomp.print_matrix m1);
        if !d2flag then
          (print_string "D2:\n";
          Homcomp.print_matrix m2)
      end else if Homcomp.is_small_prime deg then begin
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
        printf "degree = %d\n#symbol = %d, #rule = %d, #cp = %d\ndim(H2) = %d, #rule-e(R) = %d\n\n"
                  deg (List.length signt) (List.length trs) (List.length (Homcomp.crit_pairs trs)) (rk0 - ri1) (rk1 - ri2 + ri1);
        if !d1flag then
          (print_string "D1:\n";
          Homcomp.print_matrix m1);
        if !d2flag then
          (print_string "D2:\n";
          Homcomp.print_matrix m2)
      end else
        printf "degree = %d\nnon applicable\n\n" deg
    end
    else begin      
      let base = String.split_on_char ' ' !baseR in
      let comm = [(Homcomp.Term(!symbolR, [Homcomp.Var 0; Homcomp.Var 1]), Homcomp.Term (!symbolR, [Homcomp.Var 1; Homcomp.Var 0]));
                  (Homcomp.Term(!symbolR, [Homcomp.Var 0; Homcomp.Term(!symbolR, [Homcomp.Var 1; Homcomp.Var 2])]),
                   Homcomp.Term(!symbolR, [Homcomp.Var 1; Homcomp.Term(!symbolR, [Homcomp.Var 0; Homcomp.Var 2])]))]
      in
      let m2 = Homcomp.del2til_ordered base trs comm in
      let m1 = Homcomp.del1til (List.rev_append trs comm) signt in
      if deg = 0 then begin
        let module M = Matrix.Make(Q) in
        let m1' = Array.map (Array.map Q.of_int) m1 in
        let ri1 = M.rank (Array.length m1) (Array.length m1.(0)) m1' in
        let rk1 = Array.length m1.(0) - ri1 in
        let rnk = Smith.num1s (Array.length m2) (Array.length m2.(0)) m2 in
        let m2' = Array.map (Array.map Q.of_int) m2 in
        let rnk' = M.rank (Array.length m2) (Array.length m2.(0)) m2' in
        printf "degree = 0\n#symbol = %d, #rule = %d\ns(H2) = %d, #rule-e(R) = %d\n\n"
               (List.length signt) (List.length trs + 2) (rk1 - rnk') (List.length trs + 2 - rnk);
        if !d1flag then
          (print_string "D1:\n";
          Homcomp.print_matrix m1);
        if !d2flag then
          (print_string "D2:\n";
          Homcomp.print_matrix m2)
      end else if Homcomp.is_small_prime deg then begin
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
        let m2 = Array.map (Array.map F.of_int) @@ Homcomp.del2til_ordered base trs comm in
        let ri2 = M.rank (Array.length m2) (Array.length m2.(0)) m2 in
        let m1 = Array.map (Array.map F.of_int) @@ Homcomp.del1til (List.rev_append trs comm) signt in
        let ri1 = M.rank (Array.length m1) (Array.length m1.(0)) m1 in
        let rk1 = Array.length m1.(0) - ri1 in
        printf "degree = %d\n#symbol = %d, #rule = %d\ndim(H2) = %d, #rule-e(R) = %d\n\n"
                  deg (List.length signt) (List.length trs + 2) (rk1 - ri2) (List.length trs + 2 - ri2);
        if !d1flag then
          (print_string "D1:\n";
          Homcomp.print_matrix m1);
        if !d2flag then
          (print_string "D2:\n";
          Homcomp.print_matrix m2)
      end else
        printf "degree = %d\nnon applicable\n\n" deg
    end

let wcore str =
  print_string (str ^ "\n");
  let trs = Trs_parse.rules Trs_lex.lex (Lexing.from_string str) in
  core trs

let fcore file =
  print_string (file ^ "\n");
  core (read_file file)

let clist =
  [ Arg.Set_string symbolR; Arg.Set_string baseR; Arg.String fcore ]

let speclist = Arg.align
  [ ("-w", Arg.String wcore, " TRS");
    ("-c", Arg.Tuple clist, " For theories with commutative laws");
    ("-d1", Arg.Set d1flag, " Show the matrix D1");
    ("-d2", Arg.Set d2flag, " Show the matrix D2")]


let () =
  Arg.parse speclist fcore "usage:"
