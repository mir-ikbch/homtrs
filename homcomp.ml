open Util
open Printf

type var = int

type term = Var of var | Term of string * term list

let rec pt = function
  | Var x ->
      if x = -1 then
        "[]"
      else
        string_of_int x
  | Term (c, ts) ->
      match ts with
      | [] -> c
      | _ ->
          "(" ^ c ^ " " ^ with_space ts ^ ")"
and with_space = function
  | [t] -> pt t
  | t::ts -> pt t ^ " " ^ with_space ts

let rec with_comma = function
  | [t] -> pt t
  | t::ts -> pt t ^ ", " ^ with_comma ts

let pp_t t = print_string (pt t)

module IntSet = Set.Make(
  struct
    let compare = Pervasives.compare
    type t = int
  end)


let var_set =
  let rec aux res = function
    | Var x -> IntSet.add x res
    | Term (_, ts) -> aux' res ts
  and aux' res = function
    | [] -> res
    | t::ts -> aux' (aux res t) ts
  in
  aux IntSet.empty

let maximum = List.fold_left (fun res x -> if x < res then res else x) 0

let rec maxid = function
  | Var x -> x
  | Term (c, ts) -> maximum (List.rev_map maxid ts)

let rename n =
  let rec aux = function
    | Var x -> if x = -1 then Var (-1) else Var (x + n)
    | Term (c, ts) -> Term (c, List.map aux ts)
  in
  aux

let rec subst1 t (x, s) =
  match t with
  | Var y -> if x = y then s else Var y
  | Term (c, ts) -> Term (c, List.map (fun r -> subst1 r (x, s)) ts)

let rec subst t = function
  | [] -> t
  | rule::rest -> subst (subst1 t rule) rest

type substitution = (var * term) list

let occurin x =
  let rec aux = function
    | Var y -> x = y
    | Term (c, ts) -> aux' ts
  and aux' = function
    | [] -> false
    | t::ts -> if aux t then true else aux' ts
  in
  aux

let rev_combine = List.rev_map2 (fun t1 t2 -> t1,t2)

(* simple unification
 * input: equation system [(l1, r1); ...; (ln, rn)]
 * output: the most general substitution sb = [(x1, t1); ...; (xk, tk)] which satisfies
 *         subst l1 sb = subst r1 sb, ..., subst ln sb = subst rn sb
 *)
let unify ?(freez=IntSet.empty) =
  let rec aux res eqs =
    match res with
    | None -> None
    | Some res ->
        match eqs with
        | [] -> Some res
        | (t, s)::rest when t = s -> aux (Some res) rest
        | (Var x, Var y)::rest ->
            if IntSet.mem x freez then 
              if IntSet.mem y freez then None
              else
                aux (Some ((y, Var x) :: List.rev_map (fun (z, t) -> z, subst t [y, Var x]) res))
                    (List.rev_map (fun (t1, t2) -> subst t1 [y, Var x], subst t2 [y, Var x]) rest)
            else
              aux (Some ((x, Var y) :: List.rev_map (fun (z, t) -> z, subst t [x, Var y]) res))
                  (List.rev_map (fun (t1, t2) -> subst t1 [x, Var y], subst t2 [x, Var y]) rest)
        | (Var x, t)::rest ->
            if IntSet.mem x freez || occurin x t
            then None
            else aux (Some ((x, t):: List.rev_map (fun (z, s) -> z, subst s [x, t]) res))
                     (List.rev_map (fun (t1, t2) -> subst t1 [x, t], subst t2 [x, t]) rest)
        | (t, Var x)::rest ->
            if IntSet.mem x freez || occurin x t
            then None
            else aux (Some ((x,t):: List.rev_map (fun (z, s) -> z, subst s [x, t]) res))
                 (List.rev_map (fun (t1, t2) -> subst t1 [x, t], subst t2 [x, t]) rest)
        | (Term (c, ts), Term (c', ts'))::rest ->
            if c = c'
            then aux (Some res) (rev_combine ts ts' @^ rest)
            else None
  in
  aux (Some [])


type ('a,'b) either = Left of 'a | Right of 'b

let find_l tbl x =
  Hashtbl.find_opt tbl (Left x)

let find_r tbl x =
  Hashtbl.find_opt tbl (Right x)

exception Already_exists

let add_bh tbl x y =
  match (find_l tbl x, find_r tbl y) with
  | (None,None) -> Hashtbl.add tbl (Left x) y; Hashtbl.add tbl (Right y) x
  | _ -> raise Already_exists

(* * [teq t1 t2] checks if [t1] and [t2] are alpha-equivalent *)
let teq t1 t2 =
  let mapping = Hashtbl.create 10 in
  let rec aux = function
    | (Var x, Var y) ->
      begin
        match find_l mapping x with
        | Some z -> y = z
        | None ->
          try add_bh mapping x y; true
          with Already_exists -> false
      end
    | (Var _,_) | (_, Var _) -> false
    | (Term (c,ts), Term (c',ts')) ->
      if c <> c' then
        false
      else
        aux_list (ts,ts')
  and aux_list = function
    | ([],[]) -> true
    | ([],_) | (_,[]) -> false
    | (t::ts, t'::ts') ->
      if aux (t,t') then
        aux_list (ts,ts')
      else
        false
  in
  aux (t1,t2)

let sq = Var (-1)

let subst_ctx t s = subst1 t (-1, s)

type bicontext = term * substitution

let bc_mul (ctx1,t1) (ctx2,t2) =
  (subst_ctx ctx1 (subst ctx2 t1), List.map (fun (x,t) -> (x, subst t t1)) t2)

let subst_bctx t (ctx,sbt) =
  subst (subst_ctx ctx t) sbt

let match_const a c =
  a = "0" && c.[0] = '0' || a = c

let gt_base base c d =
  if c.[0] = '0' && d.[0] = '0' then
    int_of_string c > int_of_string d
  else
    let rec aux flag = function
      | a::l -> if match_const a c then aux true l else if match_const a d then flag else aux flag l
      | [] -> false
    in
    aux false base

let rec gt_lpo base s t =
  match s, t with
  | Term (c, ss), Term (d,ts) -> 
      List.exists (fun si -> si = t || gt_lpo base si t) ss
      || (gt_base base c d && List.for_all (fun ti -> gt_lpo base s ti) ts)
      || (c = d && gt_lex_lpo base ss ts && List.for_all (fun ti -> gt_lpo base s ti) ts)
  | _, _ -> false
and gt_lex_lpo base ss ts =
  match ss, ts with
  | [], [] -> false
  | si::ss', ti::ts' -> if gt_lpo base si ti then true else si = ti && gt_lex_lpo base ss' ts'

(* computing critical pairs *)

let overlap sbt (l, r) (t, s) =
  match t with
  | Var _ -> None
  | _ ->
      let m = maxid t in
      let l' = rename m l in
      let r' = rename m r in
      option_map (fun rule -> subst r' rule, subst s rule) (unify [l', t])

let isVar = function
  | Var _ -> true
  | _ -> false


(* * find overlaps between [l] and subterms of [t]
   * input : two rules [(l,r)] [(t,s)]
   * output : list of [(r', s', (sq,(t,s),sbt), (ctx,(l,r),sbt))] for
              [subst_bctx l (ctx,sbt) = subst t sbt],
              [r' = subst_bctx r (ctx,sbt)], [s' = subst s sbt]
   * [l] should not be a variable.
   *)
let overlap_to_subterms ?(avoid_triv=false) ?(avoid_top=false) ?(freez=IntSet.empty) (l, r) (t, s) =
  let m = maxid t in
  let (l,r) =
    if List.exists (fun i -> occurin i l && occurin i t) (seq 1 m) then
      (rename m l, rename m r)
    else
      (l,r)
  in
  let rec matchrec ctx res = function
    | Var x ->
        if avoid_triv || IntSet.mem x freez then
          res
        else
          (subst (ctx r) [x, l], subst s [x, l], (ctx sq,(l,r),[x,l]), (sq,(t,s),[])) :: res
    | Term (c, ts) ->
        match unify ~freez:freez [l, Term (c, ts)] with
        | None -> matchrec_list ctx res c [] ts
        | Some sbt ->
            if (avoid_triv && teq l t && ctx sq = sq) || (avoid_top && ctx sq = sq) then
              matchrec_list ctx res c [] ts
            else
              matchrec_list ctx ((subst (ctx r) sbt, subst s sbt, (ctx sq,(l,r),sbt), (sq,(t,s),sbt)) :: res) c [] ts
  and matchrec_list ctx res c ts0 = function
    | [] -> res
    | t::ts ->
        let ovlps = matchrec (fun x -> ctx (Term (c, ts0 @^ (x :: ts)))) res t in
        matchrec_list ctx ovlps c (t :: ts0) ts
  in
  matchrec (fun x -> x) [] t

let rec crit_pairs' (l, r) = function
  | [] -> []
  | (l', r')::rest ->
      overlap_to_subterms ~avoid_triv:true (l, r) (l', r')
      @^ overlap_to_subterms ~avoid_triv:true ~avoid_top:true (l', r') (l, r)
      @^ crit_pairs' (l, r) rest

(* * compute critical pairs
   * input : list of rules [[(l1,r1); ...; (ln,rn)]]
   * output : concatenation of all [overlap_to_subterms (li,ri) (lj,rj)]s
   *)
let rec crit_pairs = function
  | [] -> []
  | (l, r)::rest ->
      overlap_to_subterms ~avoid_triv:true (l, r) (l, r) @^ crit_pairs' (l, r) rest @^ crit_pairs rest

let prime_crit_pairs' rules ovlps =
  List.filter (fun (_,_,_,(ctx,(l,r),sbt)) ->
    List.for_all (fun rule ->
      List.for_all (fun (_,_,_,(ctx,_,_)) ->
        ctx = Var (-1)
      ) (overlap_to_subterms ~avoid_triv:true rule (subst l sbt, subst r sbt))
    ) rules
  ) ovlps

let prime_crit_pairs rules = prime_crit_pairs' rules (crit_pairs rules)
      
let reduce t rules =
  let vs = var_set t in
  let m = maxid t in
  let rec aux s accum = function
    | [] -> (s,accum)
    | (l,r)::rest ->
        let l' = rename m l in
        let r' = rename m r in
        let ovlps = overlap_to_subterms ~freez:vs (l', r') (s, s) in
        match ovlps with
        | [] -> aux s accum rest
        | (snew,_,pnew,_)::_ -> aux snew (pnew::accum) rules
  in
  aux t [] rules

let rewrite t rules =
  fst (reduce t rules)

let ordered_reduce base t rules identities =
  let rules' = List.rev_append (List.rev_map (fun r -> (r,true)) rules) (List.rev_map (fun r -> (r,false)) identities) in
  let vs = var_set t in
  let m = maxid t in
  let rec aux s accum = function
    | [] -> (s, accum)
    | ((u,v),isordered)::rest ->
        let u' = rename m u in
        let v' = rename m v in
        let ovlps = overlap_to_subterms ~freez:vs (u',v') (s,s) in
        match List.find_opt (fun (snew,_,_,_) -> isordered || gt_lpo base s snew) ovlps with
        | None -> aux s accum rest
        | Some (snew,_,pnew,_) -> aux snew (pnew::accum) rules'
  in
  aux t [] rules'

let ordered_rewrite base t rules identities =
  fst (ordered_reduce base t rules identities)

module FM = FreeModule

module Z = struct
  type t = int
  let zero = 0
  let add = (+)
  let sub = (-)
  let neg = fun x -> -x
  let mul = ( * )
end

module ZMod = FM.Make(Z)

module ZK = struct
  type t = bicontext ZMod.t
  let zero = ZMod.zero
  let add = ZMod.add
  let sub = ZMod.sub
  let neg = ZMod.neg

  let mul' xs (n,y) : t =
    List.rev_map (fun (m,x) -> (Z.mul m n, bc_mul x y)) xs

  let mul xs ys =
    List.concat (List.rev_map (mul' xs) ys)
end


let rec kappa i = function
  | Var j -> if i = j then [sq] else []
  | Term (c,ts) ->
      match ts with
      | [] -> []
      | t::rest -> kappa_aux i c [] t rest
and kappa_aux i c pre t rest =
  let res = kappa i t in
  let mid = List.rev_map (fun x -> (Term (c, pre @ x :: rest))) res in
  match rest with
  | [] -> mid
  | s::rest' ->
      mid @^ kappa_aux i c (pre@[t]) s rest'

let del0 (c,n) =
  (-1,(sq,[(1,Term (c, List.map (fun i -> Var i) (seq 1 n)))])) ::
    List.rev_map (fun i -> (1, (Term(c, List.map (fun i -> Var i) (seq_but 1 n i)), [(1,Var i)]))) (seq 1 n)

module M = FM.Make(ZK)

let rec del1' = function
  | Var i -> M.zero
  | Term(c,args) ->
      let n = List.length args in
      M.add [([(1,(sq, List.combine (seq 1 n) args))], (c,n))]
            (M.sum (List.rev_map (fun i ->
              M.scl [(1,(Term (c, modify (i-1) sq args), []))] (del1' (List.nth args (i-1)))
              ) (seq 1 n)))

let del1 (l,r) =
  M.sub (del1' r) (del1' l)

let del2' trs t =
  let (_,rs) = reduce t trs in
  List.map (fun (ctx,rule,sbt) -> ([1,(ctx,sbt)],rule)) rs

let del2 trs ((ctx1,(l1,r1),sbt1),(ctx2,(l2,r2),sbt2)) =
  M.add (M.add [([1,(ctx2,sbt2)], (l2,r2))] [([-1,(ctx1,sbt1)], (l1,r1))])
        (M.sub (del2' trs (subst_bctx r2 (ctx2,sbt2)))
               (del2' trs (subst_bctx r1 (ctx1,sbt1))))

let rec subst_const = function
  | Var i -> Term ("0"^string_of_int i, [])
  | Term (c,ts) -> Term (c, List.map subst_const ts)

let del2'_ordered base rules idents t =
  let t' = subst_const t in
  let (_,rs) = ordered_reduce base t' rules idents in
  List.map (fun (ctx,rule,sbt) -> ([1,(ctx,sbt)],rule)) rs

let del2_ordered base rules idents ((ctx1,(l1,r1),sbt1),(ctx2,(l2,r2),sbt2)) =
  M.add (M.add [([1,(ctx2,sbt2)], (l2,r2))] [([-1,(ctx1,sbt1)], (l1,r1))])
        (M.sub (del2'_ordered base rules idents (subst_bctx r2 (ctx2,sbt2)))
               (del2'_ordered base rules idents (subst_bctx r1 (ctx1,sbt1))))

let matrix_init row col f =
  Array.init row (fun i ->
    Array.init col (fun j ->
      f i j
    )
  )

let sum_coeff =
  List.fold_left (fun accum (k,(ctx,_)) ->
    k + accum
  ) 0

let del0til trs signt =
  matrix_init 1 (List.length signt) (fun _ j ->
    let xs = del0 (List.nth signt j) in
    sum_coeff xs
  )

let del1til trs signt =
  matrix_init  (List.length signt) (List.length trs) (fun i j ->
    let xs = del1 (List.nth trs j) in
    let rec aux = function
      | [] -> 0
      | (coeffs,c)::rest ->
          if List.nth signt i = c then
            sum_coeff coeffs
          else
            aux rest
    in
    aux xs
  )

let del2til trs =
  let cps = prime_crit_pairs trs in
  matrix_init (List.length trs) (List.length cps) (fun i j ->
    let (_,_,p,q) = List.nth cps j in
    let xs = del2 trs (p,q) in
    let rec aux = function
      | [] -> 0
      | (coeffs,(l,_))::rest ->
          if teq l (fst (List.nth trs i)) then
            sum_coeff coeffs + aux rest
          else
            aux rest
    in
    aux xs
  )

let del2til_ordered base rules idents =
  let rs = List.rev_append rules idents in
  let cps = crit_pairs rs in
  matrix_init (List.length rs) (List.length cps + List.length idents) (fun i j ->
    if j >= List.length cps then
      if i - List.length rs = j - List.length cps then 2 else 0
    else
      let (_,_,p,q) = List.nth cps j in
      let xs = del2_ordered base rules idents (p,q) in
      let rec aux = function
        | [] -> 0
        | (coeffs,(l,_))::rest ->
            if teq l (fst (List.nth rs i)) then
              sum_coeff coeffs + aux rest
            else
              aux rest
      in
      aux xs
  )

let print_matrix m =
  Array.iter (fun l -> Array.iter (fun a -> if a < 0 then printf "%d " a else printf " %d " a) l; print_string "\n") m

(*
let rki0 trs signt =
  let mat = del0til trs signt in
  let d = Mat.dim2 mat in
  let x = rank mat in
  (d - x, x)

let rki1 trs signt =
  let mat = del1til trs signt in
  let d = Mat.dim2 mat in
  let x = rank mat in
  (d - x, x)

let rki2 trs =
  let mat = del2til trs in
  let d = Mat.dim2 mat in
  let x = rank mat in
  (d - x, x)

let betti1 trs signt =
  let mat1 = del0til trs signt in
  Mat.dim2 mat1 - rank mat1 - rank (del1til trs signt)

let betti2 trs signt =
  let mat1 = del1til trs signt in
  Mat.dim2 mat1 - rank mat1 - rank (del2til trs)
  *)

module SS = Set.Make (struct
  type t = string * int
  let compare = compare
end)

let rec signt_from_term s = function
  | Var x -> s
  | Term (c,args) ->
      signt_from_terms (SS.add (c,List.length args) s) args
and signt_from_terms s = function
  | [] -> s
  | t::rest ->
      signt_from_terms (signt_from_term s t) rest

let signt_from_trs trs =
  let s = 
    List.fold_left (fun accum (l,r) ->
      signt_from_term (signt_from_term accum r) l
    ) SS.empty trs
  in
  SS.elements s

let numoccur x =
  let rec aux accum = function
    | Var y -> if x = y then (accum+1) else accum
    | Term (_,ts) -> List.fold_left (fun x t -> aux x t) accum ts
  in
  aux 0

let rec is_var_preserving = function
  | [] -> true
  | (l,r)::trs ->
    let vs = var_set l in
    if
      IntSet.for_all (fun v ->
        numoccur v l = numoccur v r
      ) vs
    then
      is_var_preserving trs
    else
      false

let rec gcd a b =
  if b = 0 then a else gcd b (a mod b)

let degree = 
  let rec aux accum = function
    | [] -> accum
    | (l,r)::trs ->
      let vs = var_set l in
      let g = IntSet.fold (fun v g' -> gcd g' (abs (numoccur v l - numoccur v r))) vs accum in
      aux g trs
  in
  aux 0

let is_small_prime x = List.mem x [2;3;5;7]
