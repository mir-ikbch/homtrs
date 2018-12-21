
open Printf
open Lexing

let finally handler f x =
  let r = try f x with e -> handler (); raise e in 
  handler (); r

let open_in_do ?path f =
  match path with 
  | None -> f stdin
  | Some file -> 
      let in_channel = open_in file in
      finally (fun () -> close_in in_channel) f in_channel


let syntax_error p =
  eprintf "File %S at line %d, character %d:@.Syntax error.@." 
    p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol)

let read_file filename =
  let f ch = 
    let lexbuf = from_channel ch in
    let lex_curr_p = 
      { lexbuf.lex_curr_p with pos_fname = filename } in
    try
      Trs_parse.rules Trs_lex.lex { lexbuf with lex_curr_p = lex_curr_p }
    with Parsing.Parse_error -> 
      (syntax_error lexbuf.lex_curr_p; exit 1)
  in
  try
    open_in_do ~path:filename f
  with Sys_error s -> 
    (eprintf "Error:@.%s@." s; exit 1)

let () =
  let trs = read_file Sys.argv.(1) in
  if Homcomp.is_var_preserving trs then
    let signt = Homcomp.signt_from_trs trs in
    let (rk2,ri2) = Homcomp.rki2 trs in
    let (rk1,ri1) = Homcomp.rki1 trs signt in
    let (rk0,ri0) = Homcomp.rki0 trs signt in
    printf "#symbol = %d, #rule = %d, #cp = %d\nrank(ker2) = %d, rank(im2) = %d, rank(ker1) = %d, rank(im1) = %d, rank(ker0) = %d, rank(im0) = %d, b2 = %d, b1 = %d\n"
            (List.length signt) (List.length trs) (List.length (Homcomp.crit_pairs trs)) rk2 ri2 rk1 ri1 rk0 ri0 (rk1 - ri2) (rk0 - ri1)
  else
    printf "not variable preserving\n"
