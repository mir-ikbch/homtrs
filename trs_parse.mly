%{
let m = ref 0
let vs = Hashtbl.create 10
let find_or_add s =
  match Hashtbl.find_opt vs s with
  | Some k -> k
  | None -> m := !m + 1; Hashtbl.add vs s !m; !m
%}

%token <string> IDENT
%token OPENPAR CLOSEPAR COMMA ARROW EOF

%start rules term
%type <(Homcomp.term * Homcomp.term) list> rules
%type <Homcomp.term> term

%%
term :
| IDENT                           { Homcomp.Var (find_or_add $1) }
| IDENT OPENPAR termlist CLOSEPAR { Homcomp.Term ($1, $3) }
| IDENT OPENPAR CLOSEPAR          { Homcomp.Term ($1, []) }

termlist :
| term COMMA termlist             { $1 :: $3 }
| term                            { [$1] }

rule :
| term ARROW term                 { ($1,$3) }

rules :
| EOF                             { [] }
| rule rules                      { $1 :: $2 }

%%
