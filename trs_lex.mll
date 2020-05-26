{
open Trs_parse

}

let ident = ((_ # [' ' '\t' '\n' '(' ')' ',' '-' '#']) | ("-" (_ # [' ' '\t' '\n' '(' ')' ',' '>'])) | "-")*
let comment = '#' ((_ # ['\n'])*) ('\n' | eof)

rule lex = parse
| [' ' '\t' '\n'] { lex lexbuf }
| "("             { OPENPAR }
| ")"             { CLOSEPAR }
| ","             { COMMA }
| "->"            { ARROW }
| comment         { COMMENT }
| ident+ as s     { IDENT s }
| eof             { EOF }

{
}
