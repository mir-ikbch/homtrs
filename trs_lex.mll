{
open Trs_parse

}

let ident = ((_ # [' ' '\t' '\n' '\r' '(' ')' ',' '-' '#']) | ("-" (_ # [' ' '\t' '\n' '\r' '(' ')' ',' '>'])) | "-")*
let comment = '#' ((_ # ['\n'])*) ('\n' | eof)

rule lex = parse
| [' ' '\t' '\n' '\r'] { lex lexbuf }
| "("             { OPENPAR }
| ")"             { CLOSEPAR }
| ","             { COMMA }
| "->"            { ARROW }
| comment         { COMMENT }
| ident+ as s     { IDENT s }
| eof             { EOF }

{
}
