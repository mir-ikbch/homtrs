{
open Trs_parse

}

let ident = ((_ # [' ' '\t' '\n' '(' ')' ',' '-']) | ("-" (_ # [' ' '\t' '\n' '(' ')' ',' '>'])) | "-")*

rule lex = parse
| [' ' '\t' '\n'] { lex lexbuf }
| "("             { OPENPAR }
| ")"             { CLOSEPAR }
| ","             { COMMA }
| "->"            { ARROW }
| ident+ as s         { IDENT s }
| eof             { EOF }

{
}
