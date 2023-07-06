(** This module provides the lexer for crontabs.
    It implements a simple state machine that counts crontab fields and switches to a special
    processing rule for lexing commands when the command is reached. *)

{
  open Tokens

  (** Rewind the lexbuf *)
  let rewind lexbuf =
    let open Lexing in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_cnum = lexbuf.lex_curr_p.pos_cnum - 1; } ;
    lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - 1

  (** The counter *)
  let lexed = ref 0

  (** Whether the lexing for the current line has started *)
  let lexing = ref false
}


let whitespace = [' ' '\t']+
let eol = ('\n' '\r' | '\r' '\n' | '\n' | '\r')
let noneol = [^ '\n' '\r']
let nonspecial = [^ '\n' '\r' '#' '-' '/' '*' ',' '=' ' ' '\t' ]


(** This rule chooses between regular or special lexing *)
rule token = parse
  | eof { lexed := 0; lexing := false; EOF }
  | eol { lexed := 0; lexing := false; EOL }
  | _   { rewind lexbuf; if !lexed = 5 then post lexbuf else pre lexbuf }

(** This rule performs regular lexing *)
and pre = parse
  | whitespace* '#' (noneol* as s) { COMMENT s }
  | whitespace+ as s               { if !lexing then lexed := !lexed+1; WHITESPACE s }
  | '-'                            { lexing := true; DASH }
  | '/'                            { lexing := true; SLASH }
  | '*'                            { lexing := true; STAR }
  | ','                            { lexing := true; COMMA }
  | '='                            { lexing := true; EQUAL }
  | nonspecial* as s               { lexing := true; RAW s }

(** This rule performs command lexing *)
and post = parse
  | noneol* as s { RAW s }
