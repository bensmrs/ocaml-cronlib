(** This module provides the lexing/parsing tokens *)

%token COMMA DASH EOF EOL EQUAL HASH SLASH STAR
%token<string> COMMENT RAW WHITESPACE

%%
