(** This module provides the parser for crontabs *)

%{
  open Exceptions
  open Types

  (** Collect the results
      @param l The results *)
  let collect l =
    let rec collect_rec acc = function
      | []                           -> acc
      | Nothing::tl                  -> collect_rec acc tl
      | hd::[]                       -> hd::acc
      | Comment _ as c::Nothing::tl  -> collect_rec (c::acc) tl
      | Comment l::Comment l'::tl    -> collect_rec acc (Comment (l@l')::tl)
      | Comment l::Entry (c, l')::tl -> collect_rec (Entry (c, l@l')::acc) tl
      | Entry _ as e::tl             -> collect_rec (e::acc) tl in
    let rec filter acc = function
      | []                 -> acc
      | Entry _ as e::tl   -> filter (e::acc) tl
      | Comment _ as c::[] -> c::acc
      | _::tl              -> filter acc tl in
    collect_rec [] l |> filter []

  (** Process a single value
      @param n The value
      @param min The minimum value
      @param max The maximum value *)
  let at n min max =
    if n < min || n > max then raise (Out_of_bounds (n, min, max))
    else At n

  (** Process a value range
      @param n The left value
      @param n' The right value
      @param min The minimum value
      @param max The maximum value *)
  let between n n' min max =
    if n < min || n > max then raise (Out_of_bounds (n, min, max))
    else if n' < min || n' > max then raise (Out_of_bounds (n', min, max))
    else if n' < n then raise (Out_of_bounds (n', n, max))
    else Between (n, n')

  (** Process a value slice
      @param n The left value
      @param n' The right value
      @param n'' The slice
      @param min The minimum value
      @param max The maximum value *)
  let slice n n' n'' min max =
    if n < min || n > max then raise (Out_of_bounds (n, min, max))
    else if n' < min || n' > max then raise (Out_of_bounds (n', min, max))
    else if n'' < 1 || n'' > max then raise (Out_of_bounds (n'', 1, max))
    else if n' < n then raise (Out_of_bounds (n', n, max))
    else Slice (n, n', n'')
%}

%parameter<Getter : Types.GETTER>

%start<block list> start

%%

start: separated_nonempty_list(EOL, line) EOF { collect $1 }

(** Process a crontab line *)
line:
  | WHITESPACE* COMMENT           { Comment [$2] }
  | WHITESPACE*                   { Nothing }
  | WHITESPACE* entry             { Entry ($2, []) }
  | WHITESPACE* RAW EQUAL noneol* { Nothing }

noneol: WHITESPACE | DASH | SLASH | STAR | COMMA | EQUAL | RAW | HASH {}

(** Process a crontab entry *)
entry:
  | list_of(minutes) WHITESPACE list_of(hours) WHITESPACE list_of(days) WHITESPACE list_of(months)
    WHITESPACE list_of(dows) WHITESPACE command
      {
        let (u, c) = $11 in
        { minutes = $1; hours = $3; days = $5; months = $7; dows = $9; user = u; command = c }
      }

minutes: atom({0},{59}) { $1 }
hours: atom({0},{23}) { $1 }
days: atom({1},{31}) { $1 }
months: atom({1},{12}) { $1 }
dows: atom({0},{7}) { $1 }

command: RAW { Getter.get $1 }

list_of(X): separated_nonempty_list(COMMA, X) { $1 }

i(X): X { int_of_string $1 }

(** Process a crontab value *)
atom(MIN, MAX):
  | MIN MAX i(RAW)                          { at $3 $1 $2 }
  | MIN MAX i(RAW) DASH i(RAW)              { between $3 $5 $1 $2 }
  | MIN MAX STAR                            { Between ($1, $2) }
  | MIN MAX i(RAW) SLASH i(RAW)             { slice $3 $2 $5 $1 $2 }
  | MIN MAX i(RAW) DASH i(RAW) SLASH i(RAW) { slice $3 $5 $7 $1 $2 }
  | MIN MAX STAR SLASH i(RAW)               { slice $1 $2 $5 $1 $2 }
