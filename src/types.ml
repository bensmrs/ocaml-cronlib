(** This module provides the types used throughout the library *)


(** Crontab frequency value *)
type frequency = At of int | Between of int * int | Slice of int * int * int

(** Crontab entry *)
type entry = { minutes: frequency list; hours: frequency list; days: frequency list;
               months: frequency list; dows: frequency list; user: string; command: string }

(** Crontab annotated block *)
type block = Nothing | Comment of string list | Entry of entry * string list

(** Signature for modules getting a user name *)
module type GETTER = sig
  val get : string -> string * string
end

(** Module getting user names from crontab lines *)
module Command_getter = struct
  let get command = match Str.(bounded_split (regexp "[ \t]+") command 2) with
    | user::cmd::[] -> (user, cmd)
    | _             -> raise Exceptions.Malformed_command
end

(** Signature for parsers *)
module type PARSER = sig
  val start : (Lexing.lexbuf -> Tokens.token) -> Lexing.lexbuf -> block list
end
