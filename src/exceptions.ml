(** This module provides the exceptions for the parser *)


(** Malformed command *)
exception Malformed_command

(** Crontab value out of bounds *)
exception Out_of_bounds of int * int * int
