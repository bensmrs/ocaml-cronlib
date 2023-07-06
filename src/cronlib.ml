(** This module provides functions to process crontabs *)


(** Process a Lexing buffer
    @param user The optional crontab owner
    @param lexbuf The Lexing buffer *)
let process_lexbuf ?user lexbuf =
  let module P = (val match user with
    | Some u -> (module Parser.Make (struct let get c = (u, c) end) : Types.PARSER)
    | None   -> (module Parser.Make (Types.Command_getter))) in
  P.start Lexer.token lexbuf


(** Process a channel
    @param user The optional crontab owner
    @param channel The channel *)
let process_channel ?user channel = process_lexbuf ?user (Lexing.from_channel channel)


(** Process a file
    @param user The optional crontab owner
    @param filename The file name *)
let process_file ?user filename = process_channel ?user (open_in filename)


(** Process a string
    @param user The optional crontab owner
    @param str The string *)
let process_string ?user str = process_lexbuf ?user (Lexing.from_string str)
