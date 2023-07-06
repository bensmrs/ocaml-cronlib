(** This module tests the library with Alcotest *)

open Alcotest
open Cronlib
open Cronlib__Types

let first = "# This is a sample system crontab
# -------------------------------

# This comment is filtered

# This is a comment attached to a command
0 */2 * * 1-5 root echo foo

# This is a multiline comment attached
# to a command
0 8-16/2 1,15 * * user echo bar

0 0 1 1,2,6-12/3 * root echo baz"

let second = "# This is a sample user crontab

* * * * * echo foo"

let first_parsed =
  [Comment [" This is a sample system crontab"; " -------------------------------"];
   Entry ({ minutes = [At 0]; hours = [Slice (0, 23, 2)]; days = [Between (1, 31)];
            months = [Between (1, 12)]; dows = [Between (1, 5)]; user = "root";
            command = "echo foo" }, [" This is a comment attached to a command"]);
   Entry ({ minutes = [At 0]; hours = [Slice (8, 16, 2)]; days = [At 1; At 15];
            months = [Between (1, 12)]; dows = [Between (0, 7)]; user = "user";
            command = "echo bar" }, [" This is a multiline comment attached"; " to a command"]);
   Entry ({ minutes = [At 0]; hours = [At 0]; days = [At 1];
            months = [At 1; At 2; Slice (6, 12, 3)]; dows = [Between (0, 7)]; user = "root";
            command = "echo baz" }, [])]

let second_parsed =
  [Comment [" This is a sample user crontab"];
   Entry ({ minutes = [Between (0, 59)]; hours = [Between (0, 23)]; days = [Between (1, 31)];
            months = [Between (1, 12)]; dows = [Between (0, 7)]; user = "root";
            command = "echo foo" }, [])]

(** A check for file processing *)
let test_process_file () =
  check bool "system crontab" true (process_file "system-crontab" = first_parsed);
  check bool "user crontab" true (process_file ~user:"root" "user-crontab" = second_parsed)

(** A check for string processing *)
let test_process_string () =
  check bool "system crontab" true (process_string first = first_parsed);
  check bool "user crontab" true (process_string ~user:"root" second = second_parsed)

let tests = [
  ("test_process_file", `Quick, test_process_file);
  ("test_process_string", `Quick, test_process_string)
]

let test_suites: unit test list = [
  "cronlib", tests;
]

(** Run the test suites *)
let () = run "cronlib" test_suites
