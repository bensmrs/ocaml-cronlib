(lang dune 2.7)
(using menhir 2.1)
(name cronlib)
(version 0.1.0)
(generate_opam_files true)
(license MIT)
(authors "Benjamin Somers <bsomers@resel.fr>")
(maintainers "Benjamin Somers <bsomers@resel.fr>")
(homepage "https://github.com/bensmrs/ocaml-cronlib")
(source (github "bensmrs/ocaml-cronlib"))
(bug_reports "https://github.com/bensmrs/ocaml-cronlib/issues")

(package
 (name cronlib)
 (synopsis "Cron parser")
 (description "This package provides a parser for crontab files")
 (depends
  (alcotest (and :with-test (>= 1.6.0)))))
