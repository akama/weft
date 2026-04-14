(lang dune 3.16)

(name weft)
(version 0.1.0)
(license ISC)

(generate_opam_files true)

(package
 (name weft)
 (synopsis "Terminal-based unified log search TUI")
 (description
  "Search and tail logs across local files, remote hosts (SSH/Teleport), and Loki in a unified timeline.")
 (depends
  (ocaml (>= 5.2.0))
  (dune (>= 3.16))
  (eio (>= 1.0))
  (eio_main (>= 1.0))
  (notty (>= 0.2))
  (otoml (>= 1.0))
  (ptime (>= 1.2.0))
  (re (>= 1.12.0))
  (yojson (>= 2.0.0))
  (cohttp-eio (>= 6.0.0))
  (digestif (>= 1.2.0))
  (camlzip (>= 1.12))
  (base64 (>= 3.0))
  (alcotest (and (>= 1.8.0) :with-test)))
 (depopts inotify))
