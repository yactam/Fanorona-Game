(lang dune 3.11)

(name fanorona)

(generate_opam_files true)

(source
 (github yactam/fanorona))

(authors "TAMDRARI Yacine" "ROUYER Aymeric")

(maintainers "TAMDRARI Yacine")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name fanorona)
 (synopsis "Fanorona board game developed using OCaml")
 (description "Fanorona is our first project using The OCaml programming language which is a strategy board game for two players. The game is indigenous to Madagascar..")
 (depends ocaml dune ppx_deriving (alcotest :with-test) qcheck qcheck-alcotest lwt js_of_ocaml js_of_ocaml-lwt)
 (tags
  (topics "OCaml" "Fanorona" "Board game" "2D game" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
