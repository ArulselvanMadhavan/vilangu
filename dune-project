(lang dune 3.7)

(name vilangu)

(generate_opam_files true)

(using menhir 2.1)
(source
 (github ArulselvanMadhavan/vilangu))

(authors "Arulselvan Madhavan")

(maintainers "Arulselvan Madhavan")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name vilangu)
 (synopsis "tiger compiler in OCaml")
 (description "Tiger compiler")
 (depends ocaml
          dune
          (menhir (>= 20230415))
          (base (>= 0.15.1))
          (ppx_sexp_conv (>= 0.15.1))
          (ppx_deriving (>= 5.2.1))
          (ppx_deriving_protobuf (>= 3.0.0))
          (fpath (>= 0.7.3))
          (ppx_let (>= 0.15.0))
          (cmdliner (>= 1.2.0)))
 (tags
  (topics compiler tiger)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
