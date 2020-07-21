(lang dune 2.6)
(name minicaml)

(generate_opam_files true)

(package
  (name minicaml)
  (description "study project")
  (depends
    fmt
    ppx_inline_test
    (alcotest (and (>= 1.2.1) :with-test))
    (ocamlformat :dev)))
