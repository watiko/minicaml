(lang dune 2.9)
(name minicaml)

(generate_opam_files true)

(package
  (name minicaml)
  (maintainers watiko)
  (description "study project")
  (depends
    ocaml
    fmt
    ppx_inline_test
    (alcotest :with-test)
    (ocamlformat :with-test)))
