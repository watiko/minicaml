(lang dune 2.6)
(name minicaml)

(generate_opam_files true)

(package
  (name minicaml)
  (maintainers watiko)
  (description "study project")
  (depends
    (ocaml (= 4.11.1))
    fmt
    ppx_inline_test
    (alcotest (and (>= 1.2.1) :with-test))
    (ocamlformat :with-test)))
