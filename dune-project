(lang dune 2.7)
(name eunix)
(generate_opam_files true)
(source (github ocaml-multicore/eioio))
(license ISC)
(authors "Anil Madhavapeddy" "Thomas Leonard")
(maintainers "anil@recoil.org")
(package
 (name eunix)
 (synopsis "effect-based direct-style IO for OCaml")
 (description "An effect-based IO implementation for multicore OCaml with fibres.")
 (depends
))
(package
 (name promise)
 (synopsis "effect-based promises")
 (description "An effect-based implementation of promises."))
