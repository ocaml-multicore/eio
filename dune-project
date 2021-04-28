(lang dune 2.7)
(name eunix)
(generate_opam_files true)
(source (github ocaml-multicore/eioio))
(license ISC)
(authors "Anil Madhavapeddy" "Thomas Leonard")
(maintainers "anil@recoil.org")
(package
 (name eunix)
 (synopsis "Effect-based direct-style IO for OCaml.")
 (description
  "An effect-based IO implementation for multicore OCaml with fibres.")
 (depends
  (ctf (= :version))
  (promise (= :version))
  (alcotest (and (>= 1.4.0) :with-test))
  (logs (>= 0.7.0))
  (fmt (>= 0.8.9))
  (bigstringaf (>= 0.7.0))
  (bheap (>= 2.0.0))))
(package
 (name promise)
 (synopsis "Effect-based promises.")
 (description "An effect-based implementation of promises.")
 (depends
  (ctf (= :version))
  (alcotest (and (>= 1.4.0) :with-test))))
(package
 (name ctf)
 (synopsis "CTF tracing.")
 (description "Trace IO events.")
 (depends
  (alcotest (and (>= 1.4.0) :with-test))
  (ppx_cstruct (>= 6.0.0))
  (ocplib-endian (>= 1.1))
  (mtime (>= 1.2.0))
  (cstruct (>= 6.0.0))))
