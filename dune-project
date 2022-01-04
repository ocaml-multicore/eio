(lang dune 2.9)
(name eunix)
(formatting disabled)
(generate_opam_files true)
(source (github ocaml-multicore/eio))
(license ISC)
(authors "Anil Madhavapeddy" "Thomas Leonard")
(maintainers "anil@recoil.org")
(package
 (name eio)
 (synopsis "Effect-based direct-style IO API for OCaml")
 (description "An effect-based IO API for multicore OCaml with fibres.")
 (depends
  (ctf (= :version))
  (cstruct (>= 6.0.1))
  lwt-dllist
  (alcotest (and (>= 1.4.0) :with-test))))
(package
 (name eunix)
 (synopsis "Eio implementation for Unix-compatible systems")
 (description "An eio implementation for Unix-compatible systems.")
 (depends
  (optint (>= 0.1.0))
  base-domains
  (ctf (= :version))
  (eio (= :version))
  (alcotest (and (>= 1.4.0) :with-test))
  (logs (>= 0.7.0))
  (fmt (>= 0.8.9))
  (bigstringaf (>= 0.7.0))
  (psq (>= 0.2.0))))
(package
 (name eio_linux)
 (synopsis "Eio implementation for Linux using io-uring")
 (description "An eio implementation for Linux using io-uring.")
 (depends
  (alcotest (and (>= 1.4.0) :with-test))
  base-domains
  (ctf (= :version))
  (eio (= :version))
  (eunix (= :version))
  (mdx (and (>= 1.10.0) :with-test))
  (logs (>= 0.7.0))
  (fmt (>= 0.8.9))
  (bigstringaf (>= 0.7.0))
  uring))
(package
 (name eio_luv)
 (synopsis "Eio implementation using luv (libuv)")
 (description "An eio implementation for most platforms, using luv.")
 (depends
  base-domains
  (ctf (= :version))
  (eio (= :version))
  (eunix (= :version))
  (luv (>= 0.5.8))
  (mdx (and (>= 1.10.0) :with-test))
  (logs (>= 0.7.0))
  (fmt (>= 0.8.9))
  (bigstringaf (>= 0.7.0))))
(package
 (name ctf)
 (synopsis "CTF tracing")
 (description "Trace IO events.")
 (depends
  (alcotest (and (>= 1.4.0) :with-test))
  (ppx_cstruct (>= 6.0.1))
  (ocplib-endian (>= 1.1))
  (mtime (>= 1.2.0))
  (cstruct (>= 6.0.1))))
(package
 (name eio_main)
 (synopsis "Effect-based direct-style IO mainloop for OCaml")
 (description "Selects an appropriate Eio backend for the current platform.")
 (depends
  (eio_linux (and (= :version) (= :os "linux")))
  (mdx (and (>= 1.10.0) :with-test))
  (eio_luv (= :version))))
(using mdx 0.1)
