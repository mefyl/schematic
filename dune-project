(lang dune 2.7)

(name schematic)

(generate_opam_files true)

(license GPLv3)

(maintainers "mefyl <mefyl@gruntech.org>")

(authors "mefyl <mefyl@gruntech.org>")

(source
 (github mefyl/schematic))

(package
 (name schematic)
 (synopsis "Derive schemas from Ocaml types")
 (depends
  (alcotest
   (or
    (and
     (>= 1.1.0)
     :with-test)
    (and
     (>= 1.1.0)
     :dev)))
  (base
   (>= 0.13.1))
  (dune
   (>= 2.7))
  (ezjsonm
   (>= 1.1.0))
  (js_of_ocaml
   (>= 3.6.0))
  (js_of_ocaml-ppx
   (>= 3.6.0))
  (merlin :dev)
  (ocamlformat
   (and
    (= 0.17.0)
    :dev))
  (ocaml-base-compiler
   (>= 4.12.0))
  fmt
  logs
  ppxlib
  (ppx_deriving :with-test)
  ppx_here
  (ptime
   (>= 0.8.5))
  uri))
