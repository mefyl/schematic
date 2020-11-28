(lang dune 2.2)

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
  (ezjsonm
   (>= 1.1.0))
  (js_of_ocaml
   (>= 3.6.0))
  (js_of_ocaml-ppx
   (>= 3.6.0))
  (merlin :dev)
  (ocamlformat
   (and
    (= 0.14.1)
    :dev))
  (pp
   (= 1.0.1))
  (ppxlib
   (>= 0.14.0))
  (ptime
   (>= 0.8.5))
  uri))
