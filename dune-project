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
    (and (>= 1.1.0) :with-test)
    (and (>= 1.1.0) :dev)))
  (batteries
   (>= 3.0.0))
  (ezjsonm
   (>= 1.1.0))
  (merlin :dev)
  (ocamlformat
   (and
    (= 0.13.0)
    :dev))
  (ppxlib
   (>= 0.13.0))))
