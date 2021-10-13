(lang dune 2.8)

(name schematic)

(generate_opam_files true)

(license GPLv3)

(maintainers "mefyl <mefyl@gruntech.org>")

(authors "mefyl <mefyl@gruntech.org>")

(version 0.10.0)

(homepage https://gitlab.routine.co/routine/schematic)

(bug_reports https://gitlab.routine.co/routine/schematic/-/issues)

(source
 (uri git+https://gitlab.routine.co/routine/schematic.git))

(package
 (name schematic)
 (synopsis "Derive schemas from Ocaml types")
 (description
  "Schematic lets you derive type meta informations using a ppx_deriving\ndriver and provide functionalities on top of it:\n\n* Conversion to and from JSON.\n* Deduction of JSON schema specification.\n* Conversion to and from js_of_ocaml FFI using schematic-jsoo.\n* Deduction of typescript definitions.\n")
 (depends
  (alcotest
   (and
    (>= 1.1.0)
    :with-test))
  (base
   (>= 0.13.1))
  (ezjsonm
   (>= 1.1.0))
  (merlin :dev)
  (ocamlformat
   (and
    (= 0.17.0)
    :dev))
  (ocaml-base-compiler
   (>= 4.12.0))
  fmt
  ppxlib
  ppx_deriving
  ppx_here
  (ptime
   (>= 0.8.5))
  uri))

(package
 (name schematic-jsoo)
 (synopsis "Derive js_of_ocaml types and values from Ocaml types")
 (description
  "Deduce js_of_ocaml FFI types\n and conversion functions from schematic type metadata.")
 (depends
  (alcotest
   (and
    (>= 1.1.0)
    :with-test))
  (js_of_ocaml
   (>= 3.6.0))
  (js_of_ocaml-ppx
   (>= 3.6.0))
  (schematic
   (= :version))))
