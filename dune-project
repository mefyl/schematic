(lang dune 2.8)

(name schematic)

(generate_opam_files true)

(license GPLv3)

(maintainers "mefyl <mefyl@gruntech.org>")

(authors "mefyl <mefyl@gruntech.org>")

(version 0.13.1)

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
  (base64
   (>= 3.5.0))
  (ezjsonm
   (>= 1.1.0))
  (ocaml-base-compiler
   (>= 4.12.0))
  fmt
  ppxlib
  ppx_deriving
  ppx_here
  (ppx_yojson
   (and
    (>= 1.2.0)
    :with-test))
  (ptime
   (>= 1.0.0))
  semver2
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
   (>= 4.0.0))
  (js_of_ocaml-ppx
   (>= 4.0.0))
  (schematic
   (= :version))))

(package
 (name schematic-sqml)
 (synopsis "Derive SQML bindings from Ocaml types")
 (description "Deduce SQML bindings from schematic type metadata.")
 (depends
  (alcotest
   (and
    (>= 1.1.0)
    :with-test))
  (sqml
   (and
    (>= 0.5.0)
    (< 0.6.0)))
  (schematic
   (= :version))))

(package
 (name schematic-http)
 (synopsis "HTTP client with schematic encoding")
 (depends
  (cohttp-lwt
   (and
    (>= 5.0.0)
    (< 5.1.0)))
  (schematic
   (= :version))))

(package
 (name schematic-cli)
 (synopsis "Schematic CLI tools")
 (depends
  (schematic
   (= :version))))
