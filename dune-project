(lang dune 3.0)

(using directory-targets 0.1)

(name schematic)

(generate_opam_files true)

(use_standard_c_and_cxx_flags false)

(license LGPL-3.0-or-later)

(maintainers "mefyl <mefyl@gruntech.org>")

(authors "mefyl <mefyl@gruntech.org>")

(version 0.19.3)

(homepage https://gitlab.routine.co/routine/schematic)

(bug_reports https://gitlab.routine.co/routine/schematic/-/issues)

(executables_implicit_empty_intf true)

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
   (>= v0.15.0))
  (base64
   (>= 3.5.0))
  (ezjsonm
   (>= 1.1.0))
  fmt
  (hlist
   (and
    (>= 0.2.0)
    (< 0.3.0)))
  landmarks
  landmarks-ppx
  (ocaml
   (>= 5.2.0))
  ppxlib
  ppx_deriving
  ppx_here
  (ppx_yojson
   (and
    (>= 1.2.0)
    :with-test))
  (ptime
   (>= 1.0.0))
  (semver2
   (>= 1.2.0))
  uri)
 ; Dune 3.7 chokes on installation with files named the same anywhere in the tree, eg.
 ; Error: Multiple rules generated for
 ; _build/install/default/lib/schematic/src/schema.ml:
 ; - schematic/lib/dune:3
 ; - schematic/ppx/dune:3
 (conflicts
  (dune
   (or
    (= 3.7.0)
    (= 3.7.1)))))

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
  (conf-npm :with-test)
  (js_of_ocaml
   (>= 4.0.0))
  (js_of_ocaml-ppx
   (>= 4.0.0))
  landmarks
  landmarks-ppx
  (schematic
   (= :version))))

(package
 (name schematic-sexp)
 (synopsis "Derive sexps from Ocaml objects")
 (description "Encode to and from s-expressions via a schema.")
 (depends
  (alcotest
   (and
    (>= 1.1.0)
    :with-test))
  (schematic
   (= :version))
  sexplib))

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
    (>= 0.10.0)
    (< 0.12.0)))
  (schematic
   (= :version))))

(package
 (name schematic-http)
 (synopsis "HTTP client with schematic encoding")
 (depends
  (cohttp-lwt
   (and
    (>= 5.3.0)
    (< 6.0.0)))
  (lwt
   (>= 5.6.0))
  (timmy
   (and
    (>= 1.0.0)
    (< 2.0.0)))
  (timmy-lwt
   (and
    (>= 1.0.0)
    (< 2.0.0)))
  (schematic
   (= :version))))

(package
 (name schematic-ffi)
 (synopsis "Generate FFI types and converters based on schemas")
 (authors "Yoha\195\175-Eliel BERREBY <yberreby@pm.me>")
 (depends
  bake
  logs
  ppx_hash
  (schematic
   (= :version))
  (metapp :with-test)))

(package
 (name schematic-cli)
 (synopsis "Schematic CLI tools")
 (depends
  (schematic
   (= :version))))

(package
 (name entropic)
 (synopsis "Random dataset generation based on schematic")
 (depends
  cryptokit
  (schematic
   (= :version))))
