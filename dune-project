(lang dune 3.15)

(using directory-targets 0.1)

(name schematic)

(generate_opam_files true)

(use_standard_c_and_cxx_flags false)

(license LGPL-3.0-or-later)

(maintainers "mefyl <mefyl@gruntech.org>")

(authors "mefyl <mefyl@gruntech.org>")

(version 0.19.7)

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
  logs
  (alcotest
   (and
    (>= 1.1.0)
    :with-test))
  (base
   (>= v0.17.0))
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
  (ppx_deriving :with-test)
  uri
  ptime
  ppx_here
  fmt
  ezjsonm
  base
  (alcotest
   (and
    (>= 1.1.0)
    :with-test))
  (conf-npm :with-test)
  (js_of_ocaml
    (and
     (>= 4.0.0)
     (< 6.0.0)))
  (js_of_ocaml-ppx
   (and
     (>= 4.0.0)
     (< 6.0.0)))
  landmarks
  landmarks-ppx
  (schematic
   (= :version))))

(package
 (name schematic-sexp)
 (synopsis "Derive sexps from Ocaml objects")
 (description "Encode to and from s-expressions via a schema.")
 (depends
  (ppx_here :with-test)
  (ppx_deriving :with-test)
  fmt
  base
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
  (ppx_deriving :with-test)
  ppx_here
  base
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
  ppx_deriving
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
 (authors "Yohaï-Eliel BERREBY <yberreby@pm.me>")
 (depends
  (stdio :with-test)
  (alcotest :with-test)
  uri
  semver2
  ptime
  ppxlib
  ppx_here
  ppx_deriving
  fmt
  ezjsonm
  base64
  base
  bake
  logs
  ppx_hash
  (schematic
   (= :version))
  metapp))

(package
 (name schematic-cli)
 (synopsis "Schematic CLI tools")
 (depends
  ppxlib
  (schematic
   (= :version))))

(package
 (name entropic)
 (synopsis "Random dataset generation based on schematic")
 (depends
  (alcotest :with-test)
  ppx_here
  base
  cryptokit
  (schematic
   (= :version))))
