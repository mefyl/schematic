(lang dune 3.0)

(name schematic)

(generate_opam_files true)

(use_standard_c_and_cxx_flags false)

(license LGPL-3.0-or-later)

(maintainers "mefyl <mefyl@gruntech.org>")

(authors "mefyl <mefyl@gruntech.org>")

(version 0.19.1)

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
   (>= 4.14.0))
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
    (< 0.11.0)))
  (schematic
   (= :version))))

(package
 (name schematic-http)
 (synopsis "HTTP client with schematic encoding")
 (depends
  (eio_main :with-test)
  (http
   (and
    (>= 6.0.0~beta2)
    (< 7.0.0)))
  nofuture
  (timmy
   (and
    (>= 1.0.0)
    (< 2.0.0)))
  (schematic
   (= :version))))

(package
 (name schematic-cohttp-lwt)
 (synopsis "cohttp-lwt implementation of schematic-http")
 (depends
  (alcotest-lwt
   (and
    (>= 1.1.0)
    :with-test))
  (cohttp-lwt
   (and
    (>= 6.0.0~beta2)
    (< 7.0.0)))
  (cohttp-lwt-unix
   (and
    :with-test
    (>= 6.0.0~beta2)
    (< 7.0.0)))
  (lwt_eio :with-test)
  (lwt
   (>= 5.6.0))
  nofuture-lwt
  (schematic-http
   (= :version))
  (timmy-lwt
   (and
    (>= 1.0.0)
    (< 2.0.0)))
  (timmy-unix
   (and
    :with-test
    (>= 1.0.0)
    (< 2.0.0)))))

(package
 (name schematic-cohttp-eio)
 (synopsis "cohttp-eio implementation of schematic-http")
 (depends
  (alcotest
   (and
    (>= 1.1.0)
    :with-test))
  (cohttp-eio
   (and
    (>= 6.0.0~beta2)
    (< 7.0.0)))
  eio
  nofuture-eio
  (schematic-http
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
