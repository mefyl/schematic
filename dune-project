(lang dune 3.0)

(using directory-targets 0.1)

(name schematic)

(generate_opam_files true)

(use_standard_c_and_cxx_flags false)

(license LGPL-3.0-or-later)

(maintainers "mefyl <mefyl@gruntech.org>")

(authors "mefyl <mefyl@gruntech.org>")

(version 0.20.0)

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
  (logs
   (>= 0.7.0))
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
  (ppx_deriving
   (and
    (>= 6.0.3)
    :with-test))
  (uri
   (>= 4.4.0))
  (ptime
   (>= 1.2.0))
  (ppx_here
   (>= v0.17.0))
  (fmt
   (>= 0.9.0))
  (ezjsonm
   (>= 1.3.0))
  (base
   (>= v0.17.1))
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
  (ppx_here
   (and
    (>= v0.17.0)
    :with-test))
  (ppx_deriving
   (and
    (>= 6.0.3)
    :with-test))
  (fmt
   (>= 0.9.0))
  (base
   (>= v0.17.1))
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
  (ppx_deriving
   (and
    (>= 6.0.3)
    :with-test))
  (ppx_here
   (>= v0.17.0))
  (base
   (>= v0.17.1))
  (alcotest
   (and
    (>= 1.1.0)
    :with-test))
  (sqml
   (and
    (>= 0.11.0)
    (< 0.12.0)))
  (schematic
   (= :version))))

(package
 (name schematic-http)
 (synopsis "HTTP client with schematic encoding")
 (depends
  (eio_main
   (and
    :with-test
    (>= 1.0)))
  (http
   (and
    (>= 6.0.0~beta2)
    (< 7.0.0)))
  (nofuture
   (and
    (>= 0.1.0)
    (< 0.2.0)))
  (timmy
   (and
    (>= 1.1.3)
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
  (nofuture-lwt
   (and
    (>= 0.1.0)
    (< 0.2.0)))
  (nofuture-lwt-unix
   (and
    (>= 0.1.0)
    (< 0.2.0)
    :with-test))
  (schematic-http
   (= :version))
  (timmy-lwt
   (and
    (>= 1.1.3)
    (< 2.0.0)))
  (timmy-unix
   (and
    :with-test
    (>= 1.1.3)
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
  (eio
   (>= 1.0))
  (nofuture-eio
   (and
    (>= 0.1.0)
    (< 0.2.0)))
  (schematic-http
   (= :version))))

(package
 (name schematic-ffi)
 (synopsis "Generate FFI types and converters based on schemas")
 (authors "Yohaï-Eliel BERREBY <yberreby@pm.me>")
 (depends
  (stdio
   (and
    (>= 0)
    :with-test))
  (alcotest
   (and
    (>= 1.8.0)
    :with-test))
  (uri
   (>= 4.4.0))
  (semver2
   (>= 1.2.0))
  (ptime
   (>= 1.2.0))
  (ppxlib
   (>= 0.33.0))
  (ppx_here
   (>= v0.17.0))
  (ppx_deriving
   (>= 6.0.3))
  (fmt
   (>= 0.9.0))
  (ezjsonm
   (>= 1.3.0))
  (base64
   (>= 3.5.1))
  (base
   (>= v0.17.1))
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
  (ppxlib
   (>= 0.33.0))
  (schematic
   (= :version))))

(package
 (name entropic)
 (synopsis "Random dataset generation based on schematic")
 (depends
  (alcotest
   (and
    (>= 1.8.0)
    :with-test))
  (ppx_here
   (>= v0.17.0))
  (base
   (>= v0.17.1))
  cryptokit
  (schematic
   (= :version))))
