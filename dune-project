(lang dune 3.20)

(using directory-targets 0.1)

(name schematic)

(generate_opam_files true)

(use_standard_c_and_cxx_flags false)

(license LGPL-3.0-or-later)

(maintainers "mefyl <mefyl@gruntech.org>")

(authors "mefyl <mefyl@gruntech.org>")

(version 0.22.2)

(homepage https://gitlab.routine.co/routine/schematic)

(bug_reports https://gitlab.routine.co/routine/schematic/-/issues)

(executables_implicit_empty_intf true)

(implicit_transitive_deps false)

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
  bytesrw
  (ezjsonm
   (>= 1.1.0))
  fmt
  (hlist
   (and
    (>= 0.2.0)
    (< 0.3.0)))
  hmap
  logs
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
    (= 3.7.1)))
  (acid
   (< 0.13.0))))

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
  base
  brr
  (conf-npm :with-test)
  ezjsonm
  fmt
  (js_of_ocaml
   (and
    (>= 4.0.0)
    (< 7.0.0)))
  (js_of_ocaml-ppx
   (and
    (>= 4.0.0)
    (< 7.0.0)))
  (ppx_deriving :with-test)
  ppx_here
  ptime
  (ppx_yojson
   (and
    (>= 1.2.0)
    :with-test))
  (schematic
   (= :version))
  uri
  wasm_of_ocaml-compiler))

(package
 (name schematic-sexp)
 (synopsis "Derive sexps from Ocaml objects")
 (description "Encode to and from s-expressions via a schema.")
 (depends
  (csexp
   (>= 1.5.2))
  (alcotest
   (and
    (>= 1.1.0)
    :with-test))
  base
  fmt
  (ppx_deriving :with-test)
  (ppx_here :with-test)
  (schematic
   (= :version))
  sexplib))

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
  base
  cryptokit
  ppx_here
  (schematic
   (= :version))))

; Work around a concurrency bug with ocamlfind + dune pkg until it's fixed
; upstream.

; File "dune.lock/bytesrw.0.3.0.pkg", line 8, characters 6-11:
; 8 |       ocaml
;           ^^^^^
; Error: Logs for package bytesrw
; Warning: The library directory where ocamlbuild was originally installed (/builds/routine/schematic/_build/.sandbox/a085ee08f9d502a0339365e7db1f402b/_private/default/.pkg/ocamlbuild.0.16.1+dune-6439ba2990d98dd44846aa8272d56e5b/target/lib/ocamlbuild) either no longer exists, or does not contain a copy of the ocamlbuild library. Guessing that the correct library directory is /builds/routine/schematic/_build/_private/default/.pkg/ocamlbuild.0.16.1+dune-6439ba2990d98dd44846aa8272d56e5b/target/lib/ocamlbuild because a copy of the ocamlbuild library exists at that location, and because it is located at the expected path relative to the current ocamlbuild executable. This can happen if ocamlbuild's files are moved from the location where they were originally installed.pkg.ml: [ERROR] cmd ['ocamlbuild' '-use-ocamlfind' '-classic-display' '-j' '4' '-tag' 'debug'
;      '-build-dir' '_build' 'pkg.otarget']: exited with 10

(pin
 (url
  git+https://github.com/dbuenzli/bytesrw#47bdebcbc527c264572fb5fbcc5ca606e67bcd32)
 (package
  (name bytesrw)
  (version 0.3.0)))

; For Wasmoo support.

(pin
 (url
  git+https://github.com/mefyl/alcotest#f0007cbfdffd868945dd7503160c8bce08d01113)
 (package
  (name alcotest)
  (version 1.9.1)))
