(rule (target schematic.%{version:schematic}.opam)
 (deps (:opam schematic.opam))
 (action
  (with-stdout-to %{target}
   (progn (cat %{opam})
    (echo
     "url { src: \"git://git@gitlab.routine.co:routine/schematic#%{version:schematic}\" }")))))
(rule (deps (universe)) (target schematic.opam.locked)
 (action (run %{bin:opam} lock schematic)))
(rule (alias extdeps) (mode (promote (until-clean)))
 (target schematic.opam.extdeps)
 (action
  (with-stdout-to %{target}
   (run %{dep:.logistic/dune/extdeps/extdeps.exe} rewrite --input
    %{dep:schematic.opam.locked} --exclude
    schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,bake))))
(rule (target schematic-cli.%{version:schematic-cli}.opam)
 (deps (:opam schematic-cli.opam))
 (action
  (with-stdout-to %{target}
   (progn (cat %{opam})
    (echo
     "url { src: \"git://git@gitlab.routine.co:routine/schematic-cli#%{version:schematic-cli}\" }")))))
(rule (deps (universe)) (target schematic-cli.opam.locked)
 (action (run %{bin:opam} lock schematic-cli)))
(rule (alias extdeps) (mode (promote (until-clean)))
 (target schematic-cli.opam.extdeps)
 (action
  (with-stdout-to %{target}
   (run %{dep:.logistic/dune/extdeps/extdeps.exe} rewrite --input
    %{dep:schematic-cli.opam.locked} --exclude
    schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,bake))))
(rule (target schematic-ffi.%{version:schematic-ffi}.opam)
 (deps (:opam schematic-ffi.opam))
 (action
  (with-stdout-to %{target}
   (progn (cat %{opam})
    (echo
     "url { src: \"git://git@gitlab.routine.co:routine/schematic-ffi#%{version:schematic-ffi}\" }")))))
(rule (deps (universe)) (target schematic-ffi.opam.locked)
 (action (run %{bin:opam} lock schematic-ffi)))
(rule (alias extdeps) (mode (promote (until-clean)))
 (target schematic-ffi.opam.extdeps)
 (action
  (with-stdout-to %{target}
   (run %{dep:.logistic/dune/extdeps/extdeps.exe} rewrite --input
    %{dep:schematic-ffi.opam.locked} --exclude
    schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,bake))))
(rule (target schematic-http.%{version:schematic-http}.opam)
 (deps (:opam schematic-http.opam))
 (action
  (with-stdout-to %{target}
   (progn (cat %{opam})
    (echo
     "url { src: \"git://git@gitlab.routine.co:routine/schematic-http#%{version:schematic-http}\" }")))))
(rule (deps (universe)) (target schematic-http.opam.locked)
 (action (run %{bin:opam} lock schematic-http)))
(rule (alias extdeps) (mode (promote (until-clean)))
 (target schematic-http.opam.extdeps)
 (action
  (with-stdout-to %{target}
   (run %{dep:.logistic/dune/extdeps/extdeps.exe} rewrite --input
    %{dep:schematic-http.opam.locked} --exclude
    schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,bake))))
(rule (target schematic-jsoo.%{version:schematic-jsoo}.opam)
 (deps (:opam schematic-jsoo.opam))
 (action
  (with-stdout-to %{target}
   (progn (cat %{opam})
    (echo
     "url { src: \"git://git@gitlab.routine.co:routine/schematic-jsoo#%{version:schematic-jsoo}\" }")))))
(rule (deps (universe)) (target schematic-jsoo.opam.locked)
 (action (run %{bin:opam} lock schematic-jsoo)))
(rule (alias extdeps) (mode (promote (until-clean)))
 (target schematic-jsoo.opam.extdeps)
 (action
  (with-stdout-to %{target}
   (run %{dep:.logistic/dune/extdeps/extdeps.exe} rewrite --input
    %{dep:schematic-jsoo.opam.locked} --exclude
    schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,bake))))
(rule (target schematic-sexp.%{version:schematic-sexp}.opam)
 (deps (:opam schematic-sexp.opam))
 (action
  (with-stdout-to %{target}
   (progn (cat %{opam})
    (echo
     "url { src: \"git://git@gitlab.routine.co:routine/schematic-sexp#%{version:schematic-sexp}\" }")))))
(rule (deps (universe)) (target schematic-sexp.opam.locked)
 (action (run %{bin:opam} lock schematic-sexp)))
(rule (alias extdeps) (mode (promote (until-clean)))
 (target schematic-sexp.opam.extdeps)
 (action
  (with-stdout-to %{target}
   (run %{dep:.logistic/dune/extdeps/extdeps.exe} rewrite --input
    %{dep:schematic-sexp.opam.locked} --exclude
    schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,bake))))
(rule (target schematic-sqml.%{version:schematic-sqml}.opam)
 (deps (:opam schematic-sqml.opam))
 (action
  (with-stdout-to %{target}
   (progn (cat %{opam})
    (echo
     "url { src: \"git://git@gitlab.routine.co:routine/schematic-sqml#%{version:schematic-sqml}\" }")))))
(rule (deps (universe)) (target schematic-sqml.opam.locked)
 (action (run %{bin:opam} lock schematic-sqml)))
(rule (alias extdeps) (mode (promote (until-clean)))
 (target schematic-sqml.opam.extdeps)
 (action
  (with-stdout-to %{target}
   (run %{dep:.logistic/dune/extdeps/extdeps.exe} rewrite --input
    %{dep:schematic-sqml.opam.locked} --exclude
    schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,bake))))
