(rule (target entropic.%{version:entropic}.opam) (deps (:opam entropic.opam))
 (action
  (with-stdout-to %{target}
   (progn (cat %{opam})
    (echo
     "url { src: \"git://git@gitlab.routine.co:routine/schematic#%{version:entropic}\" }")))))
(rule (alias default) (target entropic-ios.opam)
 (action
  (with-stdout-to %{target}
   (run %{dep:.logistic/dune/extdeps/extdeps.exe} rewrite-ios --input
    %{dep:entropic.opam} --both
    bake,fmt,ppx_deriving,ppx_hash,ppx_here,semver2 --cross
    entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,bake,hlist,sqml,timmy,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri))))
(rule (target entropic-ios.%{version:entropic}.opam)
 (deps (:opam entropic-ios.opam))
 (action
  (with-stdout-to %{target}
   (progn (cat %{opam})
    (echo
     "url { src: \"git://git@gitlab.routine.co:routine/schematic#%{version:entropic}\" }")))))
(rule (deps (universe)) (target entropic.opam.locked)
 (action (run %{bin:opam} lock entropic)))
(rule (alias extdeps) (mode (promote (until-clean)))
 (target entropic.opam.extdeps)
 (action
  (with-stdout-to %{target}
   (run %{dep:.logistic/dune/extdeps/extdeps.exe} rewrite --input
    %{dep:entropic.opam.locked} --exclude
    entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,bake,hlist,sqml,timmy))))
(rule (target schematic.%{version:schematic}.opam)
 (deps (:opam schematic.opam))
 (action
  (with-stdout-to %{target}
   (progn (cat %{opam})
    (echo
     "url { src: \"git://git@gitlab.routine.co:routine/schematic#%{version:schematic}\" }")))))
(rule (alias default) (target schematic-ios.opam)
 (action
  (with-stdout-to %{target}
   (run %{dep:.logistic/dune/extdeps/extdeps.exe} rewrite-ios --input
    %{dep:schematic.opam} --both
    bake,fmt,ppx_deriving,ppx_hash,ppx_here,semver2 --cross
    entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,bake,hlist,sqml,timmy,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri))))
(rule (target schematic-ios.%{version:schematic}.opam)
 (deps (:opam schematic-ios.opam))
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
    entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,bake,hlist,sqml,timmy))))
(rule (target schematic-cli.%{version:schematic-cli}.opam)
 (deps (:opam schematic-cli.opam))
 (action
  (with-stdout-to %{target}
   (progn (cat %{opam})
    (echo
     "url { src: \"git://git@gitlab.routine.co:routine/schematic#%{version:schematic-cli}\" }")))))
(rule (alias default) (target schematic-cli-ios.opam)
 (action
  (with-stdout-to %{target}
   (run %{dep:.logistic/dune/extdeps/extdeps.exe} rewrite-ios --input
    %{dep:schematic-cli.opam} --both
    bake,fmt,ppx_deriving,ppx_hash,ppx_here,semver2 --cross
    entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,bake,hlist,sqml,timmy,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri))))
(rule (target schematic-cli-ios.%{version:schematic-cli}.opam)
 (deps (:opam schematic-cli-ios.opam))
 (action
  (with-stdout-to %{target}
   (progn (cat %{opam})
    (echo
     "url { src: \"git://git@gitlab.routine.co:routine/schematic#%{version:schematic-cli}\" }")))))
(rule (deps (universe)) (target schematic-cli.opam.locked)
 (action (run %{bin:opam} lock schematic-cli)))
(rule (alias extdeps) (mode (promote (until-clean)))
 (target schematic-cli.opam.extdeps)
 (action
  (with-stdout-to %{target}
   (run %{dep:.logistic/dune/extdeps/extdeps.exe} rewrite --input
    %{dep:schematic-cli.opam.locked} --exclude
    entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,bake,hlist,sqml,timmy))))
(rule (target schematic-ffi.%{version:schematic-ffi}.opam)
 (deps (:opam schematic-ffi.opam))
 (action
  (with-stdout-to %{target}
   (progn (cat %{opam})
    (echo
     "url { src: \"git://git@gitlab.routine.co:routine/schematic#%{version:schematic-ffi}\" }")))))
(rule (alias default) (target schematic-ffi-ios.opam)
 (action
  (with-stdout-to %{target}
   (run %{dep:.logistic/dune/extdeps/extdeps.exe} rewrite-ios --input
    %{dep:schematic-ffi.opam} --both
    bake,fmt,ppx_deriving,ppx_hash,ppx_here,semver2 --cross
    entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,bake,hlist,sqml,timmy,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri))))
(rule (target schematic-ffi-ios.%{version:schematic-ffi}.opam)
 (deps (:opam schematic-ffi-ios.opam))
 (action
  (with-stdout-to %{target}
   (progn (cat %{opam})
    (echo
     "url { src: \"git://git@gitlab.routine.co:routine/schematic#%{version:schematic-ffi}\" }")))))
(rule (deps (universe)) (target schematic-ffi.opam.locked)
 (action (run %{bin:opam} lock schematic-ffi)))
(rule (alias extdeps) (mode (promote (until-clean)))
 (target schematic-ffi.opam.extdeps)
 (action
  (with-stdout-to %{target}
   (run %{dep:.logistic/dune/extdeps/extdeps.exe} rewrite --input
    %{dep:schematic-ffi.opam.locked} --exclude
    entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,bake,hlist,sqml,timmy))))
(rule (target schematic-http.%{version:schematic-http}.opam)
 (deps (:opam schematic-http.opam))
 (action
  (with-stdout-to %{target}
   (progn (cat %{opam})
    (echo
     "url { src: \"git://git@gitlab.routine.co:routine/schematic#%{version:schematic-http}\" }")))))
(rule (alias default) (target schematic-http-ios.opam)
 (action
  (with-stdout-to %{target}
   (run %{dep:.logistic/dune/extdeps/extdeps.exe} rewrite-ios --input
    %{dep:schematic-http.opam} --both
    bake,fmt,ppx_deriving,ppx_hash,ppx_here,semver2 --cross
    entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,bake,hlist,sqml,timmy,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri))))
(rule (target schematic-http-ios.%{version:schematic-http}.opam)
 (deps (:opam schematic-http-ios.opam))
 (action
  (with-stdout-to %{target}
   (progn (cat %{opam})
    (echo
     "url { src: \"git://git@gitlab.routine.co:routine/schematic#%{version:schematic-http}\" }")))))
(rule (deps (universe)) (target schematic-http.opam.locked)
 (action (run %{bin:opam} lock schematic-http)))
(rule (alias extdeps) (mode (promote (until-clean)))
 (target schematic-http.opam.extdeps)
 (action
  (with-stdout-to %{target}
   (run %{dep:.logistic/dune/extdeps/extdeps.exe} rewrite --input
    %{dep:schematic-http.opam.locked} --exclude
    entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,bake,hlist,sqml,timmy))))
(rule (target schematic-jsoo.%{version:schematic-jsoo}.opam)
 (deps (:opam schematic-jsoo.opam))
 (action
  (with-stdout-to %{target}
   (progn (cat %{opam})
    (echo
     "url { src: \"git://git@gitlab.routine.co:routine/schematic#%{version:schematic-jsoo}\" }")))))
(rule (alias default) (target schematic-jsoo-ios.opam)
 (action
  (with-stdout-to %{target}
   (run %{dep:.logistic/dune/extdeps/extdeps.exe} rewrite-ios --input
    %{dep:schematic-jsoo.opam} --both
    bake,fmt,ppx_deriving,ppx_hash,ppx_here,semver2 --cross
    entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,bake,hlist,sqml,timmy,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri))))
(rule (target schematic-jsoo-ios.%{version:schematic-jsoo}.opam)
 (deps (:opam schematic-jsoo-ios.opam))
 (action
  (with-stdout-to %{target}
   (progn (cat %{opam})
    (echo
     "url { src: \"git://git@gitlab.routine.co:routine/schematic#%{version:schematic-jsoo}\" }")))))
(rule (deps (universe)) (target schematic-jsoo.opam.locked)
 (action (run %{bin:opam} lock schematic-jsoo)))
(rule (alias extdeps) (mode (promote (until-clean)))
 (target schematic-jsoo.opam.extdeps)
 (action
  (with-stdout-to %{target}
   (run %{dep:.logistic/dune/extdeps/extdeps.exe} rewrite --input
    %{dep:schematic-jsoo.opam.locked} --exclude
    entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,bake,hlist,sqml,timmy))))
(rule (target schematic-sexp.%{version:schematic-sexp}.opam)
 (deps (:opam schematic-sexp.opam))
 (action
  (with-stdout-to %{target}
   (progn (cat %{opam})
    (echo
     "url { src: \"git://git@gitlab.routine.co:routine/schematic#%{version:schematic-sexp}\" }")))))
(rule (alias default) (target schematic-sexp-ios.opam)
 (action
  (with-stdout-to %{target}
   (run %{dep:.logistic/dune/extdeps/extdeps.exe} rewrite-ios --input
    %{dep:schematic-sexp.opam} --both
    bake,fmt,ppx_deriving,ppx_hash,ppx_here,semver2 --cross
    entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,bake,hlist,sqml,timmy,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri))))
(rule (target schematic-sexp-ios.%{version:schematic-sexp}.opam)
 (deps (:opam schematic-sexp-ios.opam))
 (action
  (with-stdout-to %{target}
   (progn (cat %{opam})
    (echo
     "url { src: \"git://git@gitlab.routine.co:routine/schematic#%{version:schematic-sexp}\" }")))))
(rule (deps (universe)) (target schematic-sexp.opam.locked)
 (action (run %{bin:opam} lock schematic-sexp)))
(rule (alias extdeps) (mode (promote (until-clean)))
 (target schematic-sexp.opam.extdeps)
 (action
  (with-stdout-to %{target}
   (run %{dep:.logistic/dune/extdeps/extdeps.exe} rewrite --input
    %{dep:schematic-sexp.opam.locked} --exclude
    entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,bake,hlist,sqml,timmy))))
(rule (target schematic-sqml.%{version:schematic-sqml}.opam)
 (deps (:opam schematic-sqml.opam))
 (action
  (with-stdout-to %{target}
   (progn (cat %{opam})
    (echo
     "url { src: \"git://git@gitlab.routine.co:routine/schematic#%{version:schematic-sqml}\" }")))))
(rule (alias default) (target schematic-sqml-ios.opam)
 (action
  (with-stdout-to %{target}
   (run %{dep:.logistic/dune/extdeps/extdeps.exe} rewrite-ios --input
    %{dep:schematic-sqml.opam} --both
    bake,fmt,ppx_deriving,ppx_hash,ppx_here,semver2 --cross
    entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,bake,hlist,sqml,timmy,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri))))
(rule (target schematic-sqml-ios.%{version:schematic-sqml}.opam)
 (deps (:opam schematic-sqml-ios.opam))
 (action
  (with-stdout-to %{target}
   (progn (cat %{opam})
    (echo
     "url { src: \"git://git@gitlab.routine.co:routine/schematic#%{version:schematic-sqml}\" }")))))
(rule (deps (universe)) (target schematic-sqml.opam.locked)
 (action (run %{bin:opam} lock schematic-sqml)))
(rule (alias extdeps) (mode (promote (until-clean)))
 (target schematic-sqml.opam.extdeps)
 (action
  (with-stdout-to %{target}
   (run %{dep:.logistic/dune/extdeps/extdeps.exe} rewrite --input
    %{dep:schematic-sqml.opam.locked} --exclude
    entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,bake,hlist,sqml,timmy))))
