(rule (target entropic.%{version:entropic}.opam) (deps (:opam entropic.opam))
 (action
  (with-stdout-to %{target}
   (progn (cat %{opam})
    (echo
     "url { src: \"git://git@gitlab.routine.co:routine/schematic#%{version:entropic}\" }")))))
(rule (target entropic-ios.opam)
 (action
  (with-stdout-to %{target}
   (run %{dep:.logistic/dune/extdeps/extdeps.exe} rewrite-ios --input
    %{dep:entropic.opam} --cross
    entropic,schematic,schematic-cli,schematic-cohttp-eio,schematic-cohttp-lwt,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,bake,hlist,metaversion,nofuture,nofuture-eio,nofuture-lwt,sqml,timmy,timmy-lwt,timmy-unix,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri
    --cross-both
    bake,fmt,landmarks-ppx,ppx_deriving,ppx_hash,ppx_here,semver2
    --cross-exclude ""))))
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
    %{dep:entropic.opam.locked} --local
    entropic,schematic,schematic-cli,schematic-cohttp-eio,schematic-cohttp-lwt,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,bake,hlist,metaversion,nofuture,nofuture-eio,nofuture-lwt,sqml,timmy,timmy-lwt,timmy-unix))))
(rule (target schematic.%{version:schematic}.opam)
 (deps (:opam schematic.opam))
 (action
  (with-stdout-to %{target}
   (progn (cat %{opam})
    (echo
     "url { src: \"git://git@gitlab.routine.co:routine/schematic#%{version:schematic}\" }")))))
(rule (target schematic-ios.opam)
 (action
  (with-stdout-to %{target}
   (run %{dep:.logistic/dune/extdeps/extdeps.exe} rewrite-ios --input
    %{dep:schematic.opam} --cross
    entropic,schematic,schematic-cli,schematic-cohttp-eio,schematic-cohttp-lwt,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,bake,hlist,metaversion,nofuture,nofuture-eio,nofuture-lwt,sqml,timmy,timmy-lwt,timmy-unix,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri
    --cross-both
    bake,fmt,landmarks-ppx,ppx_deriving,ppx_hash,ppx_here,semver2
    --cross-exclude ""))))
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
    %{dep:schematic.opam.locked} --local
    entropic,schematic,schematic-cli,schematic-cohttp-eio,schematic-cohttp-lwt,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,bake,hlist,metaversion,nofuture,nofuture-eio,nofuture-lwt,sqml,timmy,timmy-lwt,timmy-unix))))
(rule (target schematic-cli.%{version:schematic-cli}.opam)
 (deps (:opam schematic-cli.opam))
 (action
  (with-stdout-to %{target}
   (progn (cat %{opam})
    (echo
     "url { src: \"git://git@gitlab.routine.co:routine/schematic#%{version:schematic-cli}\" }")))))
(rule (target schematic-cli-ios.opam)
 (action
  (with-stdout-to %{target}
   (run %{dep:.logistic/dune/extdeps/extdeps.exe} rewrite-ios --input
    %{dep:schematic-cli.opam} --cross
    entropic,schematic,schematic-cli,schematic-cohttp-eio,schematic-cohttp-lwt,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,bake,hlist,metaversion,nofuture,nofuture-eio,nofuture-lwt,sqml,timmy,timmy-lwt,timmy-unix,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri
    --cross-both
    bake,fmt,landmarks-ppx,ppx_deriving,ppx_hash,ppx_here,semver2
    --cross-exclude ""))))
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
    %{dep:schematic-cli.opam.locked} --local
    entropic,schematic,schematic-cli,schematic-cohttp-eio,schematic-cohttp-lwt,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,bake,hlist,metaversion,nofuture,nofuture-eio,nofuture-lwt,sqml,timmy,timmy-lwt,timmy-unix))))
(rule (target schematic-cohttp-eio.%{version:schematic-cohttp-eio}.opam)
 (deps (:opam schematic-cohttp-eio.opam))
 (action
  (with-stdout-to %{target}
   (progn (cat %{opam})
    (echo
     "url { src: \"git://git@gitlab.routine.co:routine/schematic#%{version:schematic-cohttp-eio}\" }")))))
(rule (target schematic-cohttp-eio-ios.opam)
 (action
  (with-stdout-to %{target}
   (run %{dep:.logistic/dune/extdeps/extdeps.exe} rewrite-ios --input
    %{dep:schematic-cohttp-eio.opam} --cross
    entropic,schematic,schematic-cli,schematic-cohttp-eio,schematic-cohttp-lwt,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,bake,hlist,metaversion,nofuture,nofuture-eio,nofuture-lwt,sqml,timmy,timmy-lwt,timmy-unix,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri
    --cross-both
    bake,fmt,landmarks-ppx,ppx_deriving,ppx_hash,ppx_here,semver2
    --cross-exclude ""))))
(rule (target schematic-cohttp-eio-ios.%{version:schematic-cohttp-eio}.opam)
 (deps (:opam schematic-cohttp-eio-ios.opam))
 (action
  (with-stdout-to %{target}
   (progn (cat %{opam})
    (echo
     "url { src: \"git://git@gitlab.routine.co:routine/schematic#%{version:schematic-cohttp-eio}\" }")))))
(rule (deps (universe)) (target schematic-cohttp-eio.opam.locked)
 (action (run %{bin:opam} lock schematic-cohttp-eio)))
(rule (alias extdeps) (mode (promote (until-clean)))
 (target schematic-cohttp-eio.opam.extdeps)
 (action
  (with-stdout-to %{target}
   (run %{dep:.logistic/dune/extdeps/extdeps.exe} rewrite --input
    %{dep:schematic-cohttp-eio.opam.locked} --local
    entropic,schematic,schematic-cli,schematic-cohttp-eio,schematic-cohttp-lwt,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,bake,hlist,metaversion,nofuture,nofuture-eio,nofuture-lwt,sqml,timmy,timmy-lwt,timmy-unix))))
(rule (target schematic-cohttp-lwt.%{version:schematic-cohttp-lwt}.opam)
 (deps (:opam schematic-cohttp-lwt.opam))
 (action
  (with-stdout-to %{target}
   (progn (cat %{opam})
    (echo
     "url { src: \"git://git@gitlab.routine.co:routine/schematic#%{version:schematic-cohttp-lwt}\" }")))))
(rule (target schematic-cohttp-lwt-ios.opam)
 (action
  (with-stdout-to %{target}
   (run %{dep:.logistic/dune/extdeps/extdeps.exe} rewrite-ios --input
    %{dep:schematic-cohttp-lwt.opam} --cross
    entropic,schematic,schematic-cli,schematic-cohttp-eio,schematic-cohttp-lwt,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,bake,hlist,metaversion,nofuture,nofuture-eio,nofuture-lwt,sqml,timmy,timmy-lwt,timmy-unix,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri
    --cross-both
    bake,fmt,landmarks-ppx,ppx_deriving,ppx_hash,ppx_here,semver2
    --cross-exclude ""))))
(rule (target schematic-cohttp-lwt-ios.%{version:schematic-cohttp-lwt}.opam)
 (deps (:opam schematic-cohttp-lwt-ios.opam))
 (action
  (with-stdout-to %{target}
   (progn (cat %{opam})
    (echo
     "url { src: \"git://git@gitlab.routine.co:routine/schematic#%{version:schematic-cohttp-lwt}\" }")))))
(rule (deps (universe)) (target schematic-cohttp-lwt.opam.locked)
 (action (run %{bin:opam} lock schematic-cohttp-lwt)))
(rule (alias extdeps) (mode (promote (until-clean)))
 (target schematic-cohttp-lwt.opam.extdeps)
 (action
  (with-stdout-to %{target}
   (run %{dep:.logistic/dune/extdeps/extdeps.exe} rewrite --input
    %{dep:schematic-cohttp-lwt.opam.locked} --local
    entropic,schematic,schematic-cli,schematic-cohttp-eio,schematic-cohttp-lwt,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,bake,hlist,metaversion,nofuture,nofuture-eio,nofuture-lwt,sqml,timmy,timmy-lwt,timmy-unix))))
(rule (target schematic-ffi.%{version:schematic-ffi}.opam)
 (deps (:opam schematic-ffi.opam))
 (action
  (with-stdout-to %{target}
   (progn (cat %{opam})
    (echo
     "url { src: \"git://git@gitlab.routine.co:routine/schematic#%{version:schematic-ffi}\" }")))))
(rule (target schematic-ffi-ios.opam)
 (action
  (with-stdout-to %{target}
   (run %{dep:.logistic/dune/extdeps/extdeps.exe} rewrite-ios --input
    %{dep:schematic-ffi.opam} --cross
    entropic,schematic,schematic-cli,schematic-cohttp-eio,schematic-cohttp-lwt,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,bake,hlist,metaversion,nofuture,nofuture-eio,nofuture-lwt,sqml,timmy,timmy-lwt,timmy-unix,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri
    --cross-both
    bake,fmt,landmarks-ppx,ppx_deriving,ppx_hash,ppx_here,semver2
    --cross-exclude ""))))
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
    %{dep:schematic-ffi.opam.locked} --local
    entropic,schematic,schematic-cli,schematic-cohttp-eio,schematic-cohttp-lwt,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,bake,hlist,metaversion,nofuture,nofuture-eio,nofuture-lwt,sqml,timmy,timmy-lwt,timmy-unix))))
(rule (target schematic-http.%{version:schematic-http}.opam)
 (deps (:opam schematic-http.opam))
 (action
  (with-stdout-to %{target}
   (progn (cat %{opam})
    (echo
     "url { src: \"git://git@gitlab.routine.co:routine/schematic#%{version:schematic-http}\" }")))))
(rule (target schematic-http-ios.opam)
 (action
  (with-stdout-to %{target}
   (run %{dep:.logistic/dune/extdeps/extdeps.exe} rewrite-ios --input
    %{dep:schematic-http.opam} --cross
    entropic,schematic,schematic-cli,schematic-cohttp-eio,schematic-cohttp-lwt,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,bake,hlist,metaversion,nofuture,nofuture-eio,nofuture-lwt,sqml,timmy,timmy-lwt,timmy-unix,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri
    --cross-both
    bake,fmt,landmarks-ppx,ppx_deriving,ppx_hash,ppx_here,semver2
    --cross-exclude ""))))
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
    %{dep:schematic-http.opam.locked} --local
    entropic,schematic,schematic-cli,schematic-cohttp-eio,schematic-cohttp-lwt,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,bake,hlist,metaversion,nofuture,nofuture-eio,nofuture-lwt,sqml,timmy,timmy-lwt,timmy-unix))))
(rule (target schematic-jsoo.%{version:schematic-jsoo}.opam)
 (deps (:opam schematic-jsoo.opam))
 (action
  (with-stdout-to %{target}
   (progn (cat %{opam})
    (echo
     "url { src: \"git://git@gitlab.routine.co:routine/schematic#%{version:schematic-jsoo}\" }")))))
(rule (target schematic-jsoo-ios.opam)
 (action
  (with-stdout-to %{target}
   (run %{dep:.logistic/dune/extdeps/extdeps.exe} rewrite-ios --input
    %{dep:schematic-jsoo.opam} --cross
    entropic,schematic,schematic-cli,schematic-cohttp-eio,schematic-cohttp-lwt,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,bake,hlist,metaversion,nofuture,nofuture-eio,nofuture-lwt,sqml,timmy,timmy-lwt,timmy-unix,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri
    --cross-both
    bake,fmt,landmarks-ppx,ppx_deriving,ppx_hash,ppx_here,semver2
    --cross-exclude ""))))
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
    %{dep:schematic-jsoo.opam.locked} --local
    entropic,schematic,schematic-cli,schematic-cohttp-eio,schematic-cohttp-lwt,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,bake,hlist,metaversion,nofuture,nofuture-eio,nofuture-lwt,sqml,timmy,timmy-lwt,timmy-unix))))
(rule (target schematic-sexp.%{version:schematic-sexp}.opam)
 (deps (:opam schematic-sexp.opam))
 (action
  (with-stdout-to %{target}
   (progn (cat %{opam})
    (echo
     "url { src: \"git://git@gitlab.routine.co:routine/schematic#%{version:schematic-sexp}\" }")))))
(rule (target schematic-sexp-ios.opam)
 (action
  (with-stdout-to %{target}
   (run %{dep:.logistic/dune/extdeps/extdeps.exe} rewrite-ios --input
    %{dep:schematic-sexp.opam} --cross
    entropic,schematic,schematic-cli,schematic-cohttp-eio,schematic-cohttp-lwt,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,bake,hlist,metaversion,nofuture,nofuture-eio,nofuture-lwt,sqml,timmy,timmy-lwt,timmy-unix,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri
    --cross-both
    bake,fmt,landmarks-ppx,ppx_deriving,ppx_hash,ppx_here,semver2
    --cross-exclude ""))))
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
    %{dep:schematic-sexp.opam.locked} --local
    entropic,schematic,schematic-cli,schematic-cohttp-eio,schematic-cohttp-lwt,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,bake,hlist,metaversion,nofuture,nofuture-eio,nofuture-lwt,sqml,timmy,timmy-lwt,timmy-unix))))
(rule (target schematic-sqml.%{version:schematic-sqml}.opam)
 (deps (:opam schematic-sqml.opam))
 (action
  (with-stdout-to %{target}
   (progn (cat %{opam})
    (echo
     "url { src: \"git://git@gitlab.routine.co:routine/schematic#%{version:schematic-sqml}\" }")))))
(rule (target schematic-sqml-ios.opam)
 (action
  (with-stdout-to %{target}
   (run %{dep:.logistic/dune/extdeps/extdeps.exe} rewrite-ios --input
    %{dep:schematic-sqml.opam} --cross
    entropic,schematic,schematic-cli,schematic-cohttp-eio,schematic-cohttp-lwt,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,bake,hlist,metaversion,nofuture,nofuture-eio,nofuture-lwt,sqml,timmy,timmy-lwt,timmy-unix,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri
    --cross-both
    bake,fmt,landmarks-ppx,ppx_deriving,ppx_hash,ppx_here,semver2
    --cross-exclude ""))))
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
    %{dep:schematic-sqml.opam.locked} --local
    entropic,schematic,schematic-cli,schematic-cohttp-eio,schematic-cohttp-lwt,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,bake,hlist,metaversion,nofuture,nofuture-eio,nofuture-lwt,sqml,timmy,timmy-lwt,timmy-unix))))
