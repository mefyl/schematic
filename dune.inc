(alias (name extdeps) (deps opam/entropic.opam.extdeps))
(subdir opam
 (rule (target entropic.%{version:entropic}.opam)
  (deps (:opam ../entropic.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/schematic#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target entropic.opam.locked)
  (action (run %{bin:opam} lock entropic)))
 (rule (mode (promote (until-clean))) (target entropic.opam.extdeps)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:entropic.opam.locked} --local
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt))))
 (rule (target entropic-ios.opam) (mode (promote (until-clean)))
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite-ios --input
     %{dep:../entropic.opam} --cross
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri
     --cross-both
     bake,fmt,landmarks-ppx,ppx_deriving,ppx_hash,ppx_here,semver2
     --cross-exclude ""))))
 (rule (target entropic-ios.%{version:entropic}.opam)
  (deps (:opam entropic-ios.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/schematic#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target entropic-ios.opam.locked)
  (action (run %{bin:opam} lock entropic-ios)))
 (rule (mode (promote (until-clean))) (target entropic.opam.extdeps-ios)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:entropic-ios.opam.locked} --local
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt
     --toolchain ios))))
 (rule (target entropic-macos.opam) (mode (promote (until-clean)))
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite-macos --input
     %{dep:../entropic.opam} --cross
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri
     --cross-both
     bake,fmt,landmarks-ppx,ppx_deriving,ppx_hash,ppx_here,semver2
     --cross-exclude ""))))
 (rule (target entropic-macos.%{version:entropic}.opam)
  (deps (:opam entropic-macos.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/schematic#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target entropic-macos.opam.locked)
  (action (run %{bin:opam} lock entropic-macos)))
 (rule (mode (promote (until-clean))) (target entropic.opam.extdeps-macos)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:entropic-macos.opam.locked} --local
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt
     --toolchain macos))))
 (rule (target entropic-windows.opam) (mode (promote (until-clean)))
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite-windows --input
     %{dep:../entropic.opam} --cross
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri
     --cross-both
     bake,fmt,landmarks-ppx,ppx_deriving,ppx_hash,ppx_here,semver2
     --cross-exclude ""))))
 (rule (target entropic-windows.%{version:entropic}.opam)
  (deps (:opam entropic-windows.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/schematic#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target entropic-windows.opam.locked)
  (action (run %{bin:opam} lock entropic-windows)))
 (rule (mode (promote (until-clean))) (target entropic.opam.extdeps-windows)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:entropic-windows.opam.locked} --local
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt
     --toolchain windows))))
 (rule (target entropic-android.opam) (mode (promote (until-clean)))
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite-android --input
     %{dep:../entropic.opam} --cross
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri
     --cross-both
     bake,fmt,landmarks-ppx,ppx_deriving,ppx_hash,ppx_here,semver2
     --cross-exclude ""))))
 (rule (target entropic-android.%{version:entropic}.opam)
  (deps (:opam entropic-android.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/schematic#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target entropic-android.opam.locked)
  (action (run %{bin:opam} lock entropic-android)))
 (rule (mode (promote (until-clean))) (target entropic.opam.extdeps-android)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:entropic-android.opam.locked} --local
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt
     --toolchain android)))))
(alias (name extdeps) (deps opam/schematic.opam.extdeps))
(subdir opam
 (rule (target schematic.%{version:schematic}.opam)
  (deps (:opam ../schematic.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/schematic#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target schematic.opam.locked)
  (action (run %{bin:opam} lock schematic)))
 (rule (mode (promote (until-clean))) (target schematic.opam.extdeps)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:schematic.opam.locked} --local
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt))))
 (rule (target schematic-ios.opam) (mode (promote (until-clean)))
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite-ios --input
     %{dep:../schematic.opam} --cross
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri
     --cross-both
     bake,fmt,landmarks-ppx,ppx_deriving,ppx_hash,ppx_here,semver2
     --cross-exclude ""))))
 (rule (target schematic-ios.%{version:schematic}.opam)
  (deps (:opam schematic-ios.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/schematic#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target schematic-ios.opam.locked)
  (action (run %{bin:opam} lock schematic-ios)))
 (rule (mode (promote (until-clean))) (target schematic.opam.extdeps-ios)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:schematic-ios.opam.locked} --local
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt
     --toolchain ios))))
 (rule (target schematic-macos.opam) (mode (promote (until-clean)))
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite-macos --input
     %{dep:../schematic.opam} --cross
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri
     --cross-both
     bake,fmt,landmarks-ppx,ppx_deriving,ppx_hash,ppx_here,semver2
     --cross-exclude ""))))
 (rule (target schematic-macos.%{version:schematic}.opam)
  (deps (:opam schematic-macos.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/schematic#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target schematic-macos.opam.locked)
  (action (run %{bin:opam} lock schematic-macos)))
 (rule (mode (promote (until-clean))) (target schematic.opam.extdeps-macos)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:schematic-macos.opam.locked} --local
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt
     --toolchain macos))))
 (rule (target schematic-windows.opam) (mode (promote (until-clean)))
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite-windows --input
     %{dep:../schematic.opam} --cross
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri
     --cross-both
     bake,fmt,landmarks-ppx,ppx_deriving,ppx_hash,ppx_here,semver2
     --cross-exclude ""))))
 (rule (target schematic-windows.%{version:schematic}.opam)
  (deps (:opam schematic-windows.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/schematic#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target schematic-windows.opam.locked)
  (action (run %{bin:opam} lock schematic-windows)))
 (rule (mode (promote (until-clean))) (target schematic.opam.extdeps-windows)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:schematic-windows.opam.locked} --local
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt
     --toolchain windows))))
 (rule (target schematic-android.opam) (mode (promote (until-clean)))
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite-android --input
     %{dep:../schematic.opam} --cross
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri
     --cross-both
     bake,fmt,landmarks-ppx,ppx_deriving,ppx_hash,ppx_here,semver2
     --cross-exclude ""))))
 (rule (target schematic-android.%{version:schematic}.opam)
  (deps (:opam schematic-android.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/schematic#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target schematic-android.opam.locked)
  (action (run %{bin:opam} lock schematic-android)))
 (rule (mode (promote (until-clean))) (target schematic.opam.extdeps-android)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:schematic-android.opam.locked} --local
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt
     --toolchain android)))))
(alias (name extdeps) (deps opam/schematic-cli.opam.extdeps))
(subdir opam
 (rule (target schematic-cli.%{version:schematic-cli}.opam)
  (deps (:opam ../schematic-cli.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/schematic#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target schematic-cli.opam.locked)
  (action (run %{bin:opam} lock schematic-cli)))
 (rule (mode (promote (until-clean))) (target schematic-cli.opam.extdeps)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:schematic-cli.opam.locked} --local
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt))))
 (rule (target schematic-cli-ios.opam) (mode (promote (until-clean)))
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite-ios --input
     %{dep:../schematic-cli.opam} --cross
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri
     --cross-both
     bake,fmt,landmarks-ppx,ppx_deriving,ppx_hash,ppx_here,semver2
     --cross-exclude ""))))
 (rule (target schematic-cli-ios.%{version:schematic-cli}.opam)
  (deps (:opam schematic-cli-ios.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/schematic#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target schematic-cli-ios.opam.locked)
  (action (run %{bin:opam} lock schematic-cli-ios)))
 (rule (mode (promote (until-clean))) (target schematic-cli.opam.extdeps-ios)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:schematic-cli-ios.opam.locked} --local
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt
     --toolchain ios))))
 (rule (target schematic-cli-macos.opam) (mode (promote (until-clean)))
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite-macos --input
     %{dep:../schematic-cli.opam} --cross
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri
     --cross-both
     bake,fmt,landmarks-ppx,ppx_deriving,ppx_hash,ppx_here,semver2
     --cross-exclude ""))))
 (rule (target schematic-cli-macos.%{version:schematic-cli}.opam)
  (deps (:opam schematic-cli-macos.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/schematic#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target schematic-cli-macos.opam.locked)
  (action (run %{bin:opam} lock schematic-cli-macos)))
 (rule (mode (promote (until-clean)))
  (target schematic-cli.opam.extdeps-macos)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:schematic-cli-macos.opam.locked} --local
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt
     --toolchain macos))))
 (rule (target schematic-cli-windows.opam) (mode (promote (until-clean)))
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite-windows --input
     %{dep:../schematic-cli.opam} --cross
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri
     --cross-both
     bake,fmt,landmarks-ppx,ppx_deriving,ppx_hash,ppx_here,semver2
     --cross-exclude ""))))
 (rule (target schematic-cli-windows.%{version:schematic-cli}.opam)
  (deps (:opam schematic-cli-windows.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/schematic#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target schematic-cli-windows.opam.locked)
  (action (run %{bin:opam} lock schematic-cli-windows)))
 (rule (mode (promote (until-clean)))
  (target schematic-cli.opam.extdeps-windows)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:schematic-cli-windows.opam.locked} --local
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt
     --toolchain windows))))
 (rule (target schematic-cli-android.opam) (mode (promote (until-clean)))
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite-android --input
     %{dep:../schematic-cli.opam} --cross
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri
     --cross-both
     bake,fmt,landmarks-ppx,ppx_deriving,ppx_hash,ppx_here,semver2
     --cross-exclude ""))))
 (rule (target schematic-cli-android.%{version:schematic-cli}.opam)
  (deps (:opam schematic-cli-android.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/schematic#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target schematic-cli-android.opam.locked)
  (action (run %{bin:opam} lock schematic-cli-android)))
 (rule (mode (promote (until-clean)))
  (target schematic-cli.opam.extdeps-android)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:schematic-cli-android.opam.locked} --local
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt
     --toolchain android)))))
(alias (name extdeps) (deps opam/schematic-ffi.opam.extdeps))
(subdir opam
 (rule (target schematic-ffi.%{version:schematic-ffi}.opam)
  (deps (:opam ../schematic-ffi.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/schematic#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target schematic-ffi.opam.locked)
  (action (run %{bin:opam} lock schematic-ffi)))
 (rule (mode (promote (until-clean))) (target schematic-ffi.opam.extdeps)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:schematic-ffi.opam.locked} --local
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt))))
 (rule (target schematic-ffi-ios.opam) (mode (promote (until-clean)))
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite-ios --input
     %{dep:../schematic-ffi.opam} --cross
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri
     --cross-both
     bake,fmt,landmarks-ppx,ppx_deriving,ppx_hash,ppx_here,semver2
     --cross-exclude ""))))
 (rule (target schematic-ffi-ios.%{version:schematic-ffi}.opam)
  (deps (:opam schematic-ffi-ios.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/schematic#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target schematic-ffi-ios.opam.locked)
  (action (run %{bin:opam} lock schematic-ffi-ios)))
 (rule (mode (promote (until-clean))) (target schematic-ffi.opam.extdeps-ios)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:schematic-ffi-ios.opam.locked} --local
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt
     --toolchain ios))))
 (rule (target schematic-ffi-macos.opam) (mode (promote (until-clean)))
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite-macos --input
     %{dep:../schematic-ffi.opam} --cross
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri
     --cross-both
     bake,fmt,landmarks-ppx,ppx_deriving,ppx_hash,ppx_here,semver2
     --cross-exclude ""))))
 (rule (target schematic-ffi-macos.%{version:schematic-ffi}.opam)
  (deps (:opam schematic-ffi-macos.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/schematic#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target schematic-ffi-macos.opam.locked)
  (action (run %{bin:opam} lock schematic-ffi-macos)))
 (rule (mode (promote (until-clean)))
  (target schematic-ffi.opam.extdeps-macos)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:schematic-ffi-macos.opam.locked} --local
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt
     --toolchain macos))))
 (rule (target schematic-ffi-windows.opam) (mode (promote (until-clean)))
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite-windows --input
     %{dep:../schematic-ffi.opam} --cross
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri
     --cross-both
     bake,fmt,landmarks-ppx,ppx_deriving,ppx_hash,ppx_here,semver2
     --cross-exclude ""))))
 (rule (target schematic-ffi-windows.%{version:schematic-ffi}.opam)
  (deps (:opam schematic-ffi-windows.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/schematic#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target schematic-ffi-windows.opam.locked)
  (action (run %{bin:opam} lock schematic-ffi-windows)))
 (rule (mode (promote (until-clean)))
  (target schematic-ffi.opam.extdeps-windows)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:schematic-ffi-windows.opam.locked} --local
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt
     --toolchain windows))))
 (rule (target schematic-ffi-android.opam) (mode (promote (until-clean)))
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite-android --input
     %{dep:../schematic-ffi.opam} --cross
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri
     --cross-both
     bake,fmt,landmarks-ppx,ppx_deriving,ppx_hash,ppx_here,semver2
     --cross-exclude ""))))
 (rule (target schematic-ffi-android.%{version:schematic-ffi}.opam)
  (deps (:opam schematic-ffi-android.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/schematic#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target schematic-ffi-android.opam.locked)
  (action (run %{bin:opam} lock schematic-ffi-android)))
 (rule (mode (promote (until-clean)))
  (target schematic-ffi.opam.extdeps-android)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:schematic-ffi-android.opam.locked} --local
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt
     --toolchain android)))))
(alias (name extdeps) (deps opam/schematic-http.opam.extdeps))
(subdir opam
 (rule (target schematic-http.%{version:schematic-http}.opam)
  (deps (:opam ../schematic-http.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/schematic#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target schematic-http.opam.locked)
  (action (run %{bin:opam} lock schematic-http)))
 (rule (mode (promote (until-clean))) (target schematic-http.opam.extdeps)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:schematic-http.opam.locked} --local
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt))))
 (rule (target schematic-http-ios.opam) (mode (promote (until-clean)))
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite-ios --input
     %{dep:../schematic-http.opam} --cross
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri
     --cross-both
     bake,fmt,landmarks-ppx,ppx_deriving,ppx_hash,ppx_here,semver2
     --cross-exclude ""))))
 (rule (target schematic-http-ios.%{version:schematic-http}.opam)
  (deps (:opam schematic-http-ios.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/schematic#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target schematic-http-ios.opam.locked)
  (action (run %{bin:opam} lock schematic-http-ios)))
 (rule (mode (promote (until-clean)))
  (target schematic-http.opam.extdeps-ios)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:schematic-http-ios.opam.locked} --local
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt
     --toolchain ios))))
 (rule (target schematic-http-macos.opam) (mode (promote (until-clean)))
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite-macos --input
     %{dep:../schematic-http.opam} --cross
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri
     --cross-both
     bake,fmt,landmarks-ppx,ppx_deriving,ppx_hash,ppx_here,semver2
     --cross-exclude ""))))
 (rule (target schematic-http-macos.%{version:schematic-http}.opam)
  (deps (:opam schematic-http-macos.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/schematic#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target schematic-http-macos.opam.locked)
  (action (run %{bin:opam} lock schematic-http-macos)))
 (rule (mode (promote (until-clean)))
  (target schematic-http.opam.extdeps-macos)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:schematic-http-macos.opam.locked} --local
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt
     --toolchain macos))))
 (rule (target schematic-http-windows.opam) (mode (promote (until-clean)))
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite-windows --input
     %{dep:../schematic-http.opam} --cross
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri
     --cross-both
     bake,fmt,landmarks-ppx,ppx_deriving,ppx_hash,ppx_here,semver2
     --cross-exclude ""))))
 (rule (target schematic-http-windows.%{version:schematic-http}.opam)
  (deps (:opam schematic-http-windows.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/schematic#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target schematic-http-windows.opam.locked)
  (action (run %{bin:opam} lock schematic-http-windows)))
 (rule (mode (promote (until-clean)))
  (target schematic-http.opam.extdeps-windows)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:schematic-http-windows.opam.locked} --local
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt
     --toolchain windows))))
 (rule (target schematic-http-android.opam) (mode (promote (until-clean)))
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite-android --input
     %{dep:../schematic-http.opam} --cross
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri
     --cross-both
     bake,fmt,landmarks-ppx,ppx_deriving,ppx_hash,ppx_here,semver2
     --cross-exclude ""))))
 (rule (target schematic-http-android.%{version:schematic-http}.opam)
  (deps (:opam schematic-http-android.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/schematic#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target schematic-http-android.opam.locked)
  (action (run %{bin:opam} lock schematic-http-android)))
 (rule (mode (promote (until-clean)))
  (target schematic-http.opam.extdeps-android)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:schematic-http-android.opam.locked} --local
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt
     --toolchain android)))))
(alias (name extdeps) (deps opam/schematic-jsoo.opam.extdeps))
(subdir opam
 (rule (target schematic-jsoo.%{version:schematic-jsoo}.opam)
  (deps (:opam ../schematic-jsoo.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/schematic#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target schematic-jsoo.opam.locked)
  (action (run %{bin:opam} lock schematic-jsoo)))
 (rule (mode (promote (until-clean))) (target schematic-jsoo.opam.extdeps)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:schematic-jsoo.opam.locked} --local
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt))))
 (rule (target schematic-jsoo-ios.opam) (mode (promote (until-clean)))
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite-ios --input
     %{dep:../schematic-jsoo.opam} --cross
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri
     --cross-both
     bake,fmt,landmarks-ppx,ppx_deriving,ppx_hash,ppx_here,semver2
     --cross-exclude ""))))
 (rule (target schematic-jsoo-ios.%{version:schematic-jsoo}.opam)
  (deps (:opam schematic-jsoo-ios.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/schematic#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target schematic-jsoo-ios.opam.locked)
  (action (run %{bin:opam} lock schematic-jsoo-ios)))
 (rule (mode (promote (until-clean)))
  (target schematic-jsoo.opam.extdeps-ios)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:schematic-jsoo-ios.opam.locked} --local
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt
     --toolchain ios))))
 (rule (target schematic-jsoo-macos.opam) (mode (promote (until-clean)))
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite-macos --input
     %{dep:../schematic-jsoo.opam} --cross
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri
     --cross-both
     bake,fmt,landmarks-ppx,ppx_deriving,ppx_hash,ppx_here,semver2
     --cross-exclude ""))))
 (rule (target schematic-jsoo-macos.%{version:schematic-jsoo}.opam)
  (deps (:opam schematic-jsoo-macos.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/schematic#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target schematic-jsoo-macos.opam.locked)
  (action (run %{bin:opam} lock schematic-jsoo-macos)))
 (rule (mode (promote (until-clean)))
  (target schematic-jsoo.opam.extdeps-macos)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:schematic-jsoo-macos.opam.locked} --local
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt
     --toolchain macos))))
 (rule (target schematic-jsoo-windows.opam) (mode (promote (until-clean)))
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite-windows --input
     %{dep:../schematic-jsoo.opam} --cross
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri
     --cross-both
     bake,fmt,landmarks-ppx,ppx_deriving,ppx_hash,ppx_here,semver2
     --cross-exclude ""))))
 (rule (target schematic-jsoo-windows.%{version:schematic-jsoo}.opam)
  (deps (:opam schematic-jsoo-windows.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/schematic#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target schematic-jsoo-windows.opam.locked)
  (action (run %{bin:opam} lock schematic-jsoo-windows)))
 (rule (mode (promote (until-clean)))
  (target schematic-jsoo.opam.extdeps-windows)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:schematic-jsoo-windows.opam.locked} --local
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt
     --toolchain windows))))
 (rule (target schematic-jsoo-android.opam) (mode (promote (until-clean)))
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite-android --input
     %{dep:../schematic-jsoo.opam} --cross
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri
     --cross-both
     bake,fmt,landmarks-ppx,ppx_deriving,ppx_hash,ppx_here,semver2
     --cross-exclude ""))))
 (rule (target schematic-jsoo-android.%{version:schematic-jsoo}.opam)
  (deps (:opam schematic-jsoo-android.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/schematic#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target schematic-jsoo-android.opam.locked)
  (action (run %{bin:opam} lock schematic-jsoo-android)))
 (rule (mode (promote (until-clean)))
  (target schematic-jsoo.opam.extdeps-android)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:schematic-jsoo-android.opam.locked} --local
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt
     --toolchain android)))))
(alias (name extdeps) (deps opam/schematic-sexp.opam.extdeps))
(subdir opam
 (rule (target schematic-sexp.%{version:schematic-sexp}.opam)
  (deps (:opam ../schematic-sexp.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/schematic#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target schematic-sexp.opam.locked)
  (action (run %{bin:opam} lock schematic-sexp)))
 (rule (mode (promote (until-clean))) (target schematic-sexp.opam.extdeps)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:schematic-sexp.opam.locked} --local
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt))))
 (rule (target schematic-sexp-ios.opam) (mode (promote (until-clean)))
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite-ios --input
     %{dep:../schematic-sexp.opam} --cross
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri
     --cross-both
     bake,fmt,landmarks-ppx,ppx_deriving,ppx_hash,ppx_here,semver2
     --cross-exclude ""))))
 (rule (target schematic-sexp-ios.%{version:schematic-sexp}.opam)
  (deps (:opam schematic-sexp-ios.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/schematic#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target schematic-sexp-ios.opam.locked)
  (action (run %{bin:opam} lock schematic-sexp-ios)))
 (rule (mode (promote (until-clean)))
  (target schematic-sexp.opam.extdeps-ios)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:schematic-sexp-ios.opam.locked} --local
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt
     --toolchain ios))))
 (rule (target schematic-sexp-macos.opam) (mode (promote (until-clean)))
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite-macos --input
     %{dep:../schematic-sexp.opam} --cross
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri
     --cross-both
     bake,fmt,landmarks-ppx,ppx_deriving,ppx_hash,ppx_here,semver2
     --cross-exclude ""))))
 (rule (target schematic-sexp-macos.%{version:schematic-sexp}.opam)
  (deps (:opam schematic-sexp-macos.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/schematic#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target schematic-sexp-macos.opam.locked)
  (action (run %{bin:opam} lock schematic-sexp-macos)))
 (rule (mode (promote (until-clean)))
  (target schematic-sexp.opam.extdeps-macos)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:schematic-sexp-macos.opam.locked} --local
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt
     --toolchain macos))))
 (rule (target schematic-sexp-windows.opam) (mode (promote (until-clean)))
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite-windows --input
     %{dep:../schematic-sexp.opam} --cross
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri
     --cross-both
     bake,fmt,landmarks-ppx,ppx_deriving,ppx_hash,ppx_here,semver2
     --cross-exclude ""))))
 (rule (target schematic-sexp-windows.%{version:schematic-sexp}.opam)
  (deps (:opam schematic-sexp-windows.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/schematic#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target schematic-sexp-windows.opam.locked)
  (action (run %{bin:opam} lock schematic-sexp-windows)))
 (rule (mode (promote (until-clean)))
  (target schematic-sexp.opam.extdeps-windows)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:schematic-sexp-windows.opam.locked} --local
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt
     --toolchain windows))))
 (rule (target schematic-sexp-android.opam) (mode (promote (until-clean)))
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite-android --input
     %{dep:../schematic-sexp.opam} --cross
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri
     --cross-both
     bake,fmt,landmarks-ppx,ppx_deriving,ppx_hash,ppx_here,semver2
     --cross-exclude ""))))
 (rule (target schematic-sexp-android.%{version:schematic-sexp}.opam)
  (deps (:opam schematic-sexp-android.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/schematic#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target schematic-sexp-android.opam.locked)
  (action (run %{bin:opam} lock schematic-sexp-android)))
 (rule (mode (promote (until-clean)))
  (target schematic-sexp.opam.extdeps-android)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:schematic-sexp-android.opam.locked} --local
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt
     --toolchain android)))))
(alias (name extdeps) (deps opam/schematic-sqml.opam.extdeps))
(subdir opam
 (rule (target schematic-sqml.%{version:schematic-sqml}.opam)
  (deps (:opam ../schematic-sqml.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/schematic#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target schematic-sqml.opam.locked)
  (action (run %{bin:opam} lock schematic-sqml)))
 (rule (mode (promote (until-clean))) (target schematic-sqml.opam.extdeps)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:schematic-sqml.opam.locked} --local
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt))))
 (rule (target schematic-sqml-ios.opam) (mode (promote (until-clean)))
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite-ios --input
     %{dep:../schematic-sqml.opam} --cross
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri
     --cross-both
     bake,fmt,landmarks-ppx,ppx_deriving,ppx_hash,ppx_here,semver2
     --cross-exclude ""))))
 (rule (target schematic-sqml-ios.%{version:schematic-sqml}.opam)
  (deps (:opam schematic-sqml-ios.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/schematic#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target schematic-sqml-ios.opam.locked)
  (action (run %{bin:opam} lock schematic-sqml-ios)))
 (rule (mode (promote (until-clean)))
  (target schematic-sqml.opam.extdeps-ios)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:schematic-sqml-ios.opam.locked} --local
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt
     --toolchain ios))))
 (rule (target schematic-sqml-macos.opam) (mode (promote (until-clean)))
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite-macos --input
     %{dep:../schematic-sqml.opam} --cross
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri
     --cross-both
     bake,fmt,landmarks-ppx,ppx_deriving,ppx_hash,ppx_here,semver2
     --cross-exclude ""))))
 (rule (target schematic-sqml-macos.%{version:schematic-sqml}.opam)
  (deps (:opam schematic-sqml-macos.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/schematic#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target schematic-sqml-macos.opam.locked)
  (action (run %{bin:opam} lock schematic-sqml-macos)))
 (rule (mode (promote (until-clean)))
  (target schematic-sqml.opam.extdeps-macos)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:schematic-sqml-macos.opam.locked} --local
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt
     --toolchain macos))))
 (rule (target schematic-sqml-windows.opam) (mode (promote (until-clean)))
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite-windows --input
     %{dep:../schematic-sqml.opam} --cross
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri
     --cross-both
     bake,fmt,landmarks-ppx,ppx_deriving,ppx_hash,ppx_here,semver2
     --cross-exclude ""))))
 (rule (target schematic-sqml-windows.%{version:schematic-sqml}.opam)
  (deps (:opam schematic-sqml-windows.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/schematic#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target schematic-sqml-windows.opam.locked)
  (action (run %{bin:opam} lock schematic-sqml-windows)))
 (rule (mode (promote (until-clean)))
  (target schematic-sqml.opam.extdeps-windows)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:schematic-sqml-windows.opam.locked} --local
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt
     --toolchain windows))))
 (rule (target schematic-sqml-android.opam) (mode (promote (until-clean)))
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite-android --input
     %{dep:../schematic-sqml.opam} --cross
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt,base,base64,cohttp-lwt,ezjsonm,fmt,hlist,logs,lwt,ptime,sexplib,uri
     --cross-both
     bake,fmt,landmarks-ppx,ppx_deriving,ppx_hash,ppx_here,semver2
     --cross-exclude ""))))
 (rule (target schematic-sqml-android.%{version:schematic-sqml}.opam)
  (deps (:opam schematic-sqml-android.opam))
  (action
   (with-stdout-to %{target}
    (progn (cat %{opam})
     (run git log
      "--pretty=url { src: \"git://git@gitlab.routine.co:routine/schematic#%H\" }"
      -n1)))))
 (rule (deps (universe)) (target schematic-sqml-android.opam.locked)
  (action (run %{bin:opam} lock schematic-sqml-android)))
 (rule (mode (promote (until-clean)))
  (target schematic-sqml.opam.extdeps-android)
  (action
   (with-stdout-to %{target}
    (run %{dep:../.logistic/dune/extdeps/extdeps.exe} rewrite --input
     %{dep:schematic-sqml-android.opam.locked} --local
     entropic,schematic,schematic-cli,schematic-ffi,schematic-http,schematic-jsoo,schematic-sexp,schematic-sqml,acid,acid-lwt,bake,hlist,metaversion,sqml,timmy,timmy-lwt
     --toolchain android)))))
