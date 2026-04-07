;; WASM stubs for cstruct primitives.
;; Provides: caml_blit_string_to_bigstring
;;
;; From cstruct/lib/cstruct.js: caml_blit_string_to_bigstring is an alias for
;; caml_bigstring_blit_string_to_ba which is itself caml_bigstring_blit_bytes_to_ba.

(module
   (import "env" "caml_bigstring_blit_bytes_to_ba"
      (func $caml_bigstring_blit_bytes_to_ba
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (param (ref eq)) (result (ref eq))))

   (export "caml_blit_string_to_bigstring" (func $caml_bigstring_blit_bytes_to_ba))
)
