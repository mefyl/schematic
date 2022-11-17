let indent = 2 (* FIXME *)

let force_newline fmt _ = Caml.Format.pp_force_newline fmt ()

let block ?(delim = ('{', '}')) ?(open_sp = true) ~indent lhs body =
  let delim_open, delim_close = delim in
  let open Fmt in
  let opening =
    let sp = if open_sp then const char ' ' else nop in
    hbox @@ (const string lhs ++ sp ++ const char delim_open)
  in
  vbox ~indent (opening ++ cut ++ vbox body) ++ cut ++ const char delim_close

let case_ish ~indent header body =
  let open Fmt in
  vbox ~indent @@ (header ++ cut ++ body)

let applied opening body =
  block ~indent ~open_sp:false ~delim:('(', ')') opening body

let vertical_comma_sep pp l =
  let open Fmt in
  const (list ~sep:(const char ',' ++ cut) pp) l
