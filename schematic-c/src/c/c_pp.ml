open Base

let indent = 2

let block ?(delim = ('{', '}')) ?(open_sp = true) ~indent lhs body =
  let delim_open, delim_close = delim in
  let open Fmt in
  let opening =
    let sp = if open_sp then const char ' ' else nop in
    hbox @@ (const string lhs ++ sp ++ const char delim_open)
  in
  vbox ~indent (opening ++ cut ++ vbox body) ++ cut ++ const char delim_close

let pp_statements = Fmt.((vbox @@ list ~sep:semi string) ++ const char ';')
let ptr_type = Fmt.str "%s*"

let banners section_name =
  match
    [ "start"; "end" ]
    |> List.map ~f:(fun word ->
           Fmt.str "\n\n//\n// %s %s\n//\n\n" section_name word)
  with
  | [ s; e ] -> (s, e)
  | _ -> failwith "unreachable"

let bannerify section_name body =
  let start_banner, end_banner = banners section_name in
  start_banner ^ body ^ end_banner

let case_ish ~indent header body =
  let open Fmt in
  vbox ~indent @@ (header ++ cut ++ body)

let force_newline fmt _ = Caml.Format.pp_force_newline fmt ()

let signature ~return_type ~fname ~argtype ~argname =
  Fmt.str "%s %a(%s %s)" return_type C_identifier.pp fname argtype argname

let case ~indent case contents =
  let open Fmt in
  let post_case, post_box =
    match contents with
    | Some contents -> (cut ++ contents, nop)
    | None -> (nop, force_newline)
  in
  vbox ~indent (const string (Fmt.str "case %s:" case) ++ post_case) ++ post_box

let cases ~indent l =
  let n = List.length l in
  let open Fmt in
  l
  |> List.map ~f:(fun (condition, contents) -> case ~indent condition contents)
  |> List.foldi ~init:nop ~f:(fun i acc x ->
         let post = if i = n - 1 then nop else cut in
         acc ++ x ++ post)
