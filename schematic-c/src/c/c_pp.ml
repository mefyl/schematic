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
    |> List.map (fun word ->
           Fmt.str "\n\n//\n// %s %s\n//\n\n" section_name word)
  with
  | [ s; e ] -> (s, e)
  | _ -> failwith "unreachable"

let bannerify section_name body =
  let start_banner, end_banner = banners section_name in
  start_banner ^ body ^ end_banner
