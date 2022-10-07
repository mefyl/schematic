let braced ~indent lhs body =
  let open Fmt in
  let opening =
    hbox @@ (const string lhs ++ const char ' ' ++ const char '{')
  in
  vbox ~indent (opening ++ cut ++ vbox body) ++ cut ++ const char '}'

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
