open Base
include Ffi_format

let indent = 2
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

let signature ~return_type ~fname ~argtype ~argname =
  Fmt.str "%s %a(%s %s)" return_type Identifier.pp fname argtype argname

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
