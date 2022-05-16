let version = ref None

let () =
  let f cookies =
    match
      Ppxlib.Driver.Cookies.get cookies "schematic-version"
        Ppxlib.Ast_pattern.(estring __)
    with
    | None -> ()
    | Some s -> version := Schematic.Version.of_string s
  in
  Ppxlib.Driver.Cookies.add_handler f

let deriver = Schematic_ppx_expand.Expand.add ~version false
