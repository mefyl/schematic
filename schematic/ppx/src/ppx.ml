let jsoo = ref true
let version = ref None

let handle name pattern ~f =
  Ppxlib.Driver.Cookies.add_simple_handler name pattern ~f:(Option.iter f)

let ebool t =
  let open Ppxlib.Ast_pattern in
  let f ctx loc exp f =
    let ident = to_func @@ pexp_construct (lident __) none
    and t = to_func t in
    ident ctx loc exp (function
      | "false" -> t ctx loc false f
      | "true" -> t ctx loc true f
      | _ -> Ppxlib.Location.raise_errorf ~loc "expected literal boolean")
  in
  Ppxlib.Ast_pattern.of_func f

let () =
  handle "enable-jsoo" Ppxlib.Ast_pattern.(ebool __) ~f:(fun b -> jsoo := b);
  handle "schematic-version"
    Ppxlib.Ast_pattern.(estring __)
    ~f:(fun s -> version := Schematic.Version.of_string s)

let deriver =
  Schematic_ppx_expand.Expand.add ~inside:false ~jsoo:!jsoo ~version:!version
