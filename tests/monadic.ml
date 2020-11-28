open Base

module Nested = struct
  type i = int [@@deriving schema ~monadic]
end

type o = {
  i : int;
  nested : Nested.i;
  l : int list;
  v : variant;
}

and variant =
  | Foo
  | Bar of int

and tuple = bool * string [@@deriving schema ~monadic]

type option = O_monadic(Base.Option).t

let pp_variant fmt =
  let module M = O_monadic (Option) in
  let open M in
  function
  | Foo -> Fmt.string fmt "Foo"
  | Bar i -> Fmt.(const string "Bar" ++ sp ++ option int) fmt i

let o_monadic =
  let module M = O_monadic (Option) in
  let open M in
  Alcotest.testable
    Fmt.(
      record
        [
          field "i" (fun o -> o.i) (option int);
          field "nested" (fun o -> o.nested) (option int);
          field "l"
            (fun o -> o.l)
            (option (list ~sep:(const string ",") (option int)));
          field "v" (fun o -> o.v) (option pp_variant);
        ])
    Poly.( = )

let option : option =
  {
    i = Some 11262020;
    nested = Some 0;
    l = Some [ Some 1 ];
    v = Some (Bar (Some 2));
  }

let (_ : Variant_monadic(Base.Option).t list) = [ Foo; Bar (Some 0) ]

let (_ : Tuple_monadic(Base.Option).t) = (Some true, Some "")

type alias = o [@@deriving schema ~monadic]

let (_ : Alias_monadic(Base.Option).t) = option

module Return = struct
  let nested () =
    let module M = Nested.I_monadic (Option) in
    Alcotest.(check (option int) "nested" (Some 38) @@ M.return 38)

  let record () =
    let module M = O_monadic (Option) in
    let v = { i = 45; nested = 90; l = [ 92; 93; 94 ]; v = Foo }
    and exp =
      {
        M.i = Some 45;
        nested = Some 90;
        l = Some [ Some 92; Some 93; Some 94 ];
        v = Some Foo;
      }
    in
    Alcotest.(check o_monadic "record" exp @@ M.return v)

  let tuple () =
    let module M = Tuple_monadic (Option) in
    let v = (true, "false")
    and exp = (Some true, Some "false") in
    Alcotest.(
      check (pair (option bool) (option string)) "tuple" exp @@ M.return v)
end

let () =
  let open Alcotest in
  run "Schematic.Monadic"
    [
      ( "return",
        [
          test_case "nested" `Quick Return.nested;
          test_case "record" `Quick Return.record;
          test_case "tuple" `Quick Return.tuple;
        ] );
    ]
