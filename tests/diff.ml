open Base

module Nested = struct
  type i = int [@@deriving schema ~diff]
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

and tuple = bool * string [@@deriving schema ~diff]

let pp_variant fmt =
  let module M = O_monadic (Option) in
  let open M in
  function
  | Foo -> Fmt.string fmt "Foo"
  | Bar i -> Fmt.(const string "Bar" ++ sp ++ option int) fmt i

(* FIXME: Factor with monadic.ml. Better even, generate from schema. *)
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

module Diff = struct
  let nested () =
    let () = Alcotest.(check (option int) "same" None (Nested.i_diff 0 0)) in
    let () =
      Alcotest.(check (option int) "diff" (Some 1) (Nested.i_diff 0 1))
    in
    ()

  let record () =
    let module M = O_monadic (Option) in
    let v1 = { i = 45; nested = 90; l = [ 92; 93; 94 ]; v = Bar 0 }
    and v2 = { i = 45; nested = 90; l = [ 92; 90; 94 ]; v = Bar 1 }
    and diff =
      let open O_monadic (Option) in
      {
        i = None;
        nested = None;
        l = Some [ None; Some 90; None ];
        v = Some (Bar (Some 1));
      }
    in
    let () = Alcotest.(check (option o_monadic) "same" None (o_diff v1 v1))
    and () =
      Alcotest.(check (option o_monadic) "diff" (Some diff) (o_diff v1 v2))
    in
    ()

  let tuple () =
    let module M = Tuple_monadic (Option) in
    let () =
      Alcotest.(
        check (option (pair (option bool) (option string))) "same" None
        @@ tuple_diff (true, "false") (true, "false"))
    and () =
      Alcotest.(
        check
          (option (pair (option bool) (option string)))
          "diff"
          (Some (None, Some "fals"))
        @@ tuple_diff (true, "false") (true, "fals"))
    in
    ()
end

let () =
  let open Alcotest in
  run "Schematic.Diff"
    [
      ( "return",
        [
          test_case "nested" `Quick Diff.nested;
          test_case "record" `Quick Diff.record;
          test_case "tuple" `Quick Diff.tuple;
        ] );
    ]
