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

let option : option =
  {
    i = Some 11262020;
    nested = Some 0;
    l = Some [ Some 1 ];
    v = Some (Bar (Some 2));
  }

let variant : Variant_monadic(Base.Option).t list = [ Foo; Bar (Some 0) ]

let tuple : Tuple_monadic(Base.Option).t = (Some true, Some "")

type alias = o [@@deriving schema ~monadic]

let alias : Alias_monadic(Base.Option).t = option
