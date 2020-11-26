module Nested = struct
  type i = int [@@deriving schema ~monadic]
end

type o = {
  i : int;
  nested : Nested.i;
  l : int list;
}

and variant =
  | Foo
  | Bar of int

and tuple = bool * string [@@deriving schema ~monadic]

type option = Monadic(Base.Option).o

let option : option =
  { i = Some 11262020; nested = Some 0; l = Some [ Some 1 ] }

let variant : Monadic(Base.Option).variant list = [ Foo; Bar (Some 0) ]

let tuple : Monadic(Base.Option).tuple = (Some true, Some "")
