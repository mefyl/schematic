open Schema

type 'a t = 'a descriptor

(* FIXME: this should replace the [fields] type when we break compatibility. *)
module Fields = struct
  type 't t =
    | [] : unit t
    | ( :: ) : ('t, 'req) field * 'tail t -> ('req -> 'tail) t
end

(* FIXME: this should replace the [tuple] type when we break compatibility. *)
module Schemas = Hlist.Make (Schema)

let bool = Bool

let case ~decoder ~encoder ?(inline = false) ?since name schema =
  Case { decoder; encoder; inline; name; raw_name = name; schema; since }

let date = Date

let field ?description ?(examples = []) ?maximum ?minimum ?(omit = false)
    ~requirement ?since ?title name schema =
  {
    description;
    examples;
    maximum;
    minimum;
    name;
    omit;
    requirement;
    schema;
    since;
    title;
  }

let float = Float
let int = Int
let int64 = Int64
let json = Json
let list schema = List schema
let map ~decode ~encode descriptor = Map { decode; encode; descriptor }
let null = Null
let nullable schema = Nullable schema

let object' ~decode ~encode fields =
  let rec _fields : 'f. 'f Fields.t -> 'f Hlist.t fields =
   fun (type f) (fields : f Fields.t) : f Hlist.t fields ->
    match fields with
    | [] -> FieldEnd
    | field :: tail -> Field { field; rest = _fields tail }
  in
  Object { decode; encode; fields = _fields fields }

let string = String
let string_const string = String_const string
let time = Time
let time_local = Time_local

let tuple ~decode ~encode items =
  let rec _items : 't. 't Schemas.t -> 't Hlist.t tuple =
   fun (type t) (fields : t Schemas.t) : t Hlist.t tuple ->
    match fields with
    | [] -> ItemEnd
    | schema :: rest -> Item { schema; rest = _items rest }
  in
  Tuple { decode; encode; items = _items items }

let union ?key cases = Union { cases; key; polymorphic = false }
