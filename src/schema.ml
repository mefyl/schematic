type 'a schema = 'a Json_encoding.encoding

let int_schema = Json_encoding.int

let string_schema = Json_encoding.string

let bool_schema = Json_encoding.bool

let list_schema = Json_encoding.list

let assoc_schema = Json_encoding.assoc

let map cons decons s = Json_encoding.conv decons cons s

type (_, _) field_requirement =
  | Required : ('a, 'a) field_requirement
  | Default : 'a -> ('a, 'a) field_requirement
  | Optional : ('a, 'a option) field_requirement

type 't field = 't Json_encoding.field

let field (type a req) ?title ?description
    (requirement : (a, req) field_requirement) name (schema : a schema) :
    req field =
  match requirement with
  | Required -> Json_encoding.req ?title ?description name schema
  | Optional -> Json_encoding.opt ?title ?description name schema
  | Default v -> Json_encoding.dft ?title ?description name schema v

type 'a obj = 'a schema

let obj = Json_encoding.obj1

let obj_extend l r = Json_encoding.merge_objs l (Json_encoding.obj1 r)

let obj_close = map

type 'a tuple = 'a schema

let tuple = Json_encoding.tup1

let tuple_extend l r = Json_encoding.merge_tups l (Json_encoding.tup1 r)

let tuple_close = map

type 't case = 't Json_encoding.case

let case = Json_encoding.case

let union = Json_encoding.union

let ( ! ) s = s
