include Schema_intf

let int_schema = Int

let string_schema = String

let bool_schema = Bool

let list_schema schema = List schema

let mapping_schema schema = Mapping schema

let map cons decons schema = Map (cons, decons, schema)

(* let recursive = Json_encoding.mu
 *
 * type 'a obj = 'a schema
 *
 * let obj = Json_encoding.obj1
 *
 * let obj_extend l r = Json_encoding.merge_objs l (Json_encoding.obj1 r)
 *
 * let obj_close = map
 *
 * type 'a tuple = 'a schema
 *
 * let tuple = Json_encoding.tup1
 *
 * let tuple_extend l r = Json_encoding.merge_tups l (Json_encoding.tup1 r)
 *
 * let tuple_close = map
 *
 * type 't case = 't Json_encoding.case
 *
 * let case = Json_encoding.case
 *
 * let union = Json_encoding.union
 *
 * let ( ! ) s = s *)
