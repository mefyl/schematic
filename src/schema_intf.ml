module type Schema = sig
  type 'a schema

  val int_schema : int schema

  val string_schema : string schema

  val bool_schema : bool schema

  val list_schema : 'a schema -> 'a list schema

  val assoc_schema : 'a schema -> (string * 'a) list schema

  val map : ('a -> 'b) -> ('b -> 'a) -> 'a schema -> 'b schema

  type 'a field

  type (_, _) field_requirement =
    | Required : ('a, 'a) field_requirement
    | Default : 'a -> ('a, 'a) field_requirement
    | Optional : ('a, 'a option) field_requirement

  val field :
       ?title:string
    -> ?description:string
    -> ('a, 'req) field_requirement
    -> string
    -> 'a schema
    -> 'req field

  val recursive :
       string
    -> ?title:string
    -> ?description:string
    -> ('a schema -> 'a schema)
    -> 'a schema

  type 'a obj

  val obj : 'f field -> 'f obj

  val obj_extend : 'o obj -> 'f field -> ('o * 'f) obj

  val obj_close : ('a -> 'b) -> ('b -> 'a) -> 'a obj -> 'b schema

  type 'a tuple

  val tuple : 'f schema -> 'f tuple

  val tuple_extend : 't tuple -> 'f schema -> ('t * 'f) tuple

  val tuple_close : ('a -> 'b) -> ('b -> 'a) -> 'a tuple -> 'b schema

  type 't case

  val case : 'a schema -> ('t -> 'a option) -> ('a -> 't) -> 't case

  val union : 't case list -> 't schema
end
