type 'a customizable =
  [ `Generic
  | `Custom of 'a
  ]

module type S = sig
  type input
  type output

  val name : string
  val read : 'a Schema.t -> input -> input * ('a, Error.decoding) Result.t
  val of_string : 'a Schema.t -> string -> ('a, Error.decoding) Result.t
  val write : 'a Schema.t -> 'a -> output -> output
  val to_string : 'a Schema.t -> 'a -> string
end

module Reader = struct
  type ('a, 'reader, 'error) t = 'reader -> 'reader * ('a, 'error) Result.t
end

module Writer = struct
  type ('a, 'writer) t = 'a -> 'writer -> 'writer
end

module type IMPL = sig
  val name : string

  module Input : sig
    type t

    type error :=
      [ `Channel of string
      | `Content of string
      | `Decoding of Error.decoding
      ]

    type 'a reader := ('a, t, error) Reader.t

    val of_channel : in_channel -> t
    val of_string : string -> t
    val bool : bool reader
    val date : Ptime.date reader customizable
    val eof : unit reader
    val float : float reader
    val int : int reader
    val int64 : int64 reader
    val json : Ezjsonm.t reader customizable
    val list : 'a reader -> 'a list reader
    val object_push : unit reader
    val object_pop : unit reader
    val object_field_push : string -> string option reader
    val object_field_pop : unit reader
    val option : 'a reader -> 'a option reader
    val string : string reader
    val time : Ptime.t reader customizable
    val time_local : (Ptime.t * int) reader customizable
    val tuple_push : unit reader
    val tuple_pop : unit reader
    val tuple_item_push : unit reader
    val tuple_item_pop : unit reader
    val union_push : 'a Schema.case list -> 'a Schema.case reader
    val union_pop : unit reader
  end

  module Output : sig
    type t
    type 'a writer := ('a, t) Writer.t
    type const := t -> t

    val of_channel : out_channel -> t
    val of_buffer : Buffer.t -> t
    val bool : bool writer
    val date : Ptime.date writer customizable
    val eof : const
    val float : float writer
    val int : int writer
    val int64 : int64 writer
    val json : Ezjsonm.value writer customizable
    val list_push : const
    val list_pop : const
    val list_item_push : const
    val list_item_pop : const
    val object_push : const
    val object_pop : const
    val object_field_push : string writer
    val object_field_pop : const
    val option_none : const
    val option_push : const
    val option_pop : const
    val string : string writer
    val time : Ptime.t writer customizable
    val time_local : (Ptime.t * int) writer customizable
    val tuple_push : const
    val tuple_pop : const
    val tuple_item_push : const
    val tuple_item_pop : const
    val union_push : int -> 'a Schema.case writer
    val union_pop : const
  end
end
