type any = ..

type 'a customizable =
  [ `Generic
  | `Custom of 'a
  ]

type 't key_get = ('t -> string -> ('t option, string) Result.t) customizable
type 't key_set = ('t -> string -> 't -> ('t, string) Result.t) customizable

module type IMPL = sig
  type t

  val name : string
  val compare : t -> t -> int
  val pp : t Fmt.t
  val key_get : t key_get
  val key_set : t key_set
  val null : unit -> t
  val of_any : any -> t option
  val of_bool : bool -> t
  val of_date : (Ptime.date -> t) customizable
  val of_fields : (string * t) list -> t
  val of_float : float -> t
  val of_int : int -> t
  val of_int64 : int64 -> t
  val of_json : Ezjsonm.value -> t
  val of_list : t list -> t
  val of_option : t option -> t
  val of_string : string -> t
  val of_time : Ptime.t -> t
  val of_time_local : Ptime.t * int -> t
  val to_any : t -> any
  val to_bool : t -> bool option
  val to_date : (t -> Ptime.date option) customizable
  val to_fields : t -> (string * t) list option
  val to_float : t -> float option
  val to_int : t -> int option
  val to_int64 : t -> int64 option
  val to_json : t -> Ezjsonm.value option
  val to_list : t -> t list option
  val to_option : t -> t option option
  val to_string : t -> string option
  val to_time : t -> (Ptime.t, string option) Result.t
  val to_time_local : t -> (Ptime.t * int, string option) Result.t
  val type_name : t -> string
end
