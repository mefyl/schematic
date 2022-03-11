type t = string

(** [module type S] is the intended interface of the Base64 module. *)
module type S = sig
  val decode : 'a Schema.t -> t -> ('a, Error.decoding) Base.Result.t
  val encode : 'a Schema.t -> 'a -> t
  val of_string : string -> (t, Error.parsing) Base.Result.t
  val to_string : t -> string
  val equal : t -> t -> bool
  val pp : Base.Formatter.t -> t -> unit
end

(** [module type Decoder_Impl] is the abstraction for a Base64 decoder. *)
module type Decoder_Impl = sig
  val of_string : string -> (t, Error.parsing) Result.t
  val to_string : t -> string
end

(** [module type Backend_Impl] is the abstraction used to manage the actual
    encoding used inside the Base64 representation. *)
module type Backend_Impl = sig
  type t

  val of_string : string -> (t, Error.parsing) Result.t
  val to_string : t -> string
  val decode : 'a Schema.t -> t -> ('a, Error.decoding) Base.Result.t
  val encode : 'a Schema.t -> 'a -> t
end