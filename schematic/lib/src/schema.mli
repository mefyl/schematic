include module type of Schema_intf

val required : (_, _) requirement -> bool

val map :
  decode:('a -> ('b, string) Result.t) -> encode:('b -> 'a) -> 'a t -> 'b t

val pp : Format.formatter -> _ t -> unit
val pp_descriptor : Format.formatter -> _ descriptor -> unit

(** [objects_fields fields] is a list of alternative mandatory fields name set
    required to decode an object with fields [fields]. *)
val object_fields : 'a descriptor -> (string list list, string) Result.t

(** Map a descriptor to the simplest atomic descriptor possible. *)
val flatten : 't descriptor -> 't flatten

type 'res walk_fields = { f : 't. 't fields -> 'res }

val walk_fields :
  't descriptor -> 'res walk_fields -> ('res list, string) result

module Make_inline (M : Base.Monad.S) : sig
  include module type of Make_inline (M)

  (** [inline descriptor inline] attemps to inline [descriptor] according to
      [inline].

      This can only succeed on a non-leaf descriptor. *)
  val schema :
    't descriptor -> 'res inline -> ('res list list M.t, string) Result.t

  val value : 'res inline_v -> 't fields -> ('t -> 'res list M.t, string) result
end

module Inline : module type of Make_inline (Base.Monad.Ident)

val field_ignored : ('t, 'req) field -> bool
val field_schema : 't field_schema -> 't t
val compare : 'a t -> 'a -> 'a -> int
val equal : 'a t -> 'a -> 'a -> bool

type pack

val pack : _ t -> pack

type 'res unpacker = { unpack : 't. 't t -> 'res }

val unpack : 'res unpacker -> pack -> 'res
val name_combine : _ t -> string -> string
val make : ?id:string -> ?parametric:'t parametric -> 't descriptor -> 't t
val fixpoint : ('a descriptor -> 'a descriptor) -> 'a descriptor

module Walk (W : WALK) : sig
  val descriptor : 't descriptor -> W.t W.M.t
  val parametric : 't parametric -> W.t W.M.t
  val schema : 't t -> W.t W.M.t
end
