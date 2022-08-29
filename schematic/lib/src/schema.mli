include module type of Schema_intf

val required : (_, _) requirement -> bool

val map :
  decode:('a -> ('b, string) Result.t) -> encode:('b -> 'a) -> 'a t -> 'b t

val pp : Format.formatter -> _ t -> unit
val pp_descriptor : Format.formatter -> _ descriptor -> unit

(** [objects_fields fields] is a list of alternative mandatory fields name set
    required to decode an object with fields [fields]. *)
val object_fields : 'a descriptor -> (string list list, string) Result.t

(** [inline descriptor inline] attemps to inline [descriptor] according to
    [inline].

    This can only succeed on a non-leaf descriptor. *)
val inline :
  't descriptor ->
  'fold ->
  ('fold, 'res) inline ->
  ('fold * 'res list list, string) Result.t

val inline_v : 'res inline_v -> 't fields -> ('t -> 'res list, string) result

val inline_v_fold :
  ('fold, 'res) inline_v_fold ->
  't fields ->
  ('fold -> 't -> 'fold * 'res list, string) result

val field_ignored : ('t, 'req) field -> bool
val field_schema : 't field_schema -> 't t
val compare : 'a t -> 'a -> 'a -> int

type pack

val pack : _ t -> pack

type 'res unpacker = { unpack : 't. 't t -> 'res }

val unpack : 'res unpacker -> pack -> 'res
val name_combine : _ t -> string -> string
val make : ?id:string -> ?parametric:'t parametric -> 't descriptor -> 't t
val fixpoint : ('a descriptor -> 'a descriptor) -> 'a descriptor

module Walk (W : WALK) : sig
  val descriptor : 't descriptor -> W.t
  val parametric : 't parametric -> W.t
  val schema : 't t -> W.t
end
