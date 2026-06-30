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

type 'res walk_fields = { f : 't. 't Hlist.t fields -> 'res }

val walk_fields :
  't descriptor -> 'res walk_fields -> ('res list, string) result

module Make_inline (M : Base.Monad.S) : sig
  include module type of Make_inline (M)

  (** [inline descriptor inline] attemps to inline [descriptor] according to
      [inline].

      This can only succeed on a non-leaf descriptor. *)
  val schema :
    't descriptor -> 'res inline -> ('res list list M.t, string) Result.t

  val value_versioned :
    'res inline_v ->
    't Hlist.t fields ->
    ('t Hlist.t -> ('res list * Library_versions.t) M.t, string) result

  val value :
    'res inline_v ->
    't Hlist.t fields ->
    ('t Hlist.t -> 'res list M.t, string) result
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

val make :
  ?id:string ->
  ?documentation:string ->
  ?parametric:'t parametric ->
  't descriptor ->
  't t

val name_combine : _ t -> string -> string
val fixpoint : ('a descriptor -> 'a descriptor) -> 'a descriptor

(** [Walk W] takes care of parsing a Schematic schema into the intermediary type
    [W.t]. Its main benefit compared to pattern matching on the schema itself is
    to take care of inlining and some other subtleties, to avoid each language
    having to take care of the same shenanigans. *)
module Walk (W : WALK) : sig
  val descriptor : id:string option -> 't descriptor -> W.t W.M.t
  val parametric : 't parametric -> W.t W.M.t
  val schema : 't t -> W.t W.M.t
end

val to_sexp : _ t -> Base.Sexp.t
val descriptor_to_sexp : _ descriptor -> Base.Sexp.t

module Name : sig
  include module type of Name

  val pp : t Fmt.t
  val to_string : t -> string
  val to_id : t -> string
  val sexp_of_t : t -> Base.Sexp.t

  include Base.Comparable.S with type t := t

  val of_schema : _ Schema_intf.t -> t option
  val of_parametric : _ Schema_intf.parametric -> t option
  val combine : _ Schema_intf.t -> string -> string option
end

(** [object_wrap field_name schema] is a schema that codes values like [schema]
    but wrapped in an object with a single field named [field_name].

    It is helpful to discriminate coded values that can conflict in dynamic
    codings, eg. with a naive schema [(string, string) Result.t] will always
    yield [Ok] values since both will be coded as strings.

    Wrapping each case will serialize values as [{ value = _ }] and
    [{ error = _ }] objects respectively, preventing such collisions. *)
val object_wrap : string -> 't t -> 't t

(** [case ~decoder ~encoder name schema]. *)
val case :
  decoder:('case -> 't) ->
  encoder:('t -> 'case option) ->
  ?inline:bool ->
  ?since:Semver.t * library ->
  string ->
  'case t ->
  't case

val field :
  ?description:string ->
  ?examples:'t list ->
  ?maximum:int ->
  ?minimum:int ->
  ?omit:bool ->
  requirement:('t, 'requirement) requirement ->
  ?since:Semver.t * library ->
  ?title:string ->
  string ->
  't field_schema ->
  ('t, 'requirement) field

module Fields : sig
  type 't t =
    | [] : unit t
    | ( :: ) : ('t, 'req) field * 'tail t -> ('req -> 'tail) t
end

(** [list element_schema]. *)
val list : 't t -> 't list descriptor

(** [nullable schema]. *)
val nullable : 't t -> 't option descriptor

(** [object' ~decode ~encode fields]. *)
val object' :
  decode:('fields Hlist.t -> 't) ->
  encode:('t -> 'fields Hlist.t) ->
  'fields Fields.t ->
  't descriptor

(** [union ?key cases]. *)
val union : ?key:string -> 't case list -> 't descriptor
