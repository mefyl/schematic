open Base

type (_, _) requirement =
  | Computed : ('a, 'a option) requirement
  | Computed_optional : ('a, 'a option option) requirement
  | Default : 'a -> ('a, 'a) requirement
  | Required : ('a, 'a) requirement
  | Optional : ('a, 'a option) requirement

type _ case =
  | Case : {
      decoder : 'a -> 't;
      encoder : 't -> 'a option;
      inline : bool;
          (** Whether the tuple or record argument is inlined in the OCaml
              memory representation of the constructor. *)
      name : string;
      raw_name : string;
      schema : 'a t;
    }
      -> 't case

and 'a field_schema =
  | Outline of 'a t
  | Inline of 'a t
  | Inline_try of 'a t

(** A [_ field] can describe any number of object fields. *)
and 't fields =
  | FieldEnd : unit Hlist.t fields
  | Field : {
      field : ('t, 'req) field;
      rest : 'tail Hlist.t fields;
    }
      -> ('req -> 'tail) Hlist.t fields

and ('t, 'req) field = {
  description : string option;
  examples : 't list;
  maximum : int option;
  minimum : int option;
  name : string;
  omit : bool;
  requirement : ('t, 'req) requirement;
  schema : 't field_schema;
  title : string option;
}

and _ tuple =
  | ItemEnd : unit Hlist.t tuple
  | Item : {
      schema : 'a t;
      rest : 'b Hlist.t tuple;
    }
      -> ('a -> 'b) Hlist.t tuple

(** A descriptor describes the structure of a type. *)
and _ descriptor =
  | Any : (module Coding_intf.IMPL with type t = 't) -> 't descriptor
  | Bool : bool descriptor
  | Coding : {
      f : 'coding. 'coding Coding_intf.t option -> 'a descriptor;
    }
      -> 'a descriptor
  | Date : Ptime.date descriptor
  | Fixpoint : 'a descriptor Lazy.t -> 'a descriptor
  | Float : float descriptor
  | Ignore : unit descriptor
  | Int : int descriptor
  | Int64 : int64 descriptor
  | Json : Ezjsonm.value descriptor
  | List : 'a t -> 'a list descriptor
  | Map : {
      decode : 'encoded -> ('exposed, string) Result.t;
      encode : 'exposed -> 'encoded;
      descriptor : 'encoded descriptor;
    }
      -> 'exposed descriptor
  | Mapping : {
      construct : (string * 'item) Sequence.t -> ('result, string) Result.t;
      deconstruct : 'result -> (string * 'item) Sequence.t;
      item : 'item t;
      length : 'result -> int;
    }
      -> 'result descriptor
      (** A dictionary.

          Some backend will specialize this case, for instance JSON will
          represent this as and object and not a list of pairs. *)
  | Null : unit descriptor
  | Nullable : 'a t -> 'a option descriptor
  | Object : {
      decode : 'fields Hlist.t -> 't;
      encode : 't -> 'fields Hlist.t;
      fields : 'fields Hlist.t fields;
    }
      -> 't descriptor
  | Sequence : {
      construct : 'item Sequence.t -> ('result, string) Result.t;
      deconstruct : 'result -> 'item Sequence.t;
      item : 'item t;
      length : 'result -> int;
    }
      -> 'result descriptor
      (** A list of values just like [List], but consumed and produced as a
          sequence. *)
  | String : string descriptor
  | String_const : string -> unit descriptor
  | Time : Ptime.t descriptor
  | Time_local : (Ptime.t * Ptime.tz_offset_s) descriptor
  | Tuple : {
      decode : 'tuple Hlist.t -> 't;
      encode : 't -> 'tuple Hlist.t;
      items : 'tuple Hlist.t tuple;
    }
      -> 't descriptor
  | Union : {
      key : string option;
      cases : 't case list;
      polymorphic : bool;
    }
      -> 't descriptor

and 't t = {
  id : string option;
  descriptor : 't descriptor;
  parametric : 't parametric option;
}

and 't schema = 't t

and 't parametric =
  | Plain of {
      name : string;
      descriptor : 't descriptor;
    }
  | Parametric : {
      name : string;
      parameter : 'p t;
      next : 'p t -> 't parametric;
    }
      -> 't parametric

type ('t, 'req) schema_field = ('t, 'req) field

module type WALK = sig
  type t
  type case
  type field

  module M : Monad.S

  (* Schema *)

  (** [schema id parametric schema] *)
  val schema :
    string option -> (unit -> t M.t) option -> (unit -> t M.t) -> t M.t

  (** [parametric template_name arg_names effective_args t] *)
  val parametric : string -> string list -> t M.t list -> t M.t -> t M.t

  (* Object *)

  (** [field name required t] walks an object field.

      All schema details such as ignored fields and inlining are handled by the
      walker. *)
  val field : computed:bool -> required:bool -> string -> t M.t -> field M.t

  (** [object_ fields] combines the fields alternatives in a single object.

      There can be multiple [field list] alternative in case a variant is
      inlined. *)
  val object_ : field list list M.t -> t M.t

  (** [parametric schema field] potentially intercepts inline fields.

      The field is not inlined and replaced with the return value if it is not
      [None]. The [schema] argument can be used to walk the field schema. *)
  val inline :
    (unit -> t M.t) option ->
    (unit -> t M.t) ->
    ('t, 'req) schema_field ->
    field M.t option

  (* Union *)

  val case :
    key:string option ->
    inline:bool ->
    name:string ->
    raw_name:string ->
    t M.t ->
    case M.t

  val union : id:string option -> polymorphic:bool -> case list M.t -> t M.t

  (* Descriptors *)

  val any : t M.t
  val bool : t M.t
  val float : t M.t
  val date : t M.t
  val map : t M.t -> t M.t
  val mapping : t M.t -> t M.t
  val int : t M.t
  val ignore : t M.t
  val int64 : t M.t
  val json : t M.t
  val list : t M.t -> t M.t
  val null : t M.t
  val nullable : t M.t -> t M.t
  val string : t M.t
  val string_const : string -> t M.t
  val time : t M.t
  val time_local : t M.t
  val tuple : t list M.t -> t M.t
end

type library = ..
type version = library -> Semver.t option

(** A descriptor mapped to the simplest atomic descriptor possible *)
type 't flatten =
  | Flatten : {
      descriptor : 'a descriptor;
      decode : 'a -> ('t, string) Result.t;
      encode : 't -> 'a;
    }
      -> 't flatten

module Make_inline (M : Monad.S) = struct
  (** A ['res inline] maps a field to a ['res]. *)
  type 'res inline = {
    f : 't 'req. ('t, 'req) field -> 'res M.t;
    filter : 't 'req. (('t, 'req) field -> 'res M.t option) option;
  }

  (** A ['res inline] maps a field and a value for that field to a ['res]. *)
  type 'res inline_v = { f : 't 'req. ('t, 'req) field -> 'req -> 'res M.t }
end

module Name = struct
  type t =
    | Id of string
    | Parametric of string * t list
  [@@deriving eq, ord]
end
