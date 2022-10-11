type (_, _) requirement =
  | Computed : ('a, 'a option) requirement
  | Computed_optional : ('a, 'a option option) requirement
  | Default : 'a -> ('a, 'a) requirement
  | Required : ('a, 'a) requirement
  | Optional : ('a, 'a option) requirement

type _ case =
  | Case : {
      name : string;
      raw_name : string;
      schema : 'a t;
      encoder : 't -> 'a option;
      decoder : 'a -> 't;
    }
      -> 't case

and 'a field_schema =
  | Outline of 'a t
  | Inline of 'a t
  | Inline_try of 'a t

(** A [_ field] can describe any number of object fields. *)
and _ fields =
  | FieldEnd : unit fields
  | Field : {
      field : ('t, 'req) field;
      rest : 'rest fields;
    }
      -> ('req * 'rest) fields

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
  | ItemEnd : unit tuple
  | Item : {
      schema : 'a t;
      rest : 'b tuple;
    }
      -> ('a * 'b) tuple

(** A descriptor describes the structure of a type. *)
and _ descriptor =
  | Any : Ezjsonm.value descriptor
  | Bool : bool descriptor
  | Date : Ptime.date descriptor
  | Fixpoint : 'a descriptor Lazy.t -> 'a descriptor
  | Float : float descriptor
  | Ignore : unit descriptor
  | Int : int descriptor
  | Int64 : int64 descriptor
  | List : 'a t -> 'a list descriptor
  (* 'a: facing, 'b: underlying *)
  | Map : {
      decode : 'a -> ('b, string) Result.t;
      encode : 'b -> 'a;
      descriptor : 'a descriptor;
    }
      -> 'b descriptor
  | Mapping : 'a t -> (string * 'a) list descriptor
  | Null : unit descriptor
  | Nullable : 'a t -> 'a option descriptor
  | String : string descriptor
  | String_const : string -> unit descriptor
  | Time : Ptime.t descriptor
  | Time_local : (Ptime.t * Ptime.tz_offset_s) descriptor
  | Object : {
      decode : 'fields -> 't;
      encode : 't -> 'fields;
      fields : 'fields fields;
    }
      -> 't descriptor
  | Tuple : {
      decode : 'tuple -> 't;
      encode : 't -> 'tuple;
      items : 'tuple tuple;
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
  type field

  module M : Base.Monad.S

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
  val field : string -> bool -> t M.t -> field M.t

  (** [object_ fields] combines the fields alternatives in a single object.

      There can be multiple [field list] alternative in case a variant is
      inlined. *)
  val object_ : field list list M.t -> t M.t

  (** [inline schema field] potentially intercepts inline fields.

      The field is not inlined and replaced with the return value if it is not
      [None]. The [schema] argument can be used to walk the field schema. *)
  val inline : (unit -> t M.t) -> ('t, 'req) schema_field -> field M.t option

  (* Union *)

  (** [case key name t] *)
  val case : string option -> string -> t M.t -> t M.t

  val union : t list M.t -> t M.t

  (* Descriptors *)

  val any : t M.t
  val bool : t M.t
  val float : t M.t
  val date : t M.t
  val mapping : t M.t -> t M.t
  val int : t M.t
  val ignore : t M.t
  val int64 : t M.t
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
type version = library -> Version.t option

(** A descriptor mapped to the simplest atomic descriptor possible *)
type 't flatten =
  | Flatten : {
      descriptor : 'a descriptor;
      decode : 'a -> ('t, string) Result.t;
      encode : 't -> 'a;
    }
      -> 't flatten

module Make_inline (M : Base.Monad.S) = struct
  (** A ['res inline] maps a field to a ['res]. *)
  type 'res inline = {
    f : 't 'req. ('t, 'req) field -> 'res M.t;
    filter : 't 'req. (('t, 'req) field -> 'res M.t option) option;
  }

  (** A ['res inline] maps a field and a value for that field to a ['res]. *)
  type 'res inline_v = { f : 't 'req. ('t, 'req) field -> 'req -> 'res M.t }
end
