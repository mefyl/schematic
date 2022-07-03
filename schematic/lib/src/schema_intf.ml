type (_, _) requirement =
  | Required : ('a, 'a) requirement
  | Default : 'a -> ('a, 'a) requirement
  | Optional : ('a, 'a option) requirement

type _ case =
  | Case : {
      name : string;
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
  | Object : 't fields -> 't descriptor
  | Tuple : 't tuple -> 't descriptor
  | Union : {
      key : string option;
      cases : 't case list;
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

module type WALK_TYPES = sig
  type t
  type fields
end

module type WALK_DEFAULT = sig
  include WALK_TYPES

  val t : t
  val fields : fields
end

module type WALK = sig
  include WALK_TYPES

  (* Schema *)

  (** [schema id parametric schema] *)
  val schema : string option -> (unit -> t) option -> (unit -> t) -> t

  (** [parametric template_name arg_names effective_args t] *)
  val parametric : string -> string list -> t list -> t -> t

  (* Object *)

  val fields_empty : unit -> fields
  val field_inline : bool -> fields list -> fields -> fields
  val field_outline : string -> bool -> t -> fields -> fields
  val object_ : fields -> t

  (* Union *)

  val case : string option -> string -> t -> t
  val union : t list -> t

  (* Descriptors *)

  val any : unit -> t
  val bool : unit -> t
  val float : unit -> t
  val date : unit -> t
  val mapping : t -> t
  val int : unit -> t
  val ignore : unit -> t
  val int64 : unit -> t
  val list : t -> t
  val null : unit -> t
  val nullable : t -> t
  val string : unit -> t
  val string_const : string -> t
  val time : unit -> t
  val time_local : unit -> t
  val tuple : t list -> t
end

type library = ..
type version = library -> Version.t option

(** A ['res inline] maps a field to a ['res]. *)
type ('fold, 'res) inline = {
  f : 't 'req. 'fold -> ('t, 'req) field -> 'fold * 'res;
}
