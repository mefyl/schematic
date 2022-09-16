open Schema
open Base
include C_intf

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

  (** [field_inline required fields rest] *)
  val field_inline : bool -> fields list -> fields -> fields

  (** [field_outline name required t rest]*)
  val field_outline : string -> bool -> t -> fields -> fields

  val object_ : fields -> t

  (* Union *)

  (** [case key name t] *)
  val case : string option -> string -> t -> t

  val union : string option -> t list -> t

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

module Walk (W : WALK) = struct
  let rec descriptor : type t. string option -> t descriptor -> W.t =
   fun id d ->
    match d with
    | Any -> W.any ()
    | Bool -> W.bool ()
    | Float -> W.float ()
    | Date -> W.date ()
    | Fixpoint s -> descriptor id (Lazy.force s)
    | Int -> W.int ()
    | Ignore -> W.ignore ()
    | Int64 -> W.int64 ()
    | List l -> W.list (schema l)
    | Null -> W.null ()
    | String -> W.string ()
    | Time -> W.time ()
    | Time_local -> W.time_local ()
    | Map { descriptor = d; _ } -> descriptor id d
    | Mapping s -> W.mapping (schema s)
    | Nullable s -> W.nullable (schema s)
    | String_const s -> W.string_const s
    | Object f -> W.object_ (fields f)
    | Tuple l -> W.tuple @@ tuple l
    | Union { cases; key } -> W.union id @@ List.map ~f:(case key) cases

  and case : type t. string option -> t case -> W.t =
   fun key (Case { name; schema = s; _ }) -> W.case key name (schema s)

  and schema : type t. t Schema_intf.t -> W.t =
   fun { id; descriptor = d; parametric = p } ->
    let schema () = descriptor id d
    and parametric =
      match p with None -> None | Some p -> failwith "no generics"
    in
    W.schema id parametric schema

  and fields : type t. t fields -> W.fields = function
    | FieldEnd -> W.fields_empty ()
    | Field
        {
          field =
            {
              schema =
                ( Outline { descriptor = Ignore; _ }
                | Inline { descriptor = Ignore; _ }
                | Inline_try { descriptor = Ignore; _ } );
              _;
            };
          rest;
        } ->
      fields rest
    | Field { field = { omit = true; _ }; rest } -> fields rest
    | Field { field = { name; requirement; schema = Outline s; _ }; rest } ->
      let rest = fields rest
      and schema = schema s in
      W.field_outline name (required requirement) schema rest
    | Field
        { field = { requirement; schema = Inline { descriptor; _ }; _ }; rest }
      ->
      let fields =
        match walk_fields descriptor { f = (fun f -> fields f) } with
        | Result.Ok v -> v
        | Result.Error e -> failwith e
      and rest = fields rest in
      W.field_inline (required requirement) fields rest
    | Field
        {
          field =
            {
              name;
              requirement;
              schema = Inline_try ({ descriptor; _ } as s);
              _;
            };
          rest;
        } -> (
      let rest = fields rest in
      match walk_fields descriptor { f = (fun f -> fields f) } with
      | Result.Ok fields -> W.field_inline (required requirement) fields rest
      | Result.Error _ ->
        W.field_outline name (required requirement) (schema s) rest)

  and tuple : type t. t tuple -> W.t list = function
    | ItemEnd -> []
    | Item { schema = s; rest } -> schema s :: tuple rest
end
