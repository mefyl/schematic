open Schema
open Base
include C_intf

module type WALK_TYPES = sig
  type t
  type fields
  type case
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

  (** [field name required t rest]*)
  val field : string -> bool -> t -> fields -> fields

  val object_ : fields -> t

  (* Union *)

  val union : name:string option -> key:string option -> (string * t) list -> t

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
    | Union { cases; key } ->
      let cases =
        cases
        |> List.map ~f:(fun (Schema.Case { name; schema = s; _ }) ->
               (name, schema s))
      in
      W.union ~name:id ~key cases

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
    | Field
        {
          field =
            {
              name;
              requirement;
              schema = Outline s | Inline s | Inline_try s;
              _;
            };
          rest;
        } ->
      let rest = fields rest
      and schema = schema s in
      W.field name (required requirement) schema rest

  and tuple : type t. t tuple -> W.t list = function
    | ItemEnd -> []
    | Item { schema = s; rest } -> schema s :: tuple rest
end
