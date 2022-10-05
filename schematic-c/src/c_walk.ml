(* XXX: we should probably merge this back into a generalized walker in
   schemas *)

open Schematic
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

  val name : t -> string

  (** [schema id parametric schema] *)
  val schema : string option -> (unit -> t) option -> (unit -> t) -> t

  (* Object *)

  val fields_empty : unit -> fields

  (** [field name required t rest]*)
  val field : string -> required:bool -> t -> fields -> fields

  val object_ : fields -> t

  (* Union *)

  val union :
    name:string option ->
    key:string option ->
    cases:(string * t) list ->
    hashes:int list ->
    t

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
  val mapped : string option -> t -> t
end

module Walk (W : WALK) = struct
  let rec descriptor : type t. string option -> t Schema.descriptor -> W.t =
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
    | Map { descriptor = d; _ } ->
      (* XXX reused id *)
      W.mapped id (descriptor id d)
    | Mapping s -> W.mapping (schema s)
    | Nullable s -> W.nullable (schema s)
    | String_const s -> W.string_const s
    | Object { fields = f; _ } -> W.object_ (fields f)
    | Tuple { items; _ } -> W.tuple @@ tuple items
    | Union { cases; key; hashes } ->
      let cases =
        cases
        |> List.map ~f:(fun (Schema.Case { name; schema = s; _ }) ->
               (name, schema s))
      in
      W.union ~name:id ~key ~hashes ~cases

  and schema : type t. t Schema.t -> W.t =
   fun { id; descriptor = d; parametric = p } ->
    let schema () = descriptor id d
    and parametric =
      match p with
      | None -> None
      | Some p ->
        Some
          (fun () ->
            let _name, _args, _eff_args, t = parametric p in
            let name = W.name t in
            W.schema (Some name) None (fun () -> t))
    in
    W.schema id parametric schema

  and parametric :
      type t. t Schema.parametric -> string * string list * W.t list * W.t =
    function
    | Plain { name; descriptor = d } -> (name, [], [], descriptor (Some name) d)
    | Parametric { name; parameter; next } ->
      let template_name, args, eff_args, t =
        parametric (next { parameter with parametric = None })
      in
      (template_name, name :: args, schema parameter :: eff_args, t)

  and fields : type t. t Schema.fields -> W.fields = function
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
        } -> (
      let rest = fields rest
      and schema = schema s in
      match requirement with
      | Computed | Computed_optional -> rest
      | Optional -> W.field name ~required:false schema rest
      | Required | Default _ -> W.field name ~required:true schema rest)

  and tuple : type t. t Schema.tuple -> W.t list = function
    | ItemEnd -> []
    | Item { schema = s; rest } -> schema s :: tuple rest

  let parametric p =
    let _name, _args, _eff_args, t = parametric p in
    t
end
