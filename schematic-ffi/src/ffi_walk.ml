(* TODO: see how much of this could be factored, similarly to new walker from
   0.15.0 (or into it) *)

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

  val pp : t Fmt.t
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
    name:string -> polymorphic:bool -> cases:(string * string * t) list -> t

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
  val mapped : t -> t
end

module Walk (W : WALK) = struct
  let rec descriptor : type t. string option -> t Schema.descriptor -> W.t =
   fun id d ->
    Logs.debug (fun m ->
        m "FFI Walk Of.descriptor: id=%a, d=%a@." (Fmt.option Fmt.string) id
          Schema.pp_descriptor d);
    let f () =
      match d with
      | Any _ -> failwith "unsupported"
      | Bool -> W.bool ()
      | Float -> W.float ()
      | Date -> W.date ()
      | Fixpoint s -> descriptor id (Lazy.force s)
      | Int -> W.int ()
      | Ignore -> W.ignore ()
      | Int64 -> W.int64 ()
      | Json -> W.any ()
      | List l -> W.list (schema l)
      | Null -> W.null ()
      | String -> W.string ()
      | Time -> W.time ()
      | Time_local -> W.time_local ()
      | Map { descriptor = d; _ } as map_d ->
        (match id with
        | Some _ -> ()
        | None ->
          Fmt.failwith "Mapped descriptor %a must have an id"
            Schematic.Schema.pp_descriptor map_d);
        W.mapped (descriptor id d)
      | Mapping s -> W.mapping (schema s)
      | Nullable s -> W.nullable (schema s)
      | String_const s -> W.string_const s
      | Object { fields = f; _ } -> W.object_ (fields f)
      | Tuple { items; _ } -> W.tuple @@ tuple items
      | Union { cases; polymorphic; key = _ } ->
        let id = Option.value_exn ~here:[%here] id in
        let cases =
          cases
          |> List.map ~f:(fun (Schema.Case { name; raw_name; schema = s; _ }) ->
                 (name, raw_name, schema s))
        in
        W.union ~name:id ~cases ~polymorphic
    in
    try f ()
    with e ->
      let msg =
        Fmt.str "Error while processing descriptor (id = %a): %a"
          Fmt.(option string)
          id Schema.pp_descriptor d
      in
      Base.Exn.reraise e msg

  and schema : type t. t Schema.t -> W.t =
   fun ({ id; descriptor = d; parametric = p } as schema) ->
    Logs.debug (fun m -> m "FFI Walk Of.schema: %a@." Schema.pp schema);
    let schema () = descriptor id d
    and parametric =
      match p with
      | None -> None
      | Some p ->
        Some
          (fun () ->
            let t = parametric id p in
            let id = match id with Some id -> id | None -> W.name t in
            W.schema (Some id) None (fun () -> t))
    in
    try W.schema id parametric schema
    with e ->
      let msg =
        Fmt.str "Error while processing schema (id = %a)@."
          Fmt.(option string)
          id
      in
      Base.Exn.reraise e msg

  and parametric : type t. string option -> t Schema.parametric -> W.t =
   fun id p ->
    match p with
    | Plain { name; descriptor = d } ->
      Logs.debug (fun m -> m "Ffi_walk parametric Plain name: %s@." name);
      descriptor id d
    | Parametric { name; parameter; next } ->
      Logs.debug (fun m -> m "Ffi_walk parametric Parametric name: %s@." name);
      parametric id (next { parameter with parametric = None })

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
end
