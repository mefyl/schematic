module E = Error
open Base
include Coding_intf

let map_results ~f l =
  let rec rev_results res = function
    | [] -> Result.return res
    | (Result.Error _ as error) :: _ -> error
    | Result.Ok hd :: tl -> rev_results (hd :: res) tl
  in
  List.rev_map ~f l |> rev_results []

let fail (context : E.decoding) fmt =
  let f reason = { context with reason } in
  Fmt.kstr f fmt

let type_error expected effective =
  Fmt.str "expected %s, got %s" expected effective

module Defaults = struct
  let key_get = `Generic
  let key_set = `Generic
  let of_date = `Generic
  let to_date = `Generic
end

module type S = sig
  type t

  val name : string
  val encode : 'a Schema.t -> 'a -> t
  val decode : 'a Schema.t -> t -> ('a, E.decoding) Result.t
end

module Make (Coding : IMPL) : S with type t = Coding.t = struct
  type t = Coding.t
  type date_tuple = int * int * int [@@deriving schema]

  let name = Coding.name
  let ( let* ) = Result.( >>= )

  let type_error (context : E.decoding) expected v = function
    | Some v -> Result.return v
    | None -> (
      match Coding.to_string v with
      | Some string ->
        Result.fail @@ fail context "expected %s, got %S" expected string
      | None ->
        Result.fail @@ fail context "%s"
        @@ type_error expected (Coding.type_name v))

  let key_get (context : E.decoding) o f =
    match Coding.key_get with
    | `Custom impl ->
      impl o f |> Result.map_error ~f:(fun reason -> { context with reason })
    | `Generic ->
      let* fields = Coding.to_fields o |> type_error context "object" o in
      List.Assoc.find ~equal:String.equal fields f |> Result.return

  let key_set (context : E.decoding) o f v =
    match Coding.key_set with
    | `Custom impl ->
      impl o f v |> Result.map_error ~f:(fun reason -> { context with reason })
    | `Generic ->
      let* fields = Coding.to_fields o |> type_error context "object" v in
      Result.return (Coding.of_fields @@ ((f, v) :: fields))

  let requirement_return (type a req) (req : (a, req) Schema.requirement)
      (v : a) : req =
    match req with
    | Computed -> Some v
    | Computed_optional -> Some (Some v)
    | Default _ -> v
    | Optional -> Some v
    | Required -> v

  let missing descriptor fields =
    let rec unroll : type t. t Schema.descriptor -> string list list = function
      | Fixpoint descriptor -> unroll (Lazy.force descriptor)
      | Nullable schema -> unroll schema.descriptor
      | Map { descriptor; _ } -> unroll descriptor
      | descriptor -> (
        match Schema.object_fields descriptor with
        | Result.Ok v -> v
        | Result.Error _ -> [ [] ])
    in
    unroll descriptor
    |> List.find ~f:(fun expected ->
           Option.is_none
           @@ List.find
                ~f:(fun f -> not (List.Assoc.mem ~equal:String.equal fields f))
                expected)
    |> Option.is_none

  let decode schema =
    let decode =
      let rec decode_field :
          type t r.
          bool ->
          E.decoding ->
          string ->
          t Schema.t ->
          (t, r) Schema.requirement ->
          Coding.t option ->
          (r, 'error) Result.t =
       fun omit context field schema ->
        let decode schema v =
          decode_schema { context with path = field :: context.path } schema v
        in
        function
        | Default default -> (
          function
          | Some v -> (
            if omit then Result.return default
            else
              match (Coding.to_option v, schema.descriptor) with
              | Some None, Nullable nullable -> (
                match decode nullable v with
                | Result.Ok v -> Result.return (Some v)
                | Result.Error _ -> decode schema v)
              | Some None, _ -> decode schema v
              | Some (Some v), _ -> decode schema v
              | None, _ -> type_error context "option" v None)
          | None -> Result.return default)
        | Computed -> fun _ -> Result.Ok None
        | Computed_optional -> fun _ -> Result.Ok None
        | Optional -> (
          function
          | Some v -> (
            match Coding.to_option v with
            | Some None -> (
              match decode schema v with
              | Result.Ok v -> Result.return (Some v)
              | Result.Error _ -> Result.return None)
            | Some (Some v) -> Result.map ~f:Option.return @@ decode schema v
            | None -> type_error context "option" v None)
          | None -> Result.return None)
        | Required -> (
          function
          | Some v -> decode schema v
          | None -> Result.fail (fail context "missing required field %S" field)
          )
      and decode_descriptor :
          type t.
          E.decoding ->
          t Schema.descriptor ->
          Coding.t ->
          (t, E.decoding) Result.t =
       fun context schema v ->
        let type_error expected res = type_error context expected v res
        and error reason =
          let f reason = { context with reason } in
          Fmt.kstr f reason
        and ( >>= ) = Result.( >>= ) in
        match schema with
        | Any (module Any) ->
          Any.of_any (Coding.to_any v) |> type_error Any.name
        | Bool -> Coding.to_bool v |> type_error "boolean"
        | Coding { f } -> decode_descriptor context (f (Some Coding.coding)) v
        | Date -> (
          match Coding.to_date with
          | `Generic -> decode_schema context date_tuple_schema v
          | `Custom f -> f v |> type_error "date")
        | Fixpoint s -> decode_descriptor context (Lazy.force s) v
        | Float -> Coding.to_float v |> type_error "float"
        | Ignore -> Result.return ()
        | Int -> Coding.to_int v |> type_error "integer"
        | Int64 -> Coding.to_int64 v |> type_error "integer"
        | Json -> Coding.to_json v |> type_error "JSON"
        | List items_schema ->
          let* v = Coding.to_list v |> type_error "array" in
          map_results ~f:(decode_schema context items_schema) v
        | Sequence { construct; item; _ } ->
          let* items =
            Coding.to_list v |> type_error "array"
            >>= map_results ~f:(decode_schema context item)
          in
          Base.Sequence.of_list items |> construct |> Result.return
        | Map { decode = cons; descriptor; _ } ->
          decode_descriptor context descriptor v >>= fun value ->
          cons value |> Result.map_error ~f:(fun reason -> error "%s" reason)
        | Mapping fields_schema ->
          let* fields = Coding.to_fields v |> type_error "mapping" in
          let f (name, value) =
            decode_schema context fields_schema value >>= fun value ->
            Result.return (name, value)
          in
          map_results ~f fields
        | Null ->
          let res =
            match Coding.to_option v with Some None -> Some () | _ -> None
          in
          type_error "null" res
        | Nullable s -> (
          match Coding.to_option v with
          | Some None -> Result.return None
          | Some (Some v) ->
            decode_schema context s v |> Result.map ~f:Option.return
          | None -> type_error "option" None)
        | Object { fields = fields_schema; decode = o_decode; _ } ->
          let* fields = Coding.to_fields v |> type_error "object" in
          let rec decode_object :
              type t.
              t Hlist.t Schema.fields -> (t Hlist.t, E.decoding) Result.t =
            function
            | FieldEnd -> Result.return Hlist.[]
            | Field
                {
                  field =
                    {
                      name = field_name;
                      omit;
                      requirement;
                      schema = field_schema;
                      _;
                    };
                  rest;
                } -> (
              match Schema.field_schema field_schema with
              | { descriptor = Ignore; _ } ->
                let* rest = decode_object rest in
                Result.Ok Hlist.(requirement_return requirement () :: rest)
              | schema ->
                let field =
                  let value =
                    let inline Schema.{ descriptor; _ } =
                      let missing = missing descriptor fields
                      and inlinable =
                        Schema.Inline.schema descriptor
                          { f = (fun _ -> ()); filter = None }
                        |> Result.is_ok
                      in
                      (missing, inlinable)
                    and outline () =
                      List.Assoc.find ~equal:String.equal fields field_name
                    in
                    match field_schema with
                    | Inline schema ->
                      let missing, _inlinable = inline schema in
                      if missing then None else Some v
                    | Inline_try schema ->
                      let missing, inlinable = inline schema in
                      if missing then None
                      else if inlinable then Some v
                      else outline ()
                    | Outline _ -> outline ()
                  in
                  decode_field omit context field_name schema requirement value
                in
                let* field in
                let* tail = decode_object rest in
                Result.Ok Hlist.(field :: tail))
          in
          decode_object fields_schema |> Result.map ~f:o_decode
        | String -> Coding.to_string v |> type_error "string"
        | String_const expected -> (
          match Coding.to_string v with
          | Some s when String.(s = expected) -> Result.return ()
          | Some s -> Result.fail (error "expected %S, got %S" expected s)
          | None -> type_error "string" None)
        | Time -> (
          match Coding.to_time v with
          | Result.Ok _ as ok -> ok
          | Result.Error None -> type_error "time" None
          | Result.Error (Some e) -> Result.fail (error "%s" e))
        | Time_local -> (
          match Coding.to_time_local v with
          | Result.Ok _ as ok -> ok
          | Result.Error None -> type_error "localtime" None
          | Result.Error (Some e) -> Result.fail (error "%s" e))
        | Tuple { items; decode = t_decode; _ } ->
          let* v = Coding.to_list v |> type_error "list" in
          let rec decode_tuple :
              type t.
              t Schema.tuple -> Coding.t list -> (t, E.decoding) Result.t =
           fun t v ->
            match (t, v) with
            | Item { schema; rest }, hd :: tl ->
              decode_schema context schema hd >>= fun hd ->
              decode_tuple rest tl >>= fun tl -> Result.return Hlist.(hd :: tl)
            | ItemEnd, [] -> Result.return Hlist.[]
            | Item _, [] -> Result.fail (error "not enough tuple elements")
            | ItemEnd, _ -> Result.fail (error "too many tuple elements")
          in
          decode_tuple items v |> Result.map ~f:t_decode
        | Union { cases; key; _ } -> (
          match key with
          | None -> (
            let conv (Schema.Case { decoder; schema; _ }) =
              match
                decode_descriptor
                  {
                    context with
                    schema = Option.first_some schema.id context.schema;
                  }
                  schema.descriptor v
              with
              | Result.Ok v -> Some (decoder v)
              | Result.Error _ -> None
            in
            match List.find_map ~f:conv cases with
            | Some res -> Result.return res
            | None ->
              Result.fail
                (error "no union case was matched for @[%a@]" Coding.pp v))
          | Some key -> (
            let* key_value =
              let* v =
                match key_get context v key with
                | Result.Ok (Some v) -> Result.return v
                | Result.Ok None ->
                  Result.Error (error "missing key field %S" key)
                | Result.Error e -> Result.Error e
              in
              Coding.to_string v |> type_error "string"
            in
            let conv (Schema.Case { decoder; name; schema; _ }) =
              if String.(name = key_value) then
                Option.return
                @@ let* v =
                     decode_descriptor
                       {
                         context with
                         schema = Option.first_some schema.id context.schema;
                       }
                       schema.descriptor v
                   in
                   Result.return (decoder v)
              else None
            in
            match List.find_map ~f:conv cases with
            | Some res -> res
            | None ->
              Result.fail (error "invalid key field %S = %S" key key_value)))
      and decode_schema :
          type t.
          E.decoding -> t Schema.t -> Coding.t -> (t, E.decoding) Result.t =
       fun context schema v ->
        decode_descriptor
          { context with schema = schema.id }
          schema.descriptor v
      in
      decode_schema { schema = None; path = []; reason = "" } schema
    in
    let[@landmark "Schematic.Coding.decode " ^ Coding.name] res v = decode v in
    res

  let encode schema =
    let encode =
      let rec encode_field :
          type req t.
          E.decoding ->
          (t, req) Schema.field ->
          req ->
          (string * Coding.t) option =
       fun context { name; requirement; schema; _ } ->
        let context = { context with path = name :: context.path } in
        let encode = encode_schema context (Schema.field_schema schema) in
        let optional v =
          match v with None -> None | Some v -> Some (name, encode v)
        in
        match requirement with
        | Computed -> optional
        | Computed_optional -> (
          fun v -> match v with None -> None | Some v -> optional v)
        | Default default ->
          let compare = Schema.(compare (field_schema schema)) in
          fun v -> if compare v default = 0 then None else Some (name, encode v)
        | Optional -> optional
        | Required -> fun v -> Some (name, encode v)
      and encode_descriptor :
          type t. E.decoding -> t Schema.descriptor -> t -> Coding.t =
       fun context schema ->
        match schema with
        | Any (module Any) -> (
          fun v ->
            match Coding.of_any (Any.to_any v) with
            | Some v -> v
            | None ->
              Fmt.failwith "%s encoder cannot encode %s" Coding.name Any.name)
        | Bool -> Coding.of_bool
        | Coding { f } -> encode_descriptor context (f (Some Coding.coding))
        | Date -> (
          match Coding.of_date with
          | `Generic -> encode_schema context date_tuple_schema
          | `Custom f -> f)
        | Fixpoint s ->
          (* Break infinite recursion. Would be better to have a fixpoint here
             too. *)
          fun v -> encode_descriptor context (Lazy.force s) v
        | Float -> Coding.of_float
        | Ignore -> Coding.null
        | Int -> Coding.of_int
        | Int64 -> Coding.of_int64
        | Json -> Coding.of_json
        | List items_schema ->
          let f = encode_schema context items_schema in
          fun v -> Coding.of_list (List.map ~f v)
        | Sequence { deconstruct; item; _ } ->
          let f = encode_schema context item in
          fun v ->
            v |> deconstruct |> Base.Sequence.map ~f |> Base.Sequence.to_list
            |> Coding.of_list
        | Map { encode = decons; descriptor; _ } ->
          let encode = encode_descriptor context descriptor in
          fun v -> encode @@ decons v
        | Null -> Coding.null
        | Nullable s ->
          let encode = encode_schema context s in
          fun v -> Option.map ~f:encode v |> Coding.of_option
        | Mapping fields_schema ->
          let encode = encode_schema context fields_schema in
          fun v ->
            Coding.of_fields (List.map ~f:(fun (name, v) -> (name, encode v)) v)
        | Object { fields; encode; _ } ->
          let f =
            Schema.Inline.value
              { f = (fun field req -> encode_field context field req) }
              fields
            |> Result.ok_or_failwith
          in
          fun v ->
            let fields = f (encode v) |> List.filter_map ~f:Fn.id in
            Coding.of_fields fields
        | Tuple { items; encode = t_encode; decode = _ } ->
          let encode =
            let rec encode_tuple : type t. t Schema.tuple -> t -> Coding.t list
                = function
              | ItemEnd -> fun _ -> []
              | Item { schema; rest } ->
                let encode = encode_schema context schema
                and encode_rest = encode_tuple rest in
                fun v ->
                  let Hlist.(hd :: tl) = v in
                  let hd = encode hd in
                  hd :: encode_rest tl
            in
            encode_tuple items
          in
          fun v -> Coding.of_list (encode (t_encode v))
        | String -> Coding.of_string
        | String_const v -> fun () -> Coding.of_string v
        | Time -> Coding.of_time
        | Time_local -> Coding.of_time_local
        | Union { cases; key; _ } -> (
          let cases =
            let f (Schema.Case { encoder; name; schema; _ }) =
              let encode = encode_schema context schema in
              fun v ->
                match encoder v with
                | Some v -> (
                  let v = encode v in
                  match key with
                  | Some key -> (
                    match key_set context v key (Coding.of_string name) with
                    | Result.Ok v -> Some v
                    | Result.Error _ -> Some v)
                  | None -> Some v)
                | None -> None
            in
            List.map ~f cases
          in
          fun v ->
            match List.find_map ~f:(fun case -> case v) cases with
            | Some res -> res
            | None ->
              Fmt.failwith "unmatched variant case in schema %a"
                Fmt.(option string)
                context.schema)
      and encode_schema : type t. E.decoding -> t Schema.t -> t -> Coding.t =
       fun context schema v ->
        encode_descriptor
          { context with schema = schema.id }
          schema.descriptor v
      in
      encode_schema { schema = None; path = []; reason = "" } schema
    in
    let[@landmark "Schematic.Coding.encode " ^ Coding.name] res v = encode v in
    res
end

module type STR = sig
  include S

  val content_type : string
  val of_string : string -> (t, E.parsing) Base.Result.t
  val to_string : t -> string
end

module Make_string (C : STR) : STR with type t = string = struct
  type t = string

  let name = C.name

  let[@landmark "Schematic.Coding.String.encode " ^ C.name] encode schema t =
    C.encode schema t |> C.to_string

  let[@landmark "Schematic.Coding.String.decode " ^ C.name] decode schema s =
    match C.of_string s with
    | Result.Ok t -> C.decode schema t
    | Result.Error msg ->
      Result.Error E.{ path = []; schema = None; reason = msg }

  let content_type = C.content_type
  let to_string s = s
  let of_string s = Result.return s
end

module Any (Coding : IMPL) = struct
  let schema =
    Schema.{ descriptor = Any (module Coding); id = None; parametric = None }

  let schema_versioned _ = schema
end
