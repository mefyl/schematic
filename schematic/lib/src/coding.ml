module E = Error
open Base

let map_results ~f l =
  let rec rev_results res = function
    | [] -> Result.return res
    | (Result.Error _ as error) :: _ -> error
    | Result.Ok hd :: tl -> rev_results (hd :: res) tl
  in
  List.rev_map ~f l |> rev_results []

type 'a customizable =
  [ `Generic
  | `Custom of 'a
  ]

type 't key_get = ('t -> string -> ('t option, string) Result.t) customizable
type 't key_set = ('t -> string -> 't -> ('t, string) Result.t) customizable

module type IMPL = sig
  type t

  val pp : t Fmt.t
  val key_get : t key_get
  val key_set : t key_set
  val null : unit -> t
  val of_bool : bool -> t
  val of_date : (Ptime.date -> t) customizable
  val of_fields : (string * t) list -> t
  val of_float : float -> t
  val of_int : int -> t
  val of_int64 : int64 -> t
  val of_json : Ezjsonm.value -> t
  val of_list : t list -> t
  val of_option : t option -> t
  val of_string : string -> t
  val of_time : Ptime.t -> t
  val of_time_local : Ptime.t * int -> t
  val to_bool : t -> bool option
  val to_date : (t -> Ptime.date option) customizable
  val to_fields : t -> (string * t) list option
  val to_float : t -> float option
  val to_int : t -> int option
  val to_int64 : t -> int64 option
  val to_json : t -> Ezjsonm.value option
  val to_list : t -> t list option
  val to_option : t -> t option option
  val to_string : t -> string option
  val to_time : t -> (Ptime.t, string option) Result.t
  val to_time_local : t -> (Ptime.t * int, string option) Result.t
  val type_name : t -> string
end

let fail id ids fmt =
  let f reason = E.{ path = ids; schema = id; reason } in
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

  val encode : 'a Schema.t -> 'a -> t
  val decode : 'a Schema.t -> t -> ('a, E.decoding) Result.t
end

module Make (Coding : IMPL) : S with type t = Coding.t = struct
  type t = Coding.t
  type date_tuple = int * int * int [@@deriving schema]

  let ( let* ) = Result.( >>= )

  let type_error id ids expected v = function
    | Some v -> Result.return v
    | None ->
      Result.fail @@ fail id ids "%s"
      @@ type_error expected (Coding.type_name v)

  let key_get schema path o f =
    match Coding.key_get with
    | `Custom impl ->
      impl o f |> Result.map_error ~f:(fun reason -> E.{ path; schema; reason })
    | `Generic ->
      let* fields = Coding.to_fields o |> type_error schema path "object" o in
      List.Assoc.find ~equal:String.equal fields f |> Result.return

  let key_set schema path o f v =
    match Coding.key_set with
    | `Custom impl ->
      impl o f v
      |> Result.map_error ~f:(fun reason -> E.{ path; schema; reason })
    | `Generic ->
      let* fields = Coding.to_fields o |> type_error schema path "object" v in
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
    let rec decode_field :
        type t r.
        bool ->
        string option ->
        string list ->
        string ->
        t Schema.t ->
        (t, r) Schema.requirement ->
        Coding.t option ->
        (r, 'error) Result.t =
     fun omit id ids field schema ->
      let ids_next = field :: ids in
      function
      | Default default -> (
        function
        | Some v -> (
          if omit then Result.return default
          else
            match (Coding.to_option v, schema.descriptor) with
            | Some None, Nullable nullable -> (
              match decode schema.id ids_next nullable.descriptor v with
              | Result.Ok v -> Result.return (Some v)
              | Result.Error _ -> decode schema.id ids_next schema.descriptor v)
            | Some None, _ -> decode schema.id ids_next schema.descriptor v
            | Some (Some v), _ -> decode schema.id ids_next schema.descriptor v
            | None, _ -> type_error id ids "option" v None)
        | None -> Result.return default)
      | Computed -> fun _ -> Result.Ok None
      | Computed_optional -> fun _ -> Result.Ok None
      | Optional -> (
        function
        | Some v -> (
          match Coding.to_option v with
          | Some None -> (
            match decode schema.id ids_next schema.descriptor v with
            | Result.Ok v -> Result.return (Some v)
            | Result.Error _ -> Result.return None)
          | Some (Some v) ->
            Result.map ~f:Option.return
            @@ decode schema.id ids_next schema.descriptor v
          | None -> type_error id ids "option" v None)
        | None -> Result.return None)
      | Required -> (
        function
        | Some v -> decode schema.id ids_next schema.descriptor v
        | None -> Result.fail (fail id ids "missing required field %S" field))
    and decode :
        type t.
        string option ->
        string list ->
        t Schema.descriptor ->
        Coding.t ->
        (t, E.decoding) Result.t =
     fun id ids schema v ->
      let type_error expected res = type_error id ids expected v res
      and error reason =
        let f reason = E.{ path = ids; schema = id; reason } in
        Fmt.kstr f reason
      and ( >>= ) = Result.( >>= ) in
      match schema with
      | Any -> Coding.to_json v |> type_error "JSON"
      | Bool -> Coding.to_bool v |> type_error "boolean"
      | Date -> (
        match Coding.to_date with
        | `Generic -> decode None ids date_tuple_schema.descriptor v
        | `Custom f -> f v |> type_error "date")
      | Fixpoint s -> decode id ids (Lazy.force s) v
      | Float -> Coding.to_float v |> type_error "float"
      | Ignore -> Result.return ()
      | Int -> Coding.to_int v |> type_error "integer"
      | Int64 -> Coding.to_int64 v |> type_error "integer"
      | List items_schema ->
        let* v = Coding.to_list v |> type_error "array" in
        map_results ~f:(decode None ids items_schema.descriptor) v
      | Map { decode = cons; descriptor; _ } ->
        decode id ids descriptor v >>= fun value ->
        cons value |> Result.map_error ~f:(fun reason -> error "%s" reason)
      | Mapping fields_schema ->
        let* fields = Coding.to_fields v |> type_error "mapping" in
        let f (name, value) =
          decode fields_schema.id ids fields_schema.descriptor value
          >>= fun value -> Result.return (name, value)
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
          decode s.id ids s.descriptor v |> Result.map ~f:Option.return
        | None -> type_error "option" None)
      | Object { fields = fields_schema; decode = o_decode; _ } ->
        let* fields = Coding.to_fields v |> type_error "object" in
        let rec decode_object :
            type t. t Schema.fields -> (t, E.decoding) Result.t = function
          | FieldEnd -> Result.return ()
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
              Result.Ok (requirement_return requirement (), rest)
            | schema ->
              let field =
                let value =
                  let inline Schema.{ descriptor; _ } =
                    let missing = missing descriptor fields
                    and inlinable =
                      Schema.Inline.schema descriptor { f = (fun _ -> ()) }
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
                decode_field omit id ids field_name schema requirement value
              in
              Result.Let_syntax.Let_syntax.both field (decode_object rest))
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
            type t. t Schema.tuple -> Coding.t list -> (t, E.decoding) Result.t
            =
         fun t v ->
          match (t, v) with
          | Item { schema; rest }, hd :: tl ->
            decode schema.id ids schema.descriptor hd >>= fun hd ->
            decode_tuple rest tl >>= fun tl -> Result.return (hd, tl)
          | ItemEnd, [] -> Result.return ()
          | Item _, [] -> Result.fail (error "not enough tuple elements")
          | ItemEnd, _ -> Result.fail (error "too many tuple elements")
        in
        decode_tuple items v |> Result.map ~f:t_decode
      | Union { cases; key; _ } -> (
        match key with
        | None -> (
          let conv (Schema.Case { decoder; schema; _ }) =
            let id = Option.first_some schema.id id in
            match decode id ids schema.descriptor v with
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
              match key_get id ids v key with
              | Result.Ok (Some v) -> Result.return v
              | Result.Ok None ->
                Result.Error (error "missing key field %S" key)
              | Result.Error e -> Result.Error e
            in
            Coding.to_string v |> type_error "string"
          in
          let conv (Schema.Case { decoder; name; schema; _ }) =
            let id = Option.first_some schema.id id in
            if String.(name = key_value) then
              Option.return
              @@ let* v = decode id ids schema.descriptor v in
                 Result.return (decoder v)
            else None
          in
          match List.find_map ~f:conv cases with
          | Some res -> res
          | None ->
            Result.fail (error "invalid key field %S = %S" key key_value)))
    in
    decode schema.Schema.id [] schema.descriptor

  let encode schema =
    let rec encode_field :
        type req t. (t, req) Schema.field -> req -> (string * Coding.t) option =
     fun { name; requirement; schema; _ } ->
      let compare = Schema.(compare (field_schema schema))
      and encode = encode_schema (Schema.field_schema schema) in
      let optional v =
        match v with None -> None | Some v -> Some (name, encode v)
      in
      match requirement with
      | Computed -> optional
      | Computed_optional -> (
        fun v -> match v with None -> None | Some v -> optional v)
      | Default default ->
        fun v -> if compare v default = 0 then None else Some (name, encode v)
      | Optional -> optional
      | Required -> fun v -> Some (name, encode v)
    and encode_descriptor :
        type t. string option -> t Schema.descriptor -> t -> Coding.t =
     fun id schema ->
      match schema with
      | Any -> Coding.of_json
      | Bool -> Coding.of_bool
      | Date -> (
        match Coding.of_date with
        | `Generic -> encode_schema date_tuple_schema
        | `Custom f -> f)
      | Fixpoint s ->
        (* Break infinite recursion. Would be better to have a fixpoint here
           too. *)
        fun v -> encode_descriptor id (Lazy.force s) v
      | Float -> Coding.of_float
      | Ignore -> Coding.null
      | Int -> Coding.of_int
      | Int64 -> Coding.of_int64
      | List items_schema ->
        let f = encode_schema items_schema in
        fun v -> Coding.of_list (List.map ~f v)
      | Map { encode = decons; descriptor; _ } ->
        let encode = encode_descriptor id descriptor in
        fun v -> encode @@ decons v
      | Null -> Coding.null
      | Nullable s ->
        let encode = encode_schema s in
        fun v -> Option.map ~f:encode v |> Coding.of_option
      | Mapping fields_schema ->
        let encode = encode_schema fields_schema in
        fun v ->
          Coding.of_fields (List.map ~f:(fun (name, v) -> (name, encode v)) v)
      | Object { fields; encode; _ } ->
        let f =
          Schema.Inline.value { f = encode_field } fields
          |> Result.ok_or_failwith
        in
        fun v ->
          let fields = f (encode v) |> List.filter_map ~f:Fn.id in
          Coding.of_fields fields
      | Tuple { items; encode = t_encode; decode = _ } ->
        let encode =
          let rec encode_tuple : type t. t Schema.tuple -> t -> Coding.t list =
            function
            | ItemEnd -> fun _ -> []
            | Item { schema; rest } ->
              let encode = encode_schema schema
              and encode_rest = encode_tuple rest in
              fun v ->
                let hd, tl = v in
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
            let encode = encode_schema schema in
            fun v ->
              match encoder v with
              | Some v -> (
                let v = encode v in
                match key with
                | Some key -> (
                  match
                    key_set None [ (* FIXME: context *) ]
                      v key (Coding.of_string name)
                  with
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
              id)
    and encode_schema : type t. t Schema.t -> t -> Coding.t =
     fun schema v -> encode_descriptor schema.id schema.descriptor v
    in
    encode_schema schema
end

module type STR = sig
  include S

  val content_type : string
  val of_string : string -> (t, E.parsing) Base.Result.t
  val to_string : t -> string
end

module Make_string (C : STR) : STR with type t = string = struct
  type t = string

  let encode schema t = C.encode schema t |> C.to_string

  let decode schema s =
    match C.of_string s with
    | Result.Ok t -> C.decode schema t
    | Result.Error msg ->
      Result.Error E.{ path = []; schema = None; reason = msg }

  let content_type = C.content_type
  let to_string s = s
  let of_string s = Result.return s
end
