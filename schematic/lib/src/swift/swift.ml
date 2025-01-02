module E = Error
open Base

let src = Logs.Src.create ~doc:"schematic Swift" "schematic"

module Logs = (val Logs.src_log src)

exception NotYetImplemented of string

(** Convenience for hacking because Schematic does not provide it. *)
let pp_debug_descriptor : type a. a Schema.descriptor Fmt.t =
 fun f descriptor ->
  Fmt.pf f "%s"
  @@
  match descriptor with
  | Any _ -> "Any"
  | Bool -> "Bool"
  | Coding _ -> "Coding"
  | Date -> "Date"
  | Fixpoint _ -> "Fixpoint"
  | Float -> "Float"
  | Ignore -> "Ignore"
  | Int -> "Int"
  | Int64 -> "Int64"
  | Json -> "Json"
  | List _ -> "List"
  | Map _ -> "Map"
  | Mapping _ -> "Mapping"
  | Null -> "Null"
  | Nullable _ -> "Nullable"
  | Object _ -> "Object"
  | String -> "String"
  | String_const _ -> "String_const"
  | Time -> "Time"
  | Time_local -> "Time_local"
  | Tuple _ -> "Tuple"
  | Union _ -> "Union"

let to_pascal_case s =
  String.split_on_chars ~on:[ '_'; '-'; '.'; '\''; '$' ] s
  |> List.map ~f:String.capitalize
  |> String.concat ~sep:""

let to_camel_case s = to_pascal_case s |> String.uncapitalize

(** Reserved keywords in Swift. See
    https://docs.swift.org/swift-book/documentation/the-swift-programming-language/lexicalstructure/#Keywords-and-Punctuation *)
let swift_reserved_words =
  Set.of_list
    (module String)
    [
      "associatedtype";
      "borrowing";
      "class";
      "consuming";
      "deinit";
      "enum";
      "extension";
      "fileprivate";
      "func";
      "import";
      "init";
      "inout";
      "internal";
      "let";
      "nonisolated";
      "open";
      "operator";
      "private";
      "precedencegroup";
      "protocol";
      "public";
      "rethrows";
      "static";
      "struct";
      "subscript";
      "typealias";
      "var";
      "break";
      "case";
      "catch";
      "continue";
      "default";
      "defer";
      "do";
      "else";
      "fallthrough";
      "for";
      "guard";
      "if";
      "in";
      "repeat";
      "return";
      "throw";
      "switch";
      "where";
      "while";
      "Any";
      "as";
      "await";
      "catch";
      "false";
      "is";
      "nil";
      "rethrows";
      "self";
      "Self";
      "super";
      "throw";
      "throws";
      "true";
      "try";
      "_";
      "#available";
      "#colorLiteral";
      "#else";
      "#elseif";
      "#endif";
      "#fileLiteral";
      "#if";
      "#imageLiteral";
      "#keyPath";
      "#selector";
      "#sourceLocation";
      "#unavailable";
      "associativity";
      "convenience";
      "didSet";
      "dynamic";
      "final";
      "get";
      "indirect";
      "infix";
      "lazy";
      "left";
      "mutating";
      "none";
      "nonmutating";
      "optional";
      "override";
      "package";
      "postfix";
      "precedence";
      "prefix";
      "Protocol";
      "required";
      "right";
      "set";
      "some";
      "Type";
      "unowned";
      "weak";
      "willSet";
    ]

(** The JSON object field storing the data type for [Union]s. *)
let union_type_discriminator = "#type"

(** The JSON object field storing the value for [Union]s. *)
let union_type_value = "value"

(** Transform the given string into a valid Swift object field name. *)
let to_object_field_name s =
  let s = to_camel_case s in
  if Set.mem swift_reserved_words s then s ^ "_" else s

type swift_type =
  | Simple of string  (** E.g. [Foo] *)
  | Generic of string * swift_type list
      (** E.g.
          [Generic ("Bar", [Generic ("List", [Simple "Boolean"]); Simple "String"])]
          is [Bar<List<Boolean>, String>] *)
[@@deriving show]

type swift_struct = {
  generics : swift_type list;
  fields : (string * swift_type) list;
}

and swift_union = {
  generics : swift_type list;
  cases : (string * (string option * swift_type) list) list;
}
[@@deriving show]

type swift =
  | Atom of swift_type  (** Type alias or basic type *)
  | Struct of swift_struct  (** A record *)
  | Enum of swift_union
[@@deriving show]

(** E.g. for [type foo = A of {bar: string} | B of Int], the [A] branch will
    become [Inline] (because we don't have any type name to refer to), and [B]
    will be [Wrap], because we wrap an existing type. *)
and swift_case =
  (* TODO Add generics *)
  | Inline of swift
  | Wrap of swift_type

let empty_struct = Struct { generics = []; fields = [] }

(** Schematic cannot depend on Timmy so we copy/paste the implementation of
    functions we need to interact with Ptime dates *)
module Date = struct
  let to_string (year, month, day) = Fmt.str "%04i-%02i-%02i" year month day

  let int_of_string msg s =
    try Result.return @@ Int.of_string s
    with _ -> Result.failf "invalid %s: %s" msg s

  let of_string s =
    match String.split ~on:'-' s with
    | [ y; m; d ] ->
      let ( let* ) = Result.( >>= ) in
      let* y = int_of_string "year" y in
      let* m = int_of_string "month" m in
      let* d = int_of_string "day" d in
      Ok (y, m, d)
    | _ -> Result.fail Fmt.(str "invalid date: %s" s)
end

(** Module responsible for parsing Schematic schemas into a Swift AST. *)
module Parser = struct
  module Marks = struct
    include Map.M (String)

    type 'a with_parsing_status =
      | InProgress
      | Done of 'a

    (** The map key is a Swift type. It must NOT contain generic (parametric)
        information. Indeed [Foo<A>] and [Foo<B>] represent the same type
        declaration, so we must not declare it twice, and it is useless to parse
        it twice. This is why generic information is stored in the [swift] type
        expression only. In this case, [name = "Foo"]. *)
    type nonrec t = swift with_parsing_status t
  end

  let wrap_in_option : swift_type -> swift_type =
   fun swift_type -> Generic ("Maybe", [ swift_type ])

  (** Try to find an adequate Swift type name. *)
  let type_name_and_swift_to_swift_type_exn :
      string option -> swift -> swift_type =
   fun type_name swift ->
    match (type_name, swift) with
    | Some type_name, _ -> Simple type_name
    | None, Atom swift_type -> swift_type
    | _ ->
      Fmt.failwith "I don't know how to extract a Swift type from: %a\n"
        pp_swift swift

  (** [schema_to_swift_marks marks schema] returns:
      - the Swift type name of the schema (if any, and without generics)
      - the Swift type expression of this [schema]
      - the input [marks] enhanced with additional Swift type expressions found
        while parsing [schema]. *)
  let rec schema_to_swift_marks :
      type a. Marks.t -> a Schema.t -> string option * swift * Marks.t =
   fun marks ({ id; descriptor; parametric } as schema) ->
    let () =
      Logs.debug (fun m ->
          m "schema_to_swift_marks: Parsing schema %a" Schema.pp schema)
    in
    try
      let swift_type_name = Option.map ~f:to_pascal_case id in
      let _generics, marks =
        (* TODO generics *)
        match parametric with
        | None -> ([], marks)
        | Some parametric -> parametric_to_swift_marks marks parametric
      in
      match Option.map ~f:(Map.find marks) swift_type_name with
      | None ->
        (* No Swift type name: parse *)
        let () =
          Logs.debug (fun m ->
              m "schema_to_swift_marks: Start parsing unnamed schema")
        in
        let swift, marks = descriptor_to_swift_marks marks descriptor in
        (swift_type_name, swift, marks)
      | Some None ->
        (* Swift type name but not present in the marks: add [InProgress] in
           case we recursively try to parse the same schema while parsing it,
           and parse. *)
        let swift_type_name = Option.value_exn swift_type_name in
        let () =
          Logs.debug (fun m ->
              m "schema_to_swift_marks: Start parsing %s" swift_type_name)
        in
        let marks = Map.add_exn ~key:swift_type_name ~data:InProgress marks in
        let swift, marks = descriptor_to_swift_marks marks descriptor in
        let marks = Map.set ~key:swift_type_name ~data:(Done swift) marks in
        (Some swift_type_name, swift, marks)
      | Some (Some InProgress) ->
        (* Swift type name and parsing in progress: return the type name but
           don't parse *)
        let swift_type_name = Option.value_exn swift_type_name in
        let () =
          Logs.debug (fun m ->
              m "schema_to_swift_marks: InProgress, returning %s"
                swift_type_name)
        in
        ( Some swift_type_name,
          Atom (Simple swift_type_name (* TODO generics *)),
          marks )
      | Some (Some (Done swift)) ->
        let () =
          Logs.debug (fun m ->
              m "schema_to_swift_marks: Done, returning %a"
                Fmt.(pair (option string) pp_swift)
                (swift_type_name, swift))
        in
        (* Swift type name and parsing is done: return the parsing result *)
        (swift_type_name, swift, marks)
    with NotYetImplemented s ->
      failwith
      @@ Fmt.str "Not yet supported: %s for schema %a" s Schema.pp schema

  and descriptor_to_swift_marks :
      type a. Marks.t -> a Schema.descriptor -> swift * Marks.t =
   fun marks descriptor ->
    match descriptor with
    | Any (module Coding) -> raise (NotYetImplemented "Any")
    | Bool -> (Atom (Simple "Bool"), marks)
    | Coding { f = _ } -> raise (NotYetImplemented "Coding")
    | Date -> (Atom (Simple "LocalDate"), marks)
    | Fixpoint lazy_descriptor ->
      descriptor_to_swift_marks marks (Lazy.force lazy_descriptor)
    | Float -> (Atom (Simple "Float"), marks)
    | Ignore ->
      (empty_struct, marks)
      (* Treat [Ignore] fields as an empty object that will be removed in a
         later pass *)
    | Int -> (Atom (Simple "Int"), marks)
    | Int64 -> (Atom (Simple "Int64"), marks)
    | Json -> (Atom (Simple "JsonElement"), marks)
    | List sub_schema ->
      (* TODO We will probably need [swift] to support generics *)
      let type_name, swift, marks = schema_to_swift_marks marks sub_schema in
      let swift_type = type_name_and_swift_to_swift_type_exn type_name swift in
      let swift = Atom (Generic ("Array", [ swift_type ])) in
      (swift, marks)
    | Map { descriptor; _ } -> descriptor_to_swift_marks marks descriptor
    | Mapping value_schema ->
      (* TODO We will probably need [swift] to support generics *)
      let type_name, swift, marks = schema_to_swift_marks marks value_schema in
      let swift_type = type_name_and_swift_to_swift_type_exn type_name swift in
      let swift =
        Atom (Generic ("Dictionary", [ Simple "String"; swift_type ]))
      in
      (swift, marks)
    | Null -> (Atom (Simple "Unit"), marks)
    | Nullable schema ->
      let type_name, swift, marks = schema_to_swift_marks marks schema in
      let () =
        Logs.debug (fun m ->
            m "descriptor_to_swift_marks: Parsing Nullable for type_name %a"
              Fmt.(option string)
              type_name)
      in
      let res =
        match (type_name, swift) with
        | None, Atom swift_type -> (Atom (wrap_in_option swift_type), marks)
        | None, _swift ->
          raise
            (NotYetImplemented
               (Fmt.str
                  "We are trying to mark as nullable a type without name and \
                   that is not an Atom: %a"
                  pp_swift swift))
        | Some type_name, _swift ->
          (Atom (wrap_in_option (Simple type_name)), marks)
      in
      let () =
        Logs.debug (fun m ->
            m "descriptor_to_swift_marks: Nullable result: %a"
              Fmt.(pair pp_swift nop)
              res)
      in
      res
      (* TODO Generics *)
    | Object { fields = FieldEnd; _ } -> (empty_struct, marks)
    | Object { fields; _ } ->
      let swift_fields, marks = object_fields_to_swift_marks marks fields in
      (Struct { fields = swift_fields; generics = [] }, marks)
    | String -> (Atom (Simple "String"), marks)
    | String_const _ -> (empty_struct, marks)
    | Time -> (Atom (Simple "Date"), marks)
    | Time_local -> (Atom (Simple "ZonedDate"), marks)
    | Tuple { items; _ } ->
      let swift_type, marks = tuple_to_swift_marks marks items in
      (Atom swift_type, marks)
    | Union { key = _; cases; polymorphic = _ } ->
      (* For simplicity, we ignore the optional union key *)
      let marks, cases =
        List.fold_right ~init:(marks, [])
          ~f:(fun
              (Case { name = case_name; schema; _ })
              (marks, rest_nested_types)
            ->
            let case_name = to_pascal_case case_name in
            let type_name, swift, marks = schema_to_swift_marks marks schema in
            let nested_type =
              match (type_name, swift) with
              | Some type_name, _swift ->
                (case_name, [ (None, Simple type_name) ])
              | None, Atom swift_type -> (case_name, [ (None, swift_type) ])
              | None, Struct { fields; _ } ->
                (case_name, List.map fields ~f:(fun (name, t) -> (Some name, t)))
              | None, _ ->
                raise
                  (NotYetImplemented
                     (Fmt.str "Inline types in enum not supported, got %a"
                        pp_swift swift))
            in
            let nested_types = nested_type :: rest_nested_types in
            (marks, nested_types))
          cases
      in
      let swift = Enum { cases; generics = [ (* TODO *) ] } in
      (swift, marks)

  and tuple_to_swift_marks :
      type a. Marks.t -> a Schema.tuple -> swift_type * Marks.t =
    let tuple_element_to_swift_type marks schema =
      let type_name, swift, marks = schema_to_swift_marks marks schema in
      let swift_type = type_name_and_swift_to_swift_type_exn type_name swift in
      (swift_type, marks)
    in
    fun marks items ->
      match items with
      | Item
          {
            schema = schema_1;
            rest = Item { schema = schema_2; rest = ItemEnd };
          } ->
        let swift_type_1, marks = tuple_element_to_swift_type marks schema_1 in
        let swift_type_2, marks = tuple_element_to_swift_type marks schema_2 in
        (Generic ("Tuple2", [ swift_type_1; swift_type_2 ]), marks)
      | Item
          {
            schema = schema_1;
            rest =
              Item
                {
                  schema = schema_2;
                  rest = Item { schema = schema_3; rest = ItemEnd };
                };
          } ->
        let swift_type_1, marks = tuple_element_to_swift_type marks schema_1 in
        let swift_type_2, marks = tuple_element_to_swift_type marks schema_2 in
        let swift_type_3, marks = tuple_element_to_swift_type marks schema_3 in
        (Generic ("Tuple3", [ swift_type_1; swift_type_2; swift_type_3 ]), marks)
      | _ ->
        raise (NotYetImplemented "Only tuples of 2 or 3 elements are supported")

  and parametric_to_swift_marks :
      type a. Marks.t -> a Schema.parametric -> swift_type list * Marks.t =
   fun marks parametric ->
    match parametric with
    | Plain { name; descriptor } ->
      let _swift, marks = descriptor_to_swift_marks marks descriptor in
      (* TODO do we need this [swift] type? *)
      ([ Simple name ], marks)
    | Parametric { name; parameter; next } ->
      (* TODO do we need this parameter type and swift? *)
      let _parameter_type_name, _swift, marks =
        schema_to_swift_marks marks parameter
      in
      let other_types, marks =
        parametric_to_swift_marks marks (next parameter)
      in
      (Simple name :: other_types, marks)

  and object_fields_to_swift_marks :
      type a.
      Marks.t -> a Hlist.t Schema.fields -> (string * swift_type) list * Marks.t
      =
   fun marks fields ->
    match fields with
    | FieldEnd -> ([], marks)
    | Field
        {
          field =
            {
              name;
              schema =
                ( Outline { descriptor = Ignore; _ }
                | Inline { descriptor = Ignore; _ }
                | Inline_try { descriptor = Ignore; _ } );
              _;
            };
          rest;
        } ->
      let () =
        Logs.debug (fun m ->
            m "object_fields_to_swift_marks: Ignoring field %s" name)
      in
      object_fields_to_swift_marks marks rest
    | Field { field; rest } ->
      let swift_field, marks = object_field_to_swift_marks marks field in
      let swift_fields, marks = object_fields_to_swift_marks marks rest in
      (swift_field :: swift_fields, marks)

  and object_field_to_swift_marks :
      type t req.
      Marks.t -> (t, req) Schema.field -> (string * swift_type) * Marks.t =
   fun marks
       {
         name;
         requirement;
         (* For Swift, we do not care about inlining, we decided to preserve the
            same structure as the original OCaml type. *)
         schema = Outline schema | Inline schema | Inline_try schema;
         _;
       } ->
    let wrap_in_options swift_type =
      match requirement with
      | Required | Default _ -> swift_type
      | Computed | Optional -> wrap_in_option swift_type
      | Computed_optional -> wrap_in_option (wrap_in_option swift_type)
    in
    let swift_type_name, swift, marks = schema_to_swift_marks marks schema in
    let () =
      Logs.debug (fun m ->
          m "object_field_to_swift_marks: swift_type_name: %a"
            Fmt.(option string)
            swift_type_name)
    in
    let swift_type, marks =
      match (swift_type_name, swift) with
      | Some swift_type_name, _ -> (Simple swift_type_name, marks)
      | None, Atom (Simple type_name) -> (Simple type_name, marks)
      | None, Atom (Generic (type_name, generics)) ->
        (Generic (type_name, generics), marks)
      | None, Enum { cases = nested_data_classes; _ } ->
        (* Corner case: there is one (1) occurrence in the whole RPC type
           hierarchy where an object field refers to an anonymous schema:
           [graph-error]. In best effort, we try to craft a unique-ish enough
           name and create an associated type. *)
        let id =
          nested_data_classes
          |> List.map ~f:(fun (case_name, _swift_case) -> case_name)
          |> String.concat ~sep:"-or-" |> to_pascal_case
        in
        let swift_type_name, _swift, marks =
          schema_to_swift_marks marks { schema with id = Some id }
        in
        (Simple (Option.value_exn swift_type_name), marks)
      | _ ->
        Fmt.failwith
          "Hopefully unreachable: the object field %S has no type name: %a" name
          pp_swift swift
    in
    let swift_type = wrap_in_options swift_type in
    let () =
      Logs.debug (fun m ->
          m "object_field_to_swift_marks: Result: %a" pp_swift_type swift_type)
    in
    ((to_object_field_name name, swift_type), marks)

  let packed_schema_to_swift_marks marks packed_schema =
    Schema.unpack
      { unpack = (fun schema -> schema_to_swift_marks marks schema) }
      packed_schema

  let big_union_of (marks : string list) : swift Marks.with_parsing_status =
    let name_to_case (name : string) = (name, [ (None, Simple name) ]) in
    let cases = marks |> List.map ~f:name_to_case in
    Marks.Done (Enum { generics = []; cases })
end

(** Module responsible for generating actual Swift code from the parser result. *)
module Generator = struct
  let original_pp_swift_type = pp_swift_type

  (** Pretty-print the given list, or nothing if the list is empty *)
  let pp_optional_list ~open_delim ~close_delim ?(sep = Fmt.comma) pp_elt :
      'a list Fmt.t =
   fun fmt list ->
    match list with
    | [] -> Fmt.pf fmt ""
    | _ ->
      Fmt.pf fmt "@[<1>%s%a%s@]" open_delim (Fmt.list ~sep pp_elt) list
        close_delim

  (** Pretty-print a single field inside a [struct] *)
  let rec pp_field : (string * swift_type) Fmt.t =
   fun f (field_name, field_type) ->
    Fmt.pf f "let %s: %a" field_name pp_swift_type field_type

  (** Pretty-print a single [case] inside an [enum] *)
  and pp_case : (string * (string option * swift_type) list) Fmt.t =
   fun f (case_name, case_args) ->
    let pp_case_arg fmt ((arg_name, case_arg) : string option * swift_type) =
      let pp_name fmt = function
        | None -> Fmt.pf fmt ""
        | Some name -> Fmt.pf fmt "%s: " name
      in
      Fmt.pf fmt "%a%a" pp_name arg_name pp_swift_type case_arg
    in
    Fmt.pf f "case %s%a" case_name
      (pp_optional_list ~open_delim:"(" ~close_delim:")" pp_case_arg)
      case_args

  and pp_swift_type : swift_type Fmt.t =
   fun f swift_type ->
    let () =
      Logs.debug (fun m ->
          m "pp_swift_type: Pretty-printing swift type %a"
            original_pp_swift_type swift_type)
    in
    match swift_type with
    | Simple s -> Fmt.pf f "%s" s
    | Generic (s, generics) -> Fmt.pf f "%s%a" s pp_generics generics

  and pp : (string * swift Parser.Marks.with_parsing_status) Fmt.t =
   fun f (name, swift) ->
    match swift with
    | InProgress ->
      Fmt.failwith
        "Parsing is complete but we still have In Progress parsing for type %s"
        name
    | Done (Atom alias) -> Fmt.pf f "typealias %s = %a" name pp_swift_type alias
    | Done (Struct { fields; generics }) ->
      Fmt.pf f "@[<v 2>class %s%a : Codable {@;%a@]@;}@;" name pp_generics
        generics
        (Fmt.list ~sep:Fmt.cut pp_field)
        fields
    | Done (Enum { cases; generics }) ->
      Fmt.pf f "@[<v 2>enum %s%a : Codable {@;%a@]@;}@;" name pp_generics
        generics
        (Fmt.list ~sep:Fmt.cut pp_case)
        cases

  (** Special case if the list of generics is empty as [Foo<>] is illegal in
      Swift. *)
  and pp_generics : swift_type list Fmt.t =
   fun f swift_types ->
    pp_optional_list ~open_delim:"<" ~close_delim:">" pp_swift_type f
      swift_types

  let preamble : string =
    {|
import Foundation

// Isomorphic to `nil`, but using a more predictible JSON encoding
struct Unit : Codable {}

// Sad but required: Although Swift has native tuples, these are not `Codable`,
// so we can't use them in the protocol, and we have to define our own ugly things
struct Tuple2<T0 : Codable, T1 : Codable> : Codable {
  let _0 : T0
  let _1 : T1
}
struct Tuple3<T0 : Codable, T1 : Codable, T2 : Codable> : Codable {
  let _0 : T0
  let _1 : T1
  let _2 : T2
}

// Isomorphic to `Optional`, but with a more regular JSON encoding
struct Maybe<T : Codable> : Codable {
  let value : Optional<T>

  // Custom encoding because the default one will just omit null values, while
  // we'd rather have an explicit `null`
  func encode(to encoder: Encoder) throws {
    var container = encoder.container(keyedBy: CodingKeys.self)
    try container.encode(value, forKey: .value)
  }
}

struct ZonedDate : Codable {
  let date : Date
  let gmt_offset : Int
}
  |}
end

(** Module responsible for encoding into/decoding from Swift-compatible JSON. *)
module Json = struct
  (** [with_sorted_keys f json] is [f json'] where [json'] is similar to [json],
      except that if it is an object then its keys are sorted.

      This is useful for pattern-matching in [f] independently of the order of
      the keys. *)
  let with_sorted_keys f (json : Ezjsonm.value) =
    let sort_keys = function
      | `O keys ->
        `O
          (List.sort
             ~compare:(fun (k1, _) (k2, _) -> String.compare k1 k2)
             keys)
      | json -> json
    in
    f (sort_keys json)

  module Encode = struct
    let rec encode : type t. t Schema.t -> t -> Ezjsonm.value =
     fun { descriptor; id; _ } t ->
      let json = encode_descriptor descriptor t in
      let () =
        Logs.debug (fun m ->
            m "encode: Encoded value for schema %a: %a"
              Fmt.(option string)
              id Json.pp json)
      in
      json

    and encode_descriptor : type t. t Schema.descriptor -> t -> Ezjsonm.value =
     fun descriptor ->
      match descriptor with
      | Any (module Coding) ->
        raise
          (NotYetImplemented
             (Fmt.str "encode does not support descriptor %a"
                pp_debug_descriptor descriptor))
      | Bool -> fun v -> `Bool v
      | Coding { f = _ } ->
        raise
          (NotYetImplemented
             (Fmt.str "encode does not support descriptor %a"
                pp_debug_descriptor descriptor))
      | Date -> fun v -> v |> Date.to_string |> encode Schemas.string_schema
      | Fixpoint lazy_descriptor ->
        fun v -> encode_descriptor (Lazy.force lazy_descriptor) v
      | Float -> fun v -> `Float v
      | Ignore -> Fn.const `Null
      | Int -> fun v -> `Float (Float.of_int v)
      | Int64 -> fun v -> `Float (Float.of_int64 v)
      | Json -> Fn.id
      | List sub_schema ->
        let f = encode sub_schema in
        fun v ->
          let sub_values = List.map ~f v in
          `A sub_values
      | Map { descriptor; encode = decons; _ } ->
        let encode = encode_descriptor descriptor in
        fun v -> encode (decons v)
      | Mapping value_schema ->
        let encode = encode value_schema in
        fun v ->
          let pairs = List.map ~f:(fun (k, v) -> (k, encode v)) v in
          `O pairs
      | Null -> Fn.const (`O [ ("Unit", `O []) ])
      | Nullable schema ->
        let f = encode schema in
        fun v ->
          `O [ ("value", Option.map ~f v |> Option.value ~default:`Null) ]
      | Object { encode; fields; _ } ->
        fun v ->
          let fields_hlist = encode v in
          `O (encode_fields fields fields_hlist)
      | String -> fun v -> `String v
      | String_const _s -> Fn.const (`O [])
      | Time -> fun v -> `Float (Ptime.to_float_s v)
      | Time_local ->
        fun (t, tz_offset_s) ->
          `O
            [
              ("date", `Float (Ptime.to_float_s t));
              ("gmt_offset", `Float (Int.to_float tz_offset_s));
            ]
      | Tuple { encode; items; _ } ->
        fun v ->
          let items_hlist = encode v in
          encode_tuple items items_hlist
      | Union { cases; _ } -> (
        (* For simplicity and consistency with the Swift generated types, we
           ignore the optional union key *)
        let cases : (t -> Ezjsonm.value option) list =
          let f (Schema.Case { encoder; name; schema; _ }) =
            let encode = encode schema in
            fun v ->
              match encoder v with
              | Some v -> (
                match (schema.id, schema.descriptor) with
                | ( None,
                    Object _ (* Inline record, directly encode as the body *) )
                  ->
                  Some (`O [ (to_pascal_case name, encode v) ])
                | _ ->
                  Some (`O [ (to_pascal_case name, `O [ ("_0", encode v) ]) ]))
              | None -> None
          in
          List.map ~f cases
        in
        fun v ->
          match List.find_map ~f:(fun case -> case v) cases with
          | Some res -> res
          | None -> Fmt.failwith "unmatched variant case in schema")

    and encode_tuple :
        type t. t Hlist.t Schema.tuple -> t Hlist.t -> Ezjsonm.value =
     fun items items_hlist ->
      match (items, items_hlist) with
      | ( Item
            {
              schema = schema_1;
              rest = Item { schema = schema_2; rest = ItemEnd };
            },
          [ element_1; element_2 ] ) ->
        let encoded_1 = encode schema_1 element_1 in
        let encoded_2 = encode schema_2 element_2 in
        `O [ ("_0", encoded_1); ("_1", encoded_2) ]
      | ( Item
            {
              schema = schema_1;
              rest =
                Item
                  {
                    schema = schema_2;
                    rest = Item { schema = schema_3; rest = ItemEnd };
                  };
            },
          [ element_1; element_2; element_3 ] ) ->
        let encoded_1 = encode schema_1 element_1 in
        let encoded_2 = encode schema_2 element_2 in
        let encoded_3 = encode schema_3 element_3 in
        `O [ ("_0", encoded_1); ("_1", encoded_2); ("_2", encoded_3) ]
      | _, _ ->
        raise (NotYetImplemented "Only tuples of 2 or 3 elements are supported")

    and encode_fields :
        type t.
        t Hlist.t Schema.fields -> t Hlist.t -> (string * Ezjsonm.value) list =
     fun fields fields_hlist ->
      match (fields, fields_hlist) with
      | FieldEnd, [] -> []
      | ( (Field
             {
               field =
                 {
                   name;
                   schema =
                     ( Outline { descriptor = Ignore; _ }
                     | Inline { descriptor = Ignore; _ }
                     | Inline_try { descriptor = Ignore; _ } );
                   _;
                 };
               rest;
             } :
            t Hlist.t Schema.fields),
          _ :: fields_hlist_rest ) ->
        let () =
          Logs.debug (fun m ->
              m "encode_fields: Ignoring field %s while encoding" name)
        in
        let encoded_tail = encode_fields rest fields_hlist_rest in
        encoded_tail
      | ( (Field { field; rest } : t Hlist.t Schema.fields),
          field_hlist_head :: fields_hlist_rest ) ->
        let encoded_head = encode_field field field_hlist_head in
        let encoded_tail = encode_fields rest fields_hlist_rest in
        encoded_head :: encoded_tail

    and encode_field :
        type t req. (t, req) Schema.field -> req -> string * Ezjsonm.value =
     fun { name; requirement; schema; _ } ->
      let name = to_object_field_name name in
      let encode = encode (Schema.field_schema schema) in
      let optional v =
        match v with
        | None -> (name, `O [ ("value", `Null) ])
        | Some v -> (name, `O [ ("value", encode v) ])
      in
      match requirement with
      | Optional -> optional
      | Required -> fun v -> (name, encode v)
      | Computed -> optional
      | Computed_optional -> (
        fun v ->
          match v with
          | None -> (name, `O [ ("value", `Null) ])
          | Some None -> (name, `O [ ("value", `O [ ("value", `Null) ]) ])
          | Some (Some v) -> (name, `O [ ("value", `O [ ("value", encode v) ]) ])
        )
      | Default _default -> fun v -> (name, encode v)
  end

  module Decode = struct
    let ( let+ ) = Result.( >>| )
    let ( let* ) = Result.( >>= )

    let requirement_return : type a req. (a, req) Schema.requirement -> a -> req
        =
     fun req v ->
      match req with
      | Computed -> Some v
      | Computed_optional -> Some (Some v)
      | Default _ -> v
      | Optional -> Some v
      | Required -> v

    let rec decode :
        type t.
        E.decoding -> t Schema.t -> Ezjsonm.value -> (t, E.decoding) Result.t =
     fun context { id; descriptor; _ } json ->
      decode_descriptor { context with schema = id } descriptor json

    and decode_descriptor :
        type t.
        E.decoding ->
        t Schema.descriptor ->
        Ezjsonm.value ->
        (t, E.decoding) Result.t =
     fun context descriptor ->
      let type_error expected json =
        Error
          {
            context with
            reason = Fmt.str "Expected a %s but got %a" expected Json.pp json;
          }
      in

      match descriptor with
      | Any (module Coding) ->
        fun json ->
          Error
            {
              context with
              reason =
                Fmt.str
                  "decode does not support descriptor %a while decoding %a"
                  pp_debug_descriptor descriptor Json.pp json;
            }
      | Bool -> (
        function `Bool v -> Ok v | json -> type_error "boolean" json)
      | Coding { f = _ } ->
        fun json ->
          Error
            {
              context with
              reason =
                Fmt.str
                  "decode does not support descriptor %a while decoding %a"
                  pp_debug_descriptor descriptor Json.pp json;
            }
      | Date -> (
        function
        | `String s ->
          let date_or_error = Date.of_string s in
          Result.map_error
            ~f:(fun reason -> { context with reason })
            date_or_error
        | json -> type_error "date" json)
      | Fixpoint lazy_descriptor ->
        fun json -> decode_descriptor context (Lazy.force lazy_descriptor) json
      | Float -> (
        function `Float f -> Ok f | json -> type_error "float" json)
      | Ignore -> Fn.const (Ok ())
      | Int -> (
        function
        | `Float f -> Ok (Float.to_int f) | json -> type_error "int" json)
      | Int64 -> (
        function
        | `Float f -> Ok (Float.to_int64 f) | json -> type_error "int64" json)
      | Json -> fun json -> Ok json
      | List sub_schema -> (
        function
        | `A jsons ->
          List.map
            ~f:(fun json ->
              decode
                { context with path = context.path @ [ "[]" ] }
                sub_schema json)
            jsons
          |> Result.all
        | json -> type_error "array" json)
      | Map { descriptor; decode = cons; _ } ->
        fun json ->
          let* underlying = decode_descriptor context descriptor json in
          cons underlying
          |> Result.map_error ~f:(fun reason -> { context with reason })
      | Mapping value_schema ->
        fun json ->
          let* fields =
            match json with
            | `O fields -> Ok fields
            | json -> type_error "mapping" json
          in
          List.map
            ~f:(fun (key, value) ->
              let+ decoded_value =
                decode
                  { context with path = context.path @ [ key ] }
                  value_schema value
              in
              (key, decoded_value))
            fields
          |> Result.all
      | Null -> (
        function
        | `O [ ("Unit", `O []) ]
        | `O []
        (* XXX: Swift sometimes appear to serialize `Unit` as `{}` instead of [{
           "Unit": {} }]. Not sure why *) ->
          Ok ()
        | json -> type_error "null" json)
      | Nullable schema -> (
        function
        | `O [ ("value", `Null) ] -> Ok None
        | `O [ ("value", json) ] ->
          let+ decoded = decode context schema json in
          Some decoded
        | json -> type_error "object" json)
      | Object { decode = cons; fields; _ } ->
        fun json ->
          let* json_fields =
            match json with
            | `O fields -> Ok fields
            | json -> type_error "object" json
          in
          let* fields_hlist = decode_fields context fields json_fields in
          Ok (cons fields_hlist)
      | String -> (
        function `String v -> Ok v | json -> type_error "string" json)
      | String_const _s -> (
        function `O [] -> Ok () | json -> type_error "empty object" json)
      | Time -> (
        function
        | `Float secs -> (
          match Ptime.of_float_s secs with
          | Some t -> Ok t
          | None ->
            Error { context with reason = Fmt.str "Invalid timestamp %f" secs })
        | json -> type_error "time" json)
      | Time_local -> (
        with_sorted_keys @@ function
        | `O [ ("date", `Float t); ("gmt_offset", `Float tz) ] ->
          let* time =
            match Ptime.of_float_s t with
            | Some t -> Ok t
            | None ->
              Error { context with reason = Fmt.str "Invalid timestamp %f" t }
          in
          Ok (time, Int.of_float_unchecked tz)
        | json -> type_error "time_local" json)
      | Tuple { decode = decode_hlist; items; _ } ->
        decode_tuple context decode_hlist items
      | Union { cases; _ } -> (
        with_sorted_keys @@ function
        (* For simplicity and consistency with the Swift generated types, we
           ignore the optional union key *)
        | `O [ (datatype, `O [ ("_0", json) ]) ] | `O [ (datatype, json) ] ->
          let case_names =
            List.map cases ~f:(fun (Case { name; _ }) -> to_pascal_case name)
          in
          let case =
            List.find cases ~f:(fun (Case { name; _ }) ->
                String.(to_pascal_case name = datatype))
          in
          let* (Case { decoder; schema; _ }) =
            Result.of_option case
              ~error:
                {
                  context with
                  path = context.path @ [ union_type_discriminator ];
                  reason =
                    Fmt.str
                      "Failed to decode Union: data type %S not supported, \
                       expected one of %a"
                      datatype
                      Fmt.(list string)
                      case_names;
                }
          in
          let* decoded_value =
            decode
              { context with path = context.path @ [ union_type_value ] }
              schema json
          in
          Ok (decoder decoded_value)
        | json -> type_error "object with 2 fields #type and value" json)

    and decode_tuple :
        type t tuple.
        E.decoding ->
        (tuple Hlist.t -> t) ->
        tuple Hlist.t Schema.tuple ->
        Ezjsonm.value ->
        (t, E.decoding) Result.t =
     fun context decode_hlist items ->
      with_sorted_keys @@ fun json ->
      match (items, json) with
      | ( Item
            {
              schema = schema_1;
              rest = Item { schema = schema_2; rest = ItemEnd };
            },
          `O [ ("_0", json_1); ("_1", json_2) ] ) ->
        let* decoded_1 =
          decode { context with path = context.path @ [ "_0" ] } schema_1 json_1
        in
        let* decoded_2 =
          decode { context with path = context.path @ [ "_1" ] } schema_2 json_2
        in
        let hlist = Hlist.[ decoded_1; decoded_2 ] in
        Ok (decode_hlist hlist)
      | ( Item
            {
              schema = schema_1;
              rest =
                Item
                  {
                    schema = schema_2;
                    rest = Item { schema = schema_3; rest = ItemEnd };
                  };
            },
          `O [ ("_0", json_1); ("_1", json_2); ("_2", json_3) ] ) ->
        let* decoded_1 =
          decode { context with path = context.path @ [ "_0" ] } schema_1 json_1
        in
        let* decoded_2 =
          decode { context with path = context.path @ [ "_1" ] } schema_2 json_2
        in
        let* decoded_3 =
          decode { context with path = context.path @ [ "_2" ] } schema_3 json_3
        in
        let hlist = Hlist.[ decoded_1; decoded_2; decoded_3 ] in
        Ok (decode_hlist hlist)
      | _, _ ->
        Error
          {
            context with
            reason =
              Fmt.str
                "Only tuples of exactly 2 elements are currently supported, \
                 but found JSON value: %a"
                Json.pp json;
          }

    and decode_fields :
        type t.
        E.decoding ->
        t Hlist.t Schema.fields ->
        (string * Ezjsonm.value) list ->
        (t Hlist.t, E.decoding) Result.t =
     fun context fields json_fields ->
      match fields with
      | FieldEnd -> Ok []
      | Field { field; rest = rest_fields } -> (
        match Schema.field_schema field.schema with
        | { descriptor = Ignore; _ } ->
          let () =
            Logs.debug (fun m ->
                m "decode_fields: Ignoring field %s while decoding" field.name)
          in
          let* decoded_rest = decode_fields context rest_fields json_fields in
          Ok Hlist.(requirement_return field.requirement () :: decoded_rest)
        | _ ->
          let* decoded_head = decode_field context field json_fields in
          let* decoded_rest = decode_fields context rest_fields json_fields in
          Ok Hlist.(decoded_head :: decoded_rest))

    and decode_field :
        type t req.
        E.decoding ->
        (t, req) Schema.field ->
        (string * Ezjsonm.value) list ->
        (req, E.decoding) Result.t =
     fun context { name; requirement; schema; _ } json_fields ->
      let name = to_object_field_name name in
      let schema = Schema.field_schema schema in
      let context = { context with path = context.path @ [ name ] } in
      let m_value = List.Assoc.find ~equal:String.equal json_fields name in
      let* value =
        match m_value with
        | None ->
          Error
            { context with reason = Fmt.str "Object field %s is absent" name }
        | Some value -> Ok value
      in
      let type_error expected json =
        Error
          {
            context with
            reason = Fmt.str "Expected a %s but got %a" expected Json.pp json;
          }
      in
      (* Silly extracted variable because OCaml GADT type inference is bad. *)
      let res : (req, E.decoding) Result.t =
        match requirement with
        | Required ->
          let* decoded_value = decode context schema value in
          Ok decoded_value
        | Optional -> (
          let () =
            Logs.debug (fun m ->
                m "decode_field: Optional for optional JSON %a@."
                  Fmt.(option ~none:(fun fmt () -> Fmt.pf fmt "None") Json.pp)
                  m_value)
          in
          match value with
          | `O [ ("value", `Null) ] -> Ok None
          | `O [ ("value", json) ] ->
            let* decoded_value = decode context schema json in
            Ok (Some decoded_value)
          | json -> type_error "object with single field 'value'" json)
        | Computed -> (
          match value with
          | `O [ ("value", `Null) ] -> Ok None
          | `O [ ("value", json) ] ->
            let* decoded_value = decode context schema json in
            Ok (Some decoded_value)
          | json -> type_error "object with single field 'value'" json)
        | Computed_optional -> (
          match value with
          | `O [ ("value", `Null) ] -> Ok None
          | `O [ ("value", `O [ ("value", `Null) ]) ] -> Ok (Some None)
          | `O [ ("value", `O [ ("value", value) ]) ] ->
            let* decoded_value = decode context schema value in
            Ok (Some (Some decoded_value))
          | json -> type_error "object with single field 'value'" json)
        | Default _default_value ->
          let () =
            Logs.debug (fun m ->
                m "decode_field: Default for optional JSON %a@."
                  Fmt.(option ~none:(fun fmt () -> Fmt.pf fmt "None") Json.pp)
                  m_value)
          in
          decode context schema value
      in
      res

    let decode : type t. t Schema.t -> Ezjsonm.value -> (t, E.decoding) Result.t
        =
     fun schema json ->
      decode { schema = None; path = []; reason = "" } schema json
  end
end

module Public_api = struct
  type t = string * swift Parser.Marks.with_parsing_status

  let of_packed_schemas packed_schemas =
    List.fold
      ~init:(Map.empty (module String))
      ~f:(fun marks rpc_schema ->
        Parser.packed_schema_to_swift_marks marks rpc_schema
        |> fun (_top_level_type_name, _top_level_swift, marks) -> marks)
      packed_schemas
    |> Map.to_alist

  let pp = Generator.pp

  let pp_file fmt marks =
    Fmt.pf fmt "%s@.@[<v 0>%a@]" Generator.preamble
      (Fmt.list ~sep:Fmt.cut Generator.pp)
      (marks
      @ [
          ( "All",
            (* Add a type which is a big union of all the known types. This is
               used by the tests because we can't dynamically dispatch on a
               type, so we use this big enum to emulate it. *)
            Parser.big_union_of
              ([ "Int"; "Int64"; "Float"; "String" ] @ List.map ~f:fst marks) );
        ])

  let to_json = Json.Encode.encode
  let from_json = Json.Decode.decode
  let to_type_name = to_pascal_case
end

include Public_api
