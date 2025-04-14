module E = Error
open Base

[@@@ocaml.warning "-30"]

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
  | Sequence _ -> "Sequence"
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
      (* Keywords used in declarations *)
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
      (* Keywords used in statements *)
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
      (* Keywords used in expressions and types *)
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
      (* Keywords used in patterns *)
      "_";
      (* Keywords that begin with a number sign (#) *)
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
      (* Keywords reserved in particular contexts *)
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
      (* Not keywords, but stuff that needs to be in scope and shouldn't be
         shadowed *)
      "Swift";
      "Foundation";
    ]

(** The JSON object field storing the value for [Union]s. *)
let union_type_value = "value"

(** Transform the given string into a valid Swift identifier. *)
let to_swift_identifier s =
  if Set.mem swift_reserved_words s then s ^ "_" else s

let to_object_field_name s = to_swift_identifier @@ to_camel_case s
let to_type_name s = to_swift_identifier @@ to_pascal_case s
let to_enum_case_name = to_object_field_name

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
  cases : swift_case list;
}
[@@deriving show]

and swift_case = {
  case_name : string;
  arguments : swift_argument list;
}

(** An argument for a Swift function/constructor. *)
and swift_argument = {
  argument_name : string option;
      (** If [argument_name] is [Some name], make it a named argument
          [name : type]. Otherwise, make it positional. *)
  argument_type : swift_type;
}

type swift =
  | Atom of swift_type  (** Type alias or basic type *)
  | Struct of swift_struct  (** A record *)
  | Enum of swift_union
[@@deriving show]

let empty_struct = Struct { generics = []; fields = [] }

module Date = struct
  (** A sligtly more structured version of [PTime.date] for serialization
      purposes *)
  type t = {
    year : int;
    month : int;
    day : int;
  }
  [@@deriving schema]

  let of_ptime (year, month, day) = { year; month; day }
  let to_ptime { year; month; day } = (year, month, day)
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
   fun swift_type -> Generic ("Routine.Maybe", [ swift_type ])

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
      let swift_type_name = Option.map ~f:to_type_name id in
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
    | Bool -> (Atom (Simple "Swift.Bool"), marks)
    | Coding { f = _ } -> raise (NotYetImplemented "Coding")
    | Date -> (Atom (Simple "Routine.TimelessDate"), marks)
    | Fixpoint lazy_descriptor ->
      descriptor_to_swift_marks marks (Lazy.force lazy_descriptor)
    | Float -> (Atom (Simple "Swift.Float"), marks)
    | Ignore ->
      (empty_struct, marks)
      (* Treat [Ignore] fields as an empty object that will be removed in a
         later pass *)
    | Int -> (Atom (Simple "Swift.Int"), marks)
    | Int64 -> (Atom (Simple "Swift.Int64"), marks)
    | Json -> (Atom (Simple "Foundation.Data"), marks)
    | List item ->
      (* TODO We will probably need [swift] to support generics *)
      list_to_swift_marks marks item
    | Sequence { item; _ } ->
      (* TODO We will probably need [swift] to support generics *)
      list_to_swift_marks marks item
    | Map { descriptor; _ } -> descriptor_to_swift_marks marks descriptor
    | Mapping { item; _ } ->
      (* TODO We will probably need [swift] to support generics *)
      let type_name, swift, marks = schema_to_swift_marks marks item in
      let swift_type = type_name_and_swift_to_swift_type_exn type_name swift in
      let swift =
        Atom
          (Generic ("Swift.Dictionary", [ Simple "Swift.String"; swift_type ]))
      in
      (swift, marks)
    | Null -> (Atom (Simple "Routine.Unit"), marks)
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
    | String -> (Atom (Simple "Swift.String"), marks)
    | String_const _ -> (empty_struct, marks)
    | Time -> (Atom (Simple "Foundation.Date"), marks)
    | Time_local -> (Atom (Simple "Routine.ZonedDate"), marks)
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
            let case_name = to_enum_case_name case_name in
            let type_name, swift, marks = schema_to_swift_marks marks schema in
            let nested_type =
              match (type_name, swift) with
              | Some type_name, _swift ->
                {
                  case_name;
                  arguments =
                    [
                      { argument_name = None; argument_type = Simple type_name };
                    ];
                }
              | None, Atom swift_type ->
                {
                  case_name;
                  arguments =
                    [ { argument_name = None; argument_type = swift_type } ];
                }
              | None, Struct { fields; _ } ->
                {
                  case_name;
                  arguments =
                    List.map fields ~f:(fun (name, argument_type) ->
                        { argument_name = Some name; argument_type });
                }
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

  and list_to_swift_marks : type a. Marks.t -> a Schema.t -> swift * Marks.t =
   fun marks item_schema ->
    let type_name, swift, marks = schema_to_swift_marks marks item_schema in
    let swift_type = type_name_and_swift_to_swift_type_exn type_name swift in
    let swift = Atom (Generic ("Swift.Array", [ swift_type ])) in
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
        (Generic ("Routine.Tuple2", [ swift_type_1; swift_type_2 ]), marks)
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
        ( Generic
            ("Routine.Tuple3", [ swift_type_1; swift_type_2; swift_type_3 ]),
          marks )
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
          |> List.map ~f:(fun { case_name; _ } -> case_name)
          |> String.concat ~sep:"-or-"
          |> to_object_field_name
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
    let name_to_case (case_name : string) =
      {
        case_name = to_type_name case_name;
        arguments =
          [ { argument_name = None; argument_type = Simple case_name } ];
      }
    in
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
    Fmt.pf f "public let %s: %a" field_name pp_swift_type field_type

  (** Pretty-print a single [case] inside an [enum] *)
  and pp_case : swift_case Fmt.t =
   fun f { case_name; arguments } ->
    let pp_case_arg fmt { argument_name; argument_type } =
      let pp_name fmt = function
        | None -> Fmt.pf fmt ""
        | Some name -> Fmt.pf fmt "%s: " name
      in
      Fmt.pf fmt "%a%a" pp_name argument_name pp_swift_type argument_type
    in
    Fmt.pf f "case %s%a" case_name
      (pp_optional_list ~open_delim:"(" ~close_delim:")" pp_case_arg)
      arguments

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

  and pp_class_init : (string * swift_type) list Fmt.t =
   fun f fields ->
    Fmt.pf f "public init(%a) { %a }"
      Fmt.(list ~sep:comma (pair ~sep:(any ":@ ") string pp_swift_type))
      fields
      Fmt.(
        list ~sep:(any "@;") (fun fmt (name, _) ->
            pf fmt "self.%s = %s" name name))
      fields

  and pp : (string * swift Parser.Marks.with_parsing_status) Fmt.t =
   fun f (name, swift) ->
    match swift with
    | InProgress ->
      Fmt.failwith
        "Parsing is complete but we still have In Progress parsing for type %s"
        name
    | Done (Atom alias) ->
      Fmt.pf f "public typealias %s = %a" name pp_swift_type alias
    | Done (Struct { fields; generics }) ->
      Fmt.pf f
        "@[<v 2>public final class %s%a : Codable, Sendable {@;%a@;%a@]@;}@;"
        name pp_generics generics
        (Fmt.list ~sep:Fmt.cut pp_field)
        fields pp_class_init fields
    | Done (Enum { cases; generics }) ->
      Fmt.pf f
        "@[<v 2>public indirect enum %s%a : Codable, Sendable {@;%a@]@;}@;" name
        pp_generics generics
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

// Dirty workaround to make the code compile on the CI (and my machine)
// as they are using Swift 5.8, and `Date` was only made `Sendable` in 6.0
extension Foundation.Date: @unchecked Sendable {}

// Routine-specific types.
// Wrapped in a caseless enum because that's apparently the best way to emulate namespaces in Swift
public enum Routine {
  // Isomorphic to `nil`, but using a more predictible JSON encoding
  public enum Unit : Codable, Sendable { case Unit }

  // Sad but required: Although Swift has native tuples, these are not `Codable, Sendable`,
  // so we can't use them in the protocol, and we have to define our own ugly things
  public struct Tuple2<T0 : Codable& Sendable, T1 : Codable& Sendable> : Codable, Sendable {
    public let _0 : T0
    public let _1 : T1
    public init(_0 : T0, _1 : T1) {
      self._0 = _0
      self._1 = _1
    }
  }
  public struct Tuple3<T0 : Codable& Sendable, T1 : Codable& Sendable, T2 : Codable& Sendable> : Codable, Sendable {
    public let _0 : T0
    public let _1 : T1
    public let _2 : T2
    public init(_0 : T0, _1 : T1, _2 : T2) {
      self._0 = _0
      self._1 = _1
      self._2 = _2
    }
  }

  // Isomorphic to `Optional`, but with a more regular JSON encoding:
  // The JSON encoding of `Optional<>` represents `Some(nil)` and `nil`
  // the same way, which makes the conversion lossy (which we can't afford).
  // This wrapper will represent `nil` as `{ "value": null}` and `Some(nil)` as
  // `{ "value": { "value": nil } }` which is ugly, but unambiguous.
  public struct Maybe<T : Codable& Sendable> : Codable, Sendable {
    public let value : Optional<T>

    // Custom encoding because the default one will just omit null values, while
    // we'd rather have an explicit `null`
    public func encode(to encoder: Encoder) throws {
      var container = encoder.container(keyedBy: CodingKeys.self)
      try container.encode(value, forKey: .value)
    }

    public init(value : Optional<T>) {
      self.value = value
    }
  }

  // A date with a timezone offset, corresponding to the `Time_local`
  public struct ZonedDate : Codable, Sendable {
    public let date : Foundation.Date
    public let gmt_offset : Int

    public init(date : Foundation.Date, gmt_offset : Int) {
      self.date = date
      self.gmt_offset = gmt_offset
    }
  }

  // A date in the default (Gregorian) calendar
  public struct TimelessDate : Codable, Sendable {
    public let year : Int
    public let month : Int
    public let day : Int

    public init(year : Int, month : Int, day : Int) {
      self.year = year
      self.month = month
      self.day = day
    }
  }
  |}

  let postamble = {|
} // enum Routine
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
      | Date -> fun v -> v |> Date.of_ptime |> encode Date.schema
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
      | Sequence { deconstruct; item; _ } ->
        let f = encode item in
        fun v ->
          let sub_values =
            v |> deconstruct |> Base.Sequence.map ~f |> Base.Sequence.to_list
          in
          `A sub_values
      | Map { descriptor; encode = decons; _ } ->
        let encode = encode_descriptor descriptor in
        fun v -> encode (decons v)
      | Mapping { construct; deconstruct; item; length } ->
        encode_descriptor
          (Sequence
             {
               construct;
               deconstruct;
               item = Schemas.(Pair.schema string_schema item);
               length;
             })
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
                  Some (`O [ (to_enum_case_name name, encode v) ])
                | _ ->
                  Some
                    (`O [ (to_enum_case_name name, `O [ ("_0", encode v) ]) ]))
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
    let ( >>| ) = Result.( >>| )
    let ( let+ ) = ( >>| )
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
      let fail reason = Error { context with reason } in
      let failf fmt = Fmt.kstr fail fmt in
      let type_error expected json =
        failf "expected a %s but got %a" expected Json.pp json
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
      | Date ->
        fun json ->
          Result.map ~f:Date.to_ptime @@ decode context Date.schema json
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
      | Sequence { construct; item; _ } -> (
        function
        | `A v ->
          let* items =
            v
            |> List.map ~f:(fun json ->
                   decode
                     { context with path = context.path @ [ "[]" ] }
                     item json)
            |> Result.all
          in
          items
          |> Base.Sequence.of_list
          |> construct
          |> Result.map_error ~f:(function reason -> { context with reason })
        | json -> type_error "array" json)
      | Map { descriptor; decode = cons; _ } ->
        fun json ->
          let* underlying = decode_descriptor context descriptor json in
          cons underlying
          |> Result.map_error ~f:(fun reason -> { context with reason })
      | Mapping { construct; deconstruct; item; length } ->
        decode_descriptor context
          (Sequence
             {
               construct;
               deconstruct;
               item = Schemas.(Pair.schema string_schema item);
               length;
             })
      | Null -> (
        function
        | `O [ ("Unit", `O []) ] -> Ok () | json -> type_error "null" json)
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
        | `O [ (datatype, `O [ ("_0", json) ]) ]
        (* Inline records in enums are serialized as inline constructor
           arguments, so decode them the same way *)
        | `O [ (datatype, json) ] ->
          let case_names =
            List.map cases ~f:(fun (Case { name; _ }) -> to_enum_case_name name)
          in
          let case =
            List.find cases ~f:(fun (Case { name; _ }) ->
                String.(to_enum_case_name name = datatype))
          in
          let* (Case { decoder; schema; _ }) =
            Result.of_option case
              ~error:
                {
                  context with
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
        | json -> type_error "object with a type matching the enum case" json)

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
                "Only tuples of 2 or 3 elements are currently supported, but \
                 found JSON value: %a"
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

  let union_of types name =
    ( name,
      Parser.big_union_of
        ([
           "Swift.Int";
           "Swift.Int64";
           "Swift.Float";
           "Swift.String";
           "TimelessDate";
         ]
        @ List.map ~f:fst types) )

  let pp_file fmt marks =
    Fmt.pf fmt "%s@.@[<v 0>%a@]%s" Generator.preamble
      (Fmt.list ~sep:Fmt.cut Generator.pp)
      marks Generator.postamble

  let to_json = Json.Encode.encode
  let from_json = Json.Decode.decode
  let to_type_name = to_type_name
end

include Public_api
