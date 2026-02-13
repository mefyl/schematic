module E = Error
open Base

let src = Logs.Src.create ~doc:"schematic Kotlin" "schematic"

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

(** Reserved keywords in Kotlin. See
    https://kotlinlang.org/docs/keyword-reference.html *)
let kotlin_reserved_words =
  Set.of_list
    (module String)
    [
      "as";
      "break";
      "class";
      "continue";
      "do";
      "else";
      "false";
      "for";
      "fun";
      "if";
      "in";
      "interface";
      "is";
      "null";
      "object";
      "package";
      "return";
      "super";
      "this";
      "throw";
      "true";
      "try";
      "typealias";
      "typeof";
      "val";
      "var";
      "when";
      "while";
    ]

(** The JSON object field storing the data type for [Union]s. *)
let union_type_discriminator = "#type"

(** The JSON object field storing the value for [Union]s. *)
let union_type_value = "value"

(** The Kotlin type for [option]. *)
let option_kotlin_type = "Option"

(** Transform the given string into a valid Kotlin object field name. *)
let to_object_field_name s =
  let s = to_camel_case s in
  if Set.mem kotlin_reserved_words s then s ^ "_" else s

type kotlin_type =
  | Simple of string  (** E.g. [Foo] *)
  | Generic of string * kotlin_type list
      (** E.g.
          [Generic ("Bar", [Generic ("List", [Simple "Boolean"]); Simple
           "String"])] is [Bar<List<Boolean>, String>] *)
[@@deriving show]

type 'a documented = {
  value : 'a; [@main]
  documentation : string option;
}
[@@deriving show, make]

type kotlin_field = {
  name : string;
  type_ : kotlin_type;
}
[@@deriving show]

type kotlin_data_class = {
  generics : kotlin_type list;
  fields : kotlin_field documented list;
}
[@@deriving show]

type kotlin =
  | Atom of kotlin_type documented  (** Type alias or basic type *)
  | Data_object
      (** Data classes without any field. Kotlin forbids data classes without
          any field. *)
  | Data_class of kotlin_data_class documented
      (** A record, with fields defined as [(name, type, required)] *)
  | Sealed_interface of {
      generics : kotlin_type list;
      nested_data_classes : (string * kotlin_case * string option) list;
      documentation : string option;
    }
      (** A union type-ish in Kotlin, in the form of a sealed interface and
          implementing classes. *)
[@@deriving show]

(** E.g. for [type foo = A of {bar: string} | B of Int], the [A] branch will
    become [Inline] (because we don't have any type name to refer to), and [B]
    will be [Wrap], because we wrap an existing type. *)
and kotlin_case =
  (* TODO Add generics *)
  | Inline of kotlin
  | Wrap of kotlin_type

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

(** Module responsible for parsing Schematic schemas into a Kotlin AST. *)
module Parser = struct
  module Marks = struct
    include Map.M (String)

    type 'a with_parsing_status =
      | InProgress
      | Done of 'a

    (** The map key is a Kotlin type. It must NOT contain generic (parametric)
        information. Indeed [Foo<A>] and [Foo<B>] represent the same type
        declaration, so we must not declare it twice, and it is useless to parse
        it twice. This is why generic information is stored in the [kotlin] type
        expression only. In this case, [name = "Foo"]. *)
    type nonrec t = kotlin with_parsing_status t
  end

  let wrap_in_option : kotlin_type -> kotlin_type =
   fun kotlin_type -> Generic (option_kotlin_type, [ kotlin_type ])

  (** Try to find an adequate Kotlin type name. *)
  let type_name_and_kotlin_to_kotlin_type_exn :
      string option -> kotlin -> kotlin_type =
   fun type_name kotlin ->
    match (type_name, kotlin) with
    | Some type_name, _ -> Simple type_name
    | None, Atom kotlin_type -> kotlin_type.value
    | _ ->
      Fmt.failwith "I don't know how to extract a Kotlin type from: %a\n"
        pp_kotlin kotlin

  (** [schema_to_kotlin_marks marks schema] returns:
      - the Kotlin type name of the schema (if any, and without generics)
      - the Kotlin type expression of this [schema]
      - the input [marks] enhanced with additional Kotlin type expressions found
        while parsing [schema]. *)
  let rec schema_to_kotlin_marks : type a.
      Marks.t -> a Schema.t -> string option * kotlin * Marks.t =
   fun marks ({ id; descriptor; parametric; documentation } as schema) ->
    let () =
      Logs.debug (fun m ->
          m "schema_to_kotlin_marks: Parsing schema %a" Schema.pp schema)
    in
    try
      let kotlin_type_name = Option.map ~f:to_pascal_case id in
      let _generics, marks =
        (* TODO generics *)
        match parametric with
        | None -> ([], marks)
        | Some parametric -> parametric_to_kotlin_marks marks parametric
      in
      match Option.map ~f:(Map.find marks) kotlin_type_name with
      | None ->
        (* No Kotlin type name: parse *)
        let () =
          Logs.debug (fun m ->
              m "schema_to_kotlin_marks: Start parsing unnamed schema")
        in
        let kotlin, marks =
          descriptor_to_kotlin_marks marks documentation descriptor
        in
        (kotlin_type_name, kotlin, marks)
      | Some None ->
        (* Kotlin type name but not present in the marks: add [InProgress] in
           case we recursively try to parse the same schema while parsing it,
           and parse. *)
        let kotlin_type_name = Option.value_exn kotlin_type_name in
        let () =
          Logs.debug (fun m ->
              m "schema_to_kotlin_marks: Start parsing %s" kotlin_type_name)
        in
        let marks = Map.add_exn ~key:kotlin_type_name ~data:InProgress marks in
        let kotlin, marks =
          descriptor_to_kotlin_marks marks documentation descriptor
        in
        let marks = Map.set ~key:kotlin_type_name ~data:(Done kotlin) marks in
        (Some kotlin_type_name, kotlin, marks)
      | Some (Some InProgress) ->
        (* Kotlin type name and parsing in progress: return the type name but
           don't parse *)
        let kotlin_type_name = Option.value_exn kotlin_type_name in
        let () =
          Logs.debug (fun m ->
              m "schema_to_kotlin_marks: InProgress, returning %s"
                kotlin_type_name)
        in
        ( Some kotlin_type_name,
          Atom
            {
              value = Simple kotlin_type_name (* TODO generics *);
              documentation = None;
            },
          marks )
      | Some (Some (Done kotlin)) ->
        let () =
          Logs.debug (fun m ->
              m "schema_to_kotlin_marks: Done, returning %a"
                Fmt.(pair (option string) pp_kotlin)
                (kotlin_type_name, kotlin))
        in
        (* Kotlin type name and parsing is done: return the parsing result *)
        (kotlin_type_name, kotlin, marks)
    with NotYetImplemented s ->
      failwith
      @@ Fmt.str "Not yet supported: %s for schema %a" s Schema.pp schema

  and descriptor_to_kotlin_marks : type a.
      Marks.t -> string option -> a Schema.descriptor -> kotlin * Marks.t =
   fun marks schema_documentation descriptor ->
    let atom atm =
      Atom (make_documented ?documentation:schema_documentation atm)
    in
    match descriptor with
    | Any (module Coding) -> raise (NotYetImplemented "Any")
    | Bool -> (atom (Simple "Boolean"), marks)
    | Coding { f = _ } -> raise (NotYetImplemented "Coding")
    | Date -> (atom (Simple "LocalDate"), marks)
    | Fixpoint lazy_descriptor ->
      descriptor_to_kotlin_marks marks None (Lazy.force lazy_descriptor)
    | Float -> (atom (Simple "Float"), marks)
    | Ignore ->
      (Data_object, marks)
      (* Treat [Ignore] fields as an empty object that will be removed in a
         later pass *)
    | Int -> (atom (Simple "Int"), marks)
    | Int64 -> (atom (Simple "Long"), marks)
    | Json -> (atom (Simple "JsonElement"), marks)
    | List item_schema ->
      (* TODO We will probably need [kotlin] to support generics *)
      list_to_kotlin_marks schema_documentation marks item_schema
    | Sequence { item; _ } ->
      (* TODO We will probably need [kotlin] to support generics *)
      list_to_kotlin_marks schema_documentation marks item
    | Map { descriptor; _ } -> descriptor_to_kotlin_marks marks None descriptor
    | Mapping { item; _ } ->
      (* TODO We will probably need [kotlin] to support generics *)
      let type_name, kotlin, marks = schema_to_kotlin_marks marks item in
      let kotlin_type =
        type_name_and_kotlin_to_kotlin_type_exn type_name kotlin
      in
      let kotlin =
        atom
          (Generic ("kotlin.collections.Map", [ Simple "String"; kotlin_type ]))
      in
      (kotlin, marks)
    | Null -> (atom (Simple "Unit"), marks)
    | Nullable schema ->
      let type_name, kotlin, marks = schema_to_kotlin_marks marks schema in
      let () =
        Logs.debug (fun m ->
            m "descriptor_to_kotlin_marks: Parsing Nullable for type_name %a"
              Fmt.(option string)
              type_name)
      in
      let res =
        match (type_name, kotlin) with
        | None, Atom kotlin_type ->
          (atom (wrap_in_option kotlin_type.value), marks)
        | None, _kotlin ->
          raise
            (NotYetImplemented
               (Fmt.str
                  "We are trying to mark as nullable a type without name and \
                   that is not an Atom: %a"
                  pp_kotlin kotlin))
        | Some type_name, _kotlin ->
          (atom (wrap_in_option (Simple type_name)), marks)
      in
      let () =
        Logs.debug (fun m ->
            m "descriptor_to_kotlin_marks: Nullable result: %a"
              Fmt.(pair pp_kotlin nop)
              res)
      in
      res
      (* TODO Generics *)
    | Object { fields = FieldEnd; _ } ->
      (Data_object, marks)
      (* Objects with 0 field have to be handled differently in Kotlin as [data
         class Foo()] is illegal. *)
    | Object { fields; _ } -> (
      let kotlin_fields, marks = object_fields_to_kotlin_marks marks fields in
      match kotlin_fields with
      | [] ->
        (Data_object, marks)
        (* In case all fields have been filtered out (e.g. because they are
           [Ignore]), this data class must be converted to a data object (cf
           previous case). *)
      | kotlin_fields ->
        ( Data_class
            {
              value = { fields = kotlin_fields; generics = [] };
              documentation = schema_documentation;
            },
          marks ))
    | String -> (atom (Simple "String"), marks)
    | String_const _ -> (Data_object, marks)
    | Time -> (atom (Simple "Instant"), marks)
    | Time_local -> (atom (Simple "OffsetDateTime"), marks)
    | Tuple { items; _ } ->
      let kotlin_type, marks = tuple_to_kotlin_marks marks items in
      (atom kotlin_type, marks)
    | Union { key = _; cases; polymorphic = _ } ->
      (* For simplicity, we ignore the optional union key *)
      let marks, nested_data_classes =
        List.fold_right ~init:(marks, [])
          ~f:(fun
              (Case { name = case_name; schema; _ })
              (marks, rest_nested_types)
            ->
            let case_name = to_pascal_case case_name in
            let case_documentation = schema.documentation in
            let type_name, kotlin, marks =
              schema_to_kotlin_marks marks schema
            in
            let nested_type =
              match (type_name, kotlin) with
              | Some type_name, _kotlin ->
                (case_name, Wrap (Simple type_name), case_documentation)
              | None, Atom kotlin_type ->
                (case_name, Wrap kotlin_type.value, case_documentation)
              | None, _ -> (case_name, Inline kotlin, case_documentation)
              (* TODO Need to add generics *)
            in
            let nested_types = nested_type :: rest_nested_types in
            (marks, nested_types))
          cases
      in
      let kotlin =
        Sealed_interface
          {
            nested_data_classes;
            generics = [ (* TODO *) ];
            documentation = schema_documentation;
          }
      in
      (kotlin, marks)

  and list_to_kotlin_marks : type a.
      string option -> Marks.t -> a Schema.t -> kotlin * Marks.t =
   fun documentation marks item_schema ->
    let type_name, kotlin, marks = schema_to_kotlin_marks marks item_schema in
    let kotlin_type =
      type_name_and_kotlin_to_kotlin_type_exn type_name kotlin
    in
    let kotlin =
      Atom
        (make_documented ?documentation
        @@ Generic ("kotlin.collections.List", [ kotlin_type ]))
    in
    (kotlin, marks)

  and tuple_to_kotlin_marks : type a.
      Marks.t -> a Schema.tuple -> kotlin_type * Marks.t =
    let tuple_element_to_kotlin_type marks schema =
      let type_name, kotlin, marks = schema_to_kotlin_marks marks schema in
      let kotlin_type =
        type_name_and_kotlin_to_kotlin_type_exn type_name kotlin
      in
      (kotlin_type, marks)
    in
    fun marks items ->
      match items with
      | Item
          {
            schema = schema_1;
            rest = Item { schema = schema_2; rest = ItemEnd };
          } ->
        let kotlin_type_1, marks =
          tuple_element_to_kotlin_type marks schema_1
        in
        let kotlin_type_2, marks =
          tuple_element_to_kotlin_type marks schema_2
        in
        (Generic ("Pair", [ kotlin_type_1; kotlin_type_2 ]), marks)
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
        let kotlin_type_1, marks =
          tuple_element_to_kotlin_type marks schema_1
        in
        let kotlin_type_2, marks =
          tuple_element_to_kotlin_type marks schema_2
        in
        let kotlin_type_3, marks =
          tuple_element_to_kotlin_type marks schema_3
        in
        ( Generic ("Triple", [ kotlin_type_1; kotlin_type_2; kotlin_type_3 ]),
          marks )
      | _ ->
        raise (NotYetImplemented "Only tuples of 2 or 3 elements are supported")

  and parametric_to_kotlin_marks : type a.
      Marks.t -> a Schema.parametric -> kotlin_type list * Marks.t =
   fun marks parametric ->
    match parametric with
    | Plain { name; descriptor } ->
      let _kotlin, marks = descriptor_to_kotlin_marks marks None descriptor in
      (* TODO do we need this [kotlin] type? *)
      ([ Simple name ], marks)
    | Parametric { name; parameter; next } ->
      (* TODO do we need this parameter type and kotlin? *)
      let _parameter_type_name, _kotlin, marks =
        schema_to_kotlin_marks marks parameter
      in
      let other_types, marks =
        parametric_to_kotlin_marks marks (next parameter)
      in
      (Simple name :: other_types, marks)

  and object_fields_to_kotlin_marks : type a.
      Marks.t ->
      a Hlist.t Schema.fields ->
      kotlin_field documented list * Marks.t =
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
            m "object_fields_to_kotlin_marks: Ignoring field %s" name)
      in
      object_fields_to_kotlin_marks marks rest
    | Field { field; rest } ->
      let kotlin_field, marks = object_field_to_kotlin_marks marks field in
      let kotlin_fields, marks = object_fields_to_kotlin_marks marks rest in
      (kotlin_field :: kotlin_fields, marks)

  and object_field_to_kotlin_marks : type t req.
      Marks.t -> (t, req) Schema.field -> kotlin_field documented * Marks.t =
   fun marks
       {
         name;
         requirement;
         description;
         (* For Kotlin, we do not care about inlining, we decided to preserve
            the same structure as the original OCaml type. *)
         schema = Outline schema | Inline schema | Inline_try schema;
         _;
       } ->
    let wrap_in_options kotlin_type =
      match requirement with
      | Required | Default _ -> kotlin_type
      | Computed | Optional -> wrap_in_option kotlin_type
      | Computed_optional -> wrap_in_option (wrap_in_option kotlin_type)
    in
    let kotlin_type_name, kotlin, marks = schema_to_kotlin_marks marks schema in
    let () =
      Logs.debug (fun m ->
          m "object_field_to_kotlin_marks: kotlin_type_name: %a"
            Fmt.(option string)
            kotlin_type_name)
    in
    let kotlin_type, marks =
      match (kotlin_type_name, kotlin) with
      | Some kotlin_type_name, _ -> (Simple kotlin_type_name, marks)
      | None, Atom { value = Simple type_name; _ } -> (Simple type_name, marks)
      | None, Atom { value = Generic (type_name, generics); _ } ->
        (Generic (type_name, generics), marks)
      | None, Sealed_interface { nested_data_classes; _ } ->
        (* Corner case: there is one (1) occurrence in the whole RPC type
           hierarchy where an object field refers to an anonymous schema:
           [graph-error]. In best effort, we try to craft a unique-ish enough
           name and create an associated type. *)
        let id =
          nested_data_classes
          |> List.map ~f:(fun (case_name, _kotlin_case, _doc) -> case_name)
          |> String.concat ~sep:"-or-"
          |> to_pascal_case
        in
        let kotlin_type_name, _kotlin, marks =
          schema_to_kotlin_marks marks { schema with id = Some id }
        in
        (Simple (Option.value_exn kotlin_type_name), marks)
      | _ ->
        Fmt.failwith
          "Hopefully unreachable: the object field %S has no type name: %a" name
          pp_kotlin kotlin
    in
    let kotlin_type = wrap_in_options kotlin_type in
    let () =
      Logs.debug (fun m ->
          m "object_field_to_kotlin_marks: Result: %a" pp_kotlin_type
            kotlin_type)
    in
    ( {
        value = { name = to_object_field_name name; type_ = kotlin_type };
        documentation = description;
      },
      marks )

  let packed_schema_to_kotlin_marks marks packed_schema =
    Schema.unpack
      { unpack = (fun schema -> schema_to_kotlin_marks marks schema) }
      packed_schema
end

(** Module responsible for generating actual Kotlin code from the parser result.
*)
module Generator = struct
  let original_pp_kotlin_type = pp_kotlin_type

  let pp_docstring fmt doc =
    let escape s = String.substr_replace_all s ~pattern:"*/" ~with_:"*\\/" in
    Fmt.pf fmt "/** %s */@;" (escape doc)

  let pp_docstring_opt = Fmt.option pp_docstring

  let pp_documented pp_elt fmt (elt : 'a documented) =
    Fmt.pf fmt "%a%a" pp_docstring_opt elt.documentation pp_elt elt.value

  (** Convenience for Android developers: default all option types to [None] *)
  let add_default_none_for_option : kotlin_type -> string = function
    | Generic (name, _) when String.equal name option_kotlin_type -> " = None"
    | _ -> ""

  let rec pp_field : kotlin_field Fmt.t =
   fun f { name = field_name; type_ = field_type } ->
    Fmt.pf f "val %s: %a%s" field_name pp_kotlin_type field_type
      (add_default_none_for_option field_type)

  and pp_kotlin_type : kotlin_type Fmt.t =
   fun f kotlin_type ->
    let () =
      Logs.debug (fun m ->
          m "pp_kotlin_type: Pretty-printing kotlin type %a"
            original_pp_kotlin_type kotlin_type)
    in
    match kotlin_type with
    | Simple s -> Fmt.pf f "%s" s
    | Generic (s, generics) -> Fmt.pf f "%s%a" s pp_generics generics

  and pp : (string * kotlin Parser.Marks.with_parsing_status) Fmt.t =
   fun f (name, kotlin) ->
    match kotlin with
    | InProgress ->
      Fmt.failwith
        "Parsing is complete but we still have In Progress parsing for type %s"
        name
    | Done (Atom alias) ->
      let pp_alias f = Fmt.pf f "typealias %s = %a" name pp_kotlin_type in
      pp_documented pp_alias f alias
    | Done Data_object ->
      raise (NotYetImplemented "Top-level data object, really?")
    | Done (Data_class dataclass) ->
      let pp_data_class fmt { fields; generics } =
        Fmt.pf fmt "@Serializable@;data class %s%a(%a)" name pp_generics
          generics
          (Fmt.list ~sep:Fmt.comma (pp_documented pp_field))
          fields
      in
      pp_documented pp_data_class f dataclass
    | Done (Sealed_interface { nested_data_classes; generics; documentation })
      ->
      Fmt.pf f
        "%a@Serializable@;\
         @OptIn(ExperimentalSerializationApi::class)@;\
         @JsonClassDiscriminator(\"%s\")@;\
         sealed interface %s%a {@;\
         <0 4>@[%a@]@;\
         }"
        pp_docstring_opt documentation union_type_discriminator name pp_generics
        generics
        (Fmt.list ~sep:Fmt.cut
           (pp_sealed_nested_data_class ~parent_interface:name))
        nested_data_classes

  (** The subclass of a sealed class. We can't reuse [pp_mark] as is because we
      must add extra information, like the [@SerialName] or the parent interface
      name that it implements. *)
  and pp_sealed_nested_data_class :
      parent_interface:string -> (string * kotlin_case * string option) Fmt.t =
   fun ~parent_interface f (case_name, kotlin_case, case_documentation) ->
    (* For now we prefix with the parent interface name otherwise it can
       incorrectly recurse. E.g. imagine you have top-level type [Foo] and a
       union type [Bar] with a nested class [Foo] with a field of type [Foo]
       (top-level). Kotlin will incorrectly think the field is of the inner type
       rather than the top-level type.

       This makes the data types very verbose though (e.g.
       [YesNoMaybe.YesNoMaybeYes] instead of [YesNoMaybe.Yes]), it would be
       better to use Full Qualified Names (i.e. prefix with the package) +
       fine-tune the Kotlin formatter to remove unnecessary FQNs. *)
    let type_name = parent_interface ^ case_name in
    let generics = [] in
    (* TODO generics *)
    match kotlin_case with
    | Wrap kotlin_type ->
      Fmt.pf f
        "%a@Serializable@;\
         @SerialName(\"%s\")@;\
         data class %s%a(val value: %a) : %s@;"
        pp_docstring_opt case_documentation case_name type_name pp_generics
        generics pp_kotlin_type kotlin_type parent_interface
    | Inline kotlin ->
      (* When inlining, we actually create an "inner" data class/object, so that
         all cases of a [Union] have the same structure, and then create a
         wrapper. *)
      let inner_kotlin_type_name = type_name ^ "Inner" in
      let inner_kotlin_type = Simple inner_kotlin_type_name in
      let pp_inner_kotlin f kotlin =
        match kotlin with
        | Data_class { value = { fields; generics }; _ } ->
          Fmt.pf f "@Serializable@;data class %s%a(%a)@;" inner_kotlin_type_name
            pp_generics generics
            (Fmt.list ~sep:Fmt.comma (pp_documented pp_field))
            fields
        | Data_object ->
          Fmt.pf f "@Serializable@;data object %s%a@;" inner_kotlin_type_name
            pp_generics generics
        | _other ->
          raise
            (NotYetImplemented
               (Fmt.str
                  "Sealed nested data class: unsupported Inline Kotlin type: %a"
                  pp_kotlin kotlin))
      in
      let pp_wrapper_kotlin f () =
        Fmt.pf f
          "@Serializable@;\
           @SerialName(\"%s\")@;\
           data class %s%a(val value: %a) : %s@;"
          case_name type_name pp_generics generics pp_kotlin_type
          inner_kotlin_type parent_interface
      in
      Fmt.pf f "%a@;%a" pp_inner_kotlin kotlin pp_wrapper_kotlin ()

  (** Special case if the list of generics is empty as [data class Foo<>] is
      illegal in Kotlin. *)
  and pp_generics : kotlin_type list Fmt.t =
   fun f kotlin_types ->
    match kotlin_types with
    | [] -> Fmt.pf f ""
    | kotlin_types ->
      Fmt.pf f "<%a>" Fmt.(list ~sep:comma pp_kotlin_type) kotlin_types
end

(** Module responsible for encoding into/decoding from Kotlin-compatible JSON.
*)
module Json = struct
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
      | Mapping { deconstruct; item; _ } ->
        let encode = encode item in
        fun v ->
          let v = Sequence.to_list (deconstruct v) in
          let pairs = List.map ~f:(fun (k, v) -> (k, encode v)) v in
          `O pairs
      | Null -> Fn.const (`O [])
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
      | Time -> fun v -> Json.Impl.of_time v
      | Time_local ->
        fun (t, tz_offset_s) -> Json.Impl.of_time_local (t, tz_offset_s)
      | Tuple { encode; items; _ } ->
        fun v ->
          let items_hlist = encode v in
          encode_tuple items items_hlist
      | Union { cases; _ } -> (
        (* For simplicity and consistency with the Kotlin generated types, we
           ignore the optional union key *)
        let cases =
          let f (Schema.Case { encoder; name; schema; _ }) =
            let encode = encode schema in
            fun v ->
              match encoder v with
              | Some v ->
                Some
                  (`O
                     [
                       (union_type_discriminator, `String (to_pascal_case name));
                       (union_type_value, encode v);
                     ])
              | None -> None
          in
          List.map ~f cases
        in
        fun v ->
          match List.find_map ~f:(fun case -> case v) cases with
          | Some res -> res
          | None -> Fmt.failwith "unmatched variant case in schema")

    and encode_tuple : type t.
        t Hlist.t Schema.tuple -> t Hlist.t -> Ezjsonm.value =
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
        `O [ ("first", encoded_1); ("second", encoded_2) ]
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
        `O [ ("first", encoded_1); ("second", encoded_2); ("third", encoded_3) ]
      | _, _ ->
        raise (NotYetImplemented "Only tuples of 2 or 3 elements are supported")

    and encode_fields : type t.
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

    and encode_field : type t req.
        (t, req) Schema.field -> req -> string * Ezjsonm.value =
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
    let ( >>= ) = Result.( >>= )
    let ( >>| ) = Result.( >>| )
    let ( let* ) = ( >>= )
    let ( let+ ) = ( >>| )

    let requirement_return : type a req. (a, req) Schema.requirement -> a -> req
        =
     fun req v ->
      match req with
      | Computed -> Some v
      | Computed_optional -> Some (Some v)
      | Default _ -> v
      | Optional -> Some v
      | Required -> v

    let rec decode : type t.
        E.decoding -> t Schema.t -> Ezjsonm.value -> (t, E.decoding) Result.t =
     fun context { id; descriptor; _ } json ->
      decode_descriptor { context with schema = id } descriptor json

    and decode_descriptor : type t.
        E.decoding ->
        t Schema.descriptor ->
        Ezjsonm.value ->
        (t, E.decoding) Result.t =
     fun context descriptor ->
      let fail reason = Error { context with reason } in
      let fail' reason = { context with reason } in
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
        | `Float f -> Ok (Float.to_int f)
        | json -> type_error "int" json)
      | Int64 -> (
        function
        | `Float f -> Ok (Float.to_int64 f)
        | json -> type_error "int64" json)
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
      | Mapping { construct; item; _ } ->
        fun json ->
          let* fields =
            match json with
            | `O fields -> Ok fields
            | json -> type_error "mapping" json
          in
          let* fields =
            List.map
              ~f:(fun (key, value) ->
                let+ decoded_value =
                  decode
                    { context with path = context.path @ [ key ] }
                    item value
                in
                (key, decoded_value))
              fields
            |> Result.all
          in
          fields |> Sequence.of_list |> construct |> Result.map_error ~f:fail'
      | Null -> ( function `O [] -> Ok () | json -> type_error "null" json)
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
        fun json ->
          Json.Impl.to_time json |> function
          | Ok _ as ok -> ok
          | Error None -> type_error "time" json
          | Error (Some reason) -> Error { context with reason })
      | Time_local -> (
        function
        | `String s -> (
          let error (`RFC3339 (_, e)) =
            {
              context with
              reason = Stdlib.Format.asprintf "%a" Ptime.pp_rfc3339_error e;
            }
          in
          let* t, tz, _ =
            Ptime.of_rfc3339 ~strict:true s |> Result.map_error ~f:error
          in
          match tz with
          | Some tz -> Ok (t, tz)
          | None ->
            Error
              {
                context with
                reason =
                  Stdlib.Format.asprintf "Time_local without timezone: %s" s;
              })
        | json -> type_error "time_local" json)
      | Tuple { decode = decode_hlist; items; _ } ->
        decode_tuple context decode_hlist items
      | Union { cases; _ } -> (
        function
        (* For simplicity and consistency with the Kotlin generated types, we
           ignore the optional union key *)
        | `O [ (type_key, `String datatype); (value_key, json) ]
          when String.(
                 type_key = union_type_discriminator
                 && value_key = union_type_value) ->
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

    and decode_tuple : type t tuple.
        E.decoding ->
        (tuple Hlist.t -> t) ->
        tuple Hlist.t Schema.tuple ->
        Ezjsonm.value ->
        (t, E.decoding) Result.t =
     fun context decode_hlist items json ->
      match (items, json) with
      | ( Item
            {
              schema = schema_1;
              rest = Item { schema = schema_2; rest = ItemEnd };
            },
          `O [ ("first", json_1); ("second", json_2) ] ) ->
        let* decoded_1 =
          decode
            { context with path = context.path @ [ "first" ] }
            schema_1 json_1
        in
        let* decoded_2 =
          decode
            { context with path = context.path @ [ "second" ] }
            schema_2 json_2
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
          `O [ ("first", json_1); ("second", json_2); ("third", json_3) ] ) ->
        let* decoded_1 =
          decode
            { context with path = context.path @ [ "first" ] }
            schema_1 json_1
        in
        let* decoded_2 =
          decode
            { context with path = context.path @ [ "second" ] }
            schema_2 json_2
        in
        let* decoded_3 =
          decode
            { context with path = context.path @ [ "second" ] }
            schema_3 json_3
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

    and decode_fields : type t.
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

    and decode_field : type t req.
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
  type t = string * kotlin Parser.Marks.with_parsing_status

  let of_packed_schemas packed_schemas =
    List.fold
      ~init:(Map.empty (module String))
      ~f:(fun marks rpc_schema ->
        Parser.packed_schema_to_kotlin_marks marks rpc_schema
        |> fun (_top_level_type_name, _top_level_kotlin, marks) -> marks)
      packed_schemas
    |> Map.to_alist

  let pp = Generator.pp

  let pp_file ?package fmt marks =
    Fmt.pf fmt
      {|@file:UseSerializers(KOffsetDateTimeSerializer::class, CustomOptionSerializer::class)@.

%a@.

import arrow.core.None
import arrow.core.Option
import java.time.OffsetDateTime
import java.time.format.DateTimeFormatter
import kotlinx.datetime.Instant
import kotlinx.datetime.LocalDate
import kotlinx.serialization.*
import kotlinx.serialization.builtins.nullable
import kotlinx.serialization.descriptors.*
import kotlinx.serialization.encoding.*
import kotlinx.serialization.json.JsonClassDiscriminator
import kotlinx.serialization.json.JsonElement

/**
 * Kotlin serialization does not provide serializers for OffsetDateTime. So we manually provide one
 * and enable it in the file with the @file annotation at the top of this file.
 *
 * See https://github.com/Kotlin/kotlinx-datetime/issues/90
 * See https://stackoverflow.com/a/65077862
 */
object KOffsetDateTimeSerializer : KSerializer<OffsetDateTime> {
  override val descriptor: SerialDescriptor =
      PrimitiveSerialDescriptor("OffsetDateTime", PrimitiveKind.STRING)

  override fun serialize(encoder: Encoder, value: OffsetDateTime) {
    val format = DateTimeFormatter.ISO_OFFSET_DATE_TIME

    val string = format.format(value)
    encoder.encodeString(string)
  }

  override fun deserialize(decoder: Decoder): OffsetDateTime {
    val string = decoder.decodeString()
    return OffsetDateTime.parse(string)
  }
}

/**
 * The Arrow-kt serializer for Option directly encodes as nullable, which is no more desirable for us than
 * Kotlin's default serialization of nullable values.
 * Indeed, encoding directly as null prevents serializing nested options, like Option<Option<String>>.
 * You cannot differentiate None from Some(None).
 * So we provide instead a custom serializer that wraps in a JSON object with a single, nullable field called "value".
 * Now, None becomes {"value": null} while Some(None) becomes {"value": {"value": null}}.
 * The extra verbosity is a small price to pay for this much-needed flexibility.
 */
class CustomOptionSerializer<T>(dataSerializer: KSerializer<T>) : KSerializer<Option<T>> {

  private val nullableDataSerializer: KSerializer<T?> = dataSerializer.nullable

  override val descriptor: SerialDescriptor =
      buildClassSerialDescriptor("arrow.core.Option") {
        element("value", nullableDataSerializer.descriptor)
      }

  override fun serialize(encoder: Encoder, value: Option<T>) =
      encoder.encodeStructure(descriptor) {
        encodeSerializableElement(descriptor, 0, nullableDataSerializer, value.getOrNull())
      }

  override fun deserialize(decoder: Decoder) =
      decoder.decodeStructure(descriptor) {
        var value : T? = null
        while (true) {
          when (val index = decodeElementIndex(descriptor)) {
            0 -> value = decodeSerializableElement(descriptor, 0, nullableDataSerializer)
            CompositeDecoder.DECODE_DONE -> break
            else -> error("Unexpected index: $index")
          }
        }
        Option.fromNullable(value)
      }
}

private val <T> KSerializer<T>.nullable get() =
  @Suppress("UNCHECKED_CAST")
  if (descriptor.isNullable) (this as KSerializer<T?>)
  else (this as KSerializer<T & Any>).nullable

@[<hv 0>@[<v 2>%a@]@]@.
|}
      (Fmt.option (fun fmt -> Fmt.pf fmt "package %s;"))
      package
      (Fmt.list ~sep:Fmt.cut Generator.pp)
      marks

  let to_json = Json.Encode.encode
  let from_json = Json.Decode.decode
  let to_type_name = to_pascal_case
end

include Public_api
