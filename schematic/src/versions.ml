module V0_22_0 = struct
  module Error = Error
  module Base64 = B64
  module Coding = Coding
  module Json = Json
  module Kotlin = Kotlin
  module Marks = Marks
  module Marshal = Marshal
  module Pp = Pp
  module Schema = Schema
  module Schemas = Schemas
  module Streaming = Streaming
  module Swift = Swift
  module Typescript = Typescript
  module Version = Version
end

module V0_22_1 = V0_22_0
module V0_22_2 = V0_22_1

module V0_22_3 = struct
  include V0_22_2

  module Schema = struct
    include Schema

    (* FIXME: Drop when we break compatibility and this is only available via
       [Unversioned]. *)
    type 't t = 't Schema_intf.t = private {
      id : string option;
      documentation : string option;
      descriptor : 't descriptor;
      parametric : 't parametric option;
    }

    and 't case = 't Schema_intf.case = private
      | Case : {
          decoder : 'a -> 't;
          encoder : 't -> 'a option;
          inline : bool;
              (** Whether the tuple or record argument is inlined in the OCaml
                  memory representation of the constructor. *)
          name : string;
          raw_name : string;  (** Only used for the C ABI. *)
          schema : 'a t;
          since : (Semver.t * Schema.library) option;
        }
          -> 't case

    and 't descriptor = 't Schema_intf.descriptor = private
      | Any : (module Coding_intf.IMPL with type t = 't) -> 't descriptor
      | Bool : bool descriptor
      | Coding : {
          f : 'coding. 'coding Coding_intf.t option -> 'a descriptor;
        }
          -> 'a descriptor
      | Date : Ptime.date descriptor
      | Fixpoint : 'a descriptor Lazy.t -> 'a descriptor
      | Float : float descriptor
      | Ignore : unit descriptor
      | Int : int descriptor
      | Int64 : int64 descriptor
      | Json : Ezjsonm.value descriptor
      | List : 'a t -> 'a list descriptor
      | Map : {
          decode : 'encoded -> ('t, string) Result.t;
          encode : 't -> 'encoded;
          descriptor : 'encoded descriptor;
        }
          -> 't descriptor
      | Mapping : {
          construct : (string * 'item) Base.Sequence.t -> ('t, string) Result.t;
          deconstruct : 't -> (string * 'item) Base.Sequence.t;
          item : 'item t;
          length : 't -> int;
        }
          -> 't descriptor
      | Null : unit descriptor
      | Nullable : 'a t -> 'a option descriptor
      | Object : {
          decode : 'fields Hlist.t -> 't;
          encode : 't -> 'fields Hlist.t;
          fields : 'fields Hlist.t fields;
        }
          -> 't descriptor
      | Sequence : {
          construct : 'item Base.Sequence.t -> ('result, string) Result.t;
          deconstruct : 'result -> 'item Base.Sequence.t;
          item : 'item t;
          length : 'result -> int;
        }
          -> 'result descriptor
      | String : string descriptor
      | String_const : string -> unit descriptor
      | Time : Ptime.t descriptor
      | Time_local : (Ptime.t * Ptime.tz_offset_s) descriptor
      | Tuple : {
          decode : 'tuple Hlist.t -> 't;
          encode : 't -> 'tuple Hlist.t;
          items : 'tuple Hlist.t tuple;
        }
          -> 't descriptor
      | Union : {
          key : string option;
          cases : 't case list;
          polymorphic : bool;
              (** Whether this is a polymorphic variant in OCaml. *)
        }
          -> 't descriptor

    and ('t, 'req) field = ('t, 'req) Schema_intf.field = private {
      description : string option;
      examples : 't list;
      maximum : int option;
      minimum : int option;
      name : string;
      omit : bool;
      requirement : ('t, 'req) requirement;
      schema : 't field_schema;
      since : (Semver.t * Schema.library) option;
      title : string option;
    }

    and 'a field_schema = 'a Schema_intf.field_schema =
      | Outline of 'a t
      | Inline of 'a t
      | Inline_try of 'a t

    and 't fields = 't Schema_intf.fields = private
      | FieldEnd : unit Hlist.t fields
      | Field : {
          field : ('t, 'req) field;
          rest : 'tail Hlist.t fields;
        }
          -> ('req -> 'tail) Hlist.t fields

    and 't parametric = 't Schema_intf.parametric = private
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

    and ('a, 'req) requirement = ('a, 'req) Schema_intf.requirement =
      | Computed : ('a, 'a option) requirement
      | Computed_optional : ('a, 'a option option) requirement
      | Default : 'a -> ('a, 'a) requirement
      | Required : ('a, 'a) requirement
      | Optional : ('a, 'a option) requirement

    and 't tuple = 't Schema_intf.tuple = private
      | ItemEnd : unit Hlist.t tuple
      | Item : {
          schema : 'a t;
          rest : 'b Hlist.t tuple;
        }
          -> ('a -> 'b) Hlist.t tuple

    module Unversioned = Schema

    module Descriptor : sig
      module Fields : sig
        type 't t =
          | [] : unit t
          | ( :: ) : ('t, 'req) field * 'tail t -> ('req -> 'tail) t
      end

      (** [case ~decoder ~encoder name schema]. *)
      val case :
        decoder:('case -> 't) ->
        encoder:('t -> 'case option) ->
        ?inline:bool ->
        ?since:Semver.t * library ->
        string ->
        'case t ->
        't case

      val field :
        ?description:string ->
        ?examples:'t list ->
        ?maximum:int ->
        ?minimum:int ->
        ?omit:bool ->
        requirement:('t, 'requirement) requirement ->
        ?since:Semver.t * library ->
        ?title:string ->
        string ->
        't field_schema ->
        ('t, 'requirement) field

      (** [list element_schema]. *)
      val list : 't t -> 't list descriptor

      (** [map ~decode ~encode mapped_schema]. *)
      val map :
        decode:('mapped -> ('t, string) Result.t) ->
        encode:('t -> 'mapped) ->
        'mapped descriptor ->
        't descriptor

      (** [nullable schema]. *)
      val nullable : 't t -> 't option descriptor

      (** [object' ~decode ~encode fields]. *)
      val object' :
        decode:('fields Hlist.t -> 't) ->
        encode:('t -> 'fields Hlist.t) ->
        'fields Fields.t ->
        't descriptor

      (** [union ?key cases]. *)
      val union : ?key:string -> 't case list -> 't descriptor

      type 'a t = 'a descriptor
    end =
      Descriptor
  end
end
