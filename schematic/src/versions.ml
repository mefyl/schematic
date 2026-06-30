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
    module Unversioned = Schema

    (* FIXME: Drop when we break compatibility. *)
    type 't t = 't Schema_intf.t = private {
      id : string option;
      documentation : string option;
      descriptor : 't descriptor;
      parametric : 't parametric option;
    }

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
          since : (Semver.t * library) option;
        }
          -> 't case

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
  end
end
