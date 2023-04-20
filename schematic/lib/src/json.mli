include module type of Json_intf
module Impl : Coding.IMPL with type t = t
include S

val schema : t Schema.t
val schema_versioned : Schema.version -> t Schema.t

module String : Coding.STR with type t = string
module Make (Impl : Impl) : S
