include module type of Json_coding_intf
module Impl : Coding.IMPL with type t = t
include S

val schema : t Schema.t
val schema_versioned : Schema.version -> t Schema.t

module String : Coding.STR with type t = string
module Make (_ : Impl) : S
module Streaming : Streaming.S with type error = Error.Decoding.t
