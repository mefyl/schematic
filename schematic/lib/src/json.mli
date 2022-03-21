include module type of Json_intf
include S
module String : Coding.S with type t = string
module Make (Impl : Impl) : S
