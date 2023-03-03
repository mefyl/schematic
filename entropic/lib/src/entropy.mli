include module type of Entropy_intf

module Bytes : sig
  include S

  val make : Bytes.t -> t
end

module Entropy : sig
  include S

  val make : (module S with type t = 't) -> 't -> t
end
