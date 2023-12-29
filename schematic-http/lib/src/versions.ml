module V0_19_0 = struct
  include Client

  module Id : sig
    type t = Id.t

    val pp : t Fmt.t
  end =
    Id

  module Error = Error
end

module V0_19_1 = V0_19_0
