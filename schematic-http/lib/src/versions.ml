module V0_20_0 = struct
  include Client

  module Id : sig
    type t = Id.t

    val pp : t Fmt.t
  end =
    Id

  module Error = Error
end

module V0_20_1 = V0_20_0
