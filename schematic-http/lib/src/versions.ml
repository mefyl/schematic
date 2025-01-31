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
module V0_20_2 = V0_20_1
