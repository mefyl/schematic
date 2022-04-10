module Cohttp_ = Cohttp

module V0_13_1 = struct
  include Client
  module Error = Error
end

module V0_13_0 = struct
  include V0_13_1

  module type STATUS = sig
    type error

    val check : Cohttp_.Code.status_code -> error option
  end

  let status_backward = function Some error -> `Error error | None -> `Ok

  module Check_status (Status : STATUS) (Handler : HANDLER) = struct
    module Status = struct
      include Status

      let check code = check code |> status_backward
    end

    include Check_status (Status) (Handler)
  end

  module Status_success = struct
    type error = Cohttp_.Code.status_code

    let check status =
      if Cohttp_.Code.(is_success (code_of_status status)) then None
      else Some status
  end

  let check_status_is expected =
    let expected = Cohttp_.Code.code_of_status expected in
    (module struct
      type error = Cohttp_.Code.status_code

      let check status =
        if Cohttp_.Code.code_of_status status = expected then None
        else Some status
    end : STATUS
      with type error = Cohttp_.Code.status_code)

  module Error = Error
end
