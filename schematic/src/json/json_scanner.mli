include module type of Json_scanner_intf

module Error : sig
  type t

  val pp : t Fmt.t

  exception Parse_error of t
end

type t

val of_string : string -> t
val scan : t -> token
