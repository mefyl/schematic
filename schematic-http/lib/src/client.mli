include module type of Client_intf
module Make (Handler : HANDLER) : S with type 'a t = 'a Handler.response

module Cohttp
    (Client : Cohttp_lwt.S.Client)
    (Coding : Schematic.Coding.S with type t = string) :
  HANDLER with type 'a response = 'a

module Rate_limit (Rate : RATE_LIMIT) (Handler : HANDLER) :
  HANDLER with type 'a response = 'a Handler.response

module Optional (Handler : HANDLER) :
  HANDLER with type 'a response = 'a option Handler.response
