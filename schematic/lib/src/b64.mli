
include module type of Base64_intf
include S
module Make (Decoder : Decoder_Impl) (Backend: Backend_Impl) : S
module Decoder : Decoder_Impl