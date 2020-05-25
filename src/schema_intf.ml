type (_, _) field_requirement =
  | Required : ('a, 'a) field_requirement
  | Default : 'a -> ('a, 'a) field_requirement
  | Optional : ('a, 'a option) field_requirement

type _ case =
  | Case : {
      schema : 'a schema;
      encoder : 't -> 'a option;
      decoder : 'a -> 't;
    }
      -> 't case

and _ descriptor =
  | Any : Ezjsonm.value descriptor
  | Bool : bool descriptor
  | End : unit descriptor
  | Int : int descriptor
  | List : 'a schema -> 'a list descriptor
  | Map : ('a -> 'b) * ('b -> 'a) * 'a descriptor -> 'b descriptor
  | Mapping : 'a schema -> (string * 'a) list descriptor
  | Null : unit descriptor
  | String : string descriptor
  | String_const : string -> unit descriptor
  | Time : Ptime.t descriptor
  | Time_local : (Ptime.t * Ptime.tz_offset_s) descriptor
  | Object : {
      field : string;
      title : string option;
      description : string option;
      requirement : ('a, 'req) field_requirement;
      inline : bool;
      schema : 'a schema;
      rest : 'b descriptor;
    }
      -> ('req * 'b) descriptor
  | Tuple : {
      schema : 'a schema;
      rest : 'b descriptor;
    }
      -> ('a * 'b) descriptor
  | Union : 't case list -> 't descriptor

and 't schema = {
  id : string option;
  descriptor : 't descriptor;
}
