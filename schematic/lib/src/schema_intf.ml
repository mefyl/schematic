type (_, _) requirement =
  | Required : ('a, 'a) requirement
  | Default : 'a -> ('a, 'a) requirement
  | Optional : ('a, 'a option) requirement

type _ case =
  | Case : {
      name : string;
      schema : 'a t;
      encoder : 't -> 'a option;
      decoder : 'a -> 't;
    }
      -> 't case

and 'a field_schema =
  | Outline of 'a t
  | Inline of 'a t

and _ field =
  | FieldEnd : unit field
  | Field : {
      description : string option;
      examples : 'a list;
      field : string;
      maximum : int option;
      minimum : int option;
      omit : bool;
      requirement : ('a, 'req) requirement;
      rest : 'rest field;
      schema : 'a field_schema;
      title : string option;
    }
      -> ('req * 'rest) field

and _ tuple =
  | ItemEnd : unit tuple
  | Item : {
      schema : 'a t;
      rest : 'b tuple;
    }
      -> ('a * 'b) tuple

and _ descriptor =
  | Any : Ezjsonm.value descriptor
  | Bool : bool descriptor
  | Date : Ptime.date descriptor
  | Float : float descriptor
  | Ignore : unit descriptor
  | Int : int descriptor
  | Int64 : int64 descriptor
  | List : 'a t -> 'a list descriptor
  | Map : ('a -> 'b) * ('b -> 'a) * 'a descriptor -> 'b descriptor
  | Mapping : 'a t -> (string * 'a) list descriptor
  | Null : unit descriptor
  | Nullable : 'a t -> 'a option descriptor
  | String : string descriptor
  | String_const : string -> unit descriptor
  | Time : Ptime.t descriptor
  | Time_local : (Ptime.t * Ptime.tz_offset_s) descriptor
  | Object : 't field -> 't descriptor
  | Tuple : 't tuple -> 't descriptor
  | Union : {
      key : string option;
      cases : 't case list;
    }
      -> 't descriptor

and 't t = {
  id : string option;
  descriptor : 't descriptor;
}
