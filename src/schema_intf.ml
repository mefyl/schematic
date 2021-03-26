type (_, _) field_requirement =
  | Required : ('a, 'a) field_requirement
  | Default : 'a -> ('a, 'a) field_requirement
  | Optional : ('a, 'a option) field_requirement

type _ case =
  | Case : {
      name : string;
      schema : 'a schema;
      encoder : 't -> 'a option;
      decoder : 'a -> 't;
    }
      -> 't case

and 'a field_schema =
  | Outline of 'a schema
  | Inline of 'a schema

and _ field =
  | FieldEnd : unit field
  | Field : {
      description : string option;
      examples : 'a list;
      field : string;
      maximum : int option;
      minimum : int option;
      omit : bool;
      requirement : ('a, 'req) field_requirement;
      rest : 'rest field;
      schema : 'a field_schema;
      title : string option;
    }
      -> ('req * 'rest) field

and _ tuple =
  | ItemEnd : unit tuple
  | Item : {
      schema : 'a schema;
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
  | List : 'a schema -> 'a list descriptor
  | Map : ('a -> 'b) * ('b -> 'a) * 'a descriptor -> 'b descriptor
  | Mapping : 'a schema -> (string * 'a) list descriptor
  | Null : unit descriptor
  | Nullable : 'a schema -> 'a option descriptor
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

and 't schema = {
  id : string option;
  descriptor : 't descriptor;
}
