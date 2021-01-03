type (_, _) field_requirement =
  | Required : ('a, 'a) field_requirement
  | Default : 'a -> ('a, 'a) field_requirement
  | Optional : ('a, 'a option) field_requirement

type kinds =
  [ `Object
  | `Tuple
  | `Value
  ]

type _ case =
  | Case : {
      schema : 'a schema;
      encoder : 't -> 'a option;
      decoder : 'a -> 't;
    }
      -> 't case

and 'a field_schema =
  | Outline of 'a schema
  | Inline of 'a schema

and ('t, 'kind) descriptor =
  | Any : (Ezjsonm.value, [< `Value ]) descriptor
  | Bool : (bool, [< `Value ]) descriptor
  | Date : (Ptime.date, [< `Value ]) descriptor
  | Int : (int, [< `Value ]) descriptor
  | List : 'a schema -> ('a list, [< `Value ]) descriptor
  | Map :
      ('a -> 'b) * ('b -> 'a) * ('a, [ `Value ]) descriptor
      -> ('b, [< `Value ]) descriptor
  | Mapping : 'a schema -> ((string * 'a) list, [< `Value ]) descriptor
  | Null : (unit, [< kinds ]) descriptor
  | Nullable : 'a schema -> ('a option, [< `Value ]) descriptor
  | String : (string, [< `Value ]) descriptor
  | String_const : string -> (unit, [< `Value ]) descriptor
  | Time : (Ptime.t, [< `Value ]) descriptor
  | Time_local : (Ptime.t * Ptime.tz_offset_s, [< `Value ]) descriptor
  | Object : {
      description : string option;
      examples : 'a list;
      field : string;
      maximum : int option;
      minimum : int option;
      requirement : ('a, 'req) field_requirement;
      rest : ('b, [ `Object ]) descriptor;
      schema : 'a field_schema;
      title : string option;
    }
      -> ('req * 'b, [< `Object | `Value ]) descriptor
  | Tuple : {
      schema : 'a schema;
      rest : ('b, [ `Tuple ]) descriptor;
    }
      -> ('a * 'b, [< `Tuple | `Value ]) descriptor
  | Union : 't case list -> ('t, [< `Value ]) descriptor

and 't schema = {
  id : string option;
  descriptor : ('t, [ `Value ]) descriptor;
}
