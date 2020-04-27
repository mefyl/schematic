type (_, _) field_requirement =
  | Required : ('a, 'a) field_requirement
  | Default : 'a -> ('a, 'a) field_requirement
  | Optional : ('a, 'a option) field_requirement

type _ case =
  | Case :
      { schema : 'a schema
      ; encoder : 't -> 'a option
      ; decoder : 'a -> 't
      }
      -> 't case

and _ schema =
  | Null : unit schema
  | End : unit schema
  | Int : int schema
  | String : string schema
  | Bool : bool schema
  | List : 'a schema -> 'a list schema
  | Mapping : 'a schema -> (string * 'a) list schema
  | Map : ('a -> 'b) * ('b -> 'a) * 'a schema -> 'b schema
  | Object :
      { field : string
      ; title : string option
      ; description : string option
      ; requirement : ('a, 'req) field_requirement
      ; inline : bool
      ; schema : 'a schema
      ; rest : 'b schema
      }
      -> ('req * 'b) schema
  | Tuple :
      { schema : 'a schema
      ; rest : 'b schema
      }
      -> ('a * 'b) schema
  | Union : 't case list -> 't schema
