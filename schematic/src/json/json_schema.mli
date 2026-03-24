include module type of Json_schema_intf

val make :
  ?applicator:applicator ->
  ?core:core ->
  ?metadata:metadata ->
  ?validation:validation ->
  unit ->
  t

val of_schema : _ Schema.t -> t
