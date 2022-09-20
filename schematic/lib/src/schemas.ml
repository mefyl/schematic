open Base
module Schema = Schema

type ignore = unit

let pp_ignore _ () = ()
let equal_ignore () () = true

type any = Ezjsonm.value

let rec equal_any l r =
  match (l, r) with
  | `Null, `Null -> true
  | `Bool l, `Bool r -> Bool.equal l r
  | `Float l, `Float r -> Float.equal l r
  | `String l, `String r -> String.equal l r
  | `A l, `A r -> List.equal equal_any l r
  | `O l, `O r -> (
    try
      let l = Map.of_alist_exn (module String) l
      and r = Map.of_alist_exn (module String) r in
      Map.equal equal_any l r
    with _ -> false)
  | _ -> false

let any_schema = Schema.make Any
let any_schema_versioned (_ : Schema.version) = any_schema
let bool_schema = Schema.make Bool
let bool_schema_versioned (_ : Schema.version) = bool_schema
let float_schema = Schema.make Float
let float_schema_versioned (_ : Schema.version) = float_schema
let ignore_schema = Schema.make Ignore
let ignore_schema_versioned (_ : Schema.version) = ignore_schema
let int64_schema = Schema.make Int64
let int64_schema_versioned (_ : Schema.version) = int64_schema
let int_schema = Schema.make Int
let int_schema_versioned (_ : Schema.version) = int_schema
let list_schema schema = Schema.make (List schema)
let list_schema_versioned _ = list_schema
let mapping_schema schema = Schema.make (Mapping schema)
let option_schema s = Schema.make (Nullable s)
let option_schema_versioned _ = option_schema
let string_const_schema s = Schema.make (String_const s)
let string_schema = Schema.make String
let string_schema_versioned (_ : Schema.version) = string_schema
let unit_schema = Schema.make Null
let unit_schema_versioned (_ : Schema.version) = unit_schema

module Either = struct
  include Base.Either

  let schema first second =
    let descriptor first second =
      Schema.Union
        {
          cases =
            [
              Case
                {
                  name = "first";
                  schema = first;
                  encoder = (function First x -> Some x | Second _ -> None);
                  decoder = Either.first;
                };
              Case
                {
                  name = "second";
                  schema = second;
                  encoder = (function Second x -> Some x | First _ -> None);
                  decoder = Either.second;
                };
            ];
          key = None;
        }
    in
    let parametric =
      Schema.Parametric
        {
          name = "first";
          parameter = first;
          next =
            (fun first ->
              Parametric
                {
                  name = "second";
                  parameter = second;
                  next =
                    (fun second ->
                      Plain
                        {
                          name = "either";
                          descriptor = descriptor first second;
                        });
                });
        }
    in
    Schema.make ~parametric (descriptor first second)

  let schema_versioned (_ : Schema.version) = schema
end

module Result = struct
  include Base.Result

  let schema t error_schema =
    let descriptor t error_schema =
      Schema.Union
        {
          cases =
            [
              Case
                {
                  name = "t";
                  schema = t;
                  encoder = (function Ok x -> Some x | Error _ -> None);
                  decoder = Result.return;
                };
              Case
                {
                  name = "error";
                  schema = error_schema;
                  encoder =
                    (function Result.Error x -> Some x | Result.Ok _ -> None);
                  decoder = Result.fail;
                };
            ];
          key = None;
        }
    in
    let parametric =
      Schema.Parametric
        {
          name = "t";
          parameter = t;
          next =
            (fun t ->
              Parametric
                {
                  name = "error";
                  parameter = error_schema;
                  next =
                    (fun error ->
                      Plain { name = "result"; descriptor = descriptor t error });
                });
        }
    in
    Schema.make ~parametric (descriptor t error_schema)

  let schema_versioned _ = schema
end

module Ptime = struct
  include Ptime

  let schema = Schema.make Time
  let schema_versioned (_ : Schema.version) = schema

  type local = Ptime.t * Ptime.tz_offset_s

  let equal_local (t1, o1) (t2, o2) = Ptime.equal t1 t2 && o1 = o2
  let pp_local = Fmt.(pair Ptime.pp Int.pp)
  let local_schema = Schema.make Time_local
  let local_schema_versioned (_ : Schema.version) = local_schema
  let equal_date (y1, m1, d1) (y2, m2, d2) = y1 = y2 && m1 = m2 && d1 = d2
  let date_schema = Schema.make Date
  let date_schema_versioned (_ : Schema.version) = date_schema

  let span_schema =
    Schema.make ~id:"duration"
      (Map
         {
           decode = (fun s -> Result.return @@ Span.of_int_s s);
           encode =
             (fun span -> Option.value_exn ~here:[%here] (Span.to_int_s span));
           descriptor = Int;
         })

  let span_schema_versioned (_ : Schema.version) = span_schema
end

module Uri = struct
  include Uri

  let schema =
    Schema.make
      (Map
         {
           decode = (fun s -> Result.return @@ Uri.of_string s);
           encode = Uri.to_string;
           descriptor = String;
         })

  let schema_versioned (_ : Schema.version) = schema
end

let one_or_many_versioned schema _ =
  let either = Either.schema schema (list_schema schema)
  and decode = function
    | Either.First v -> Result.return [ v ]
    | Second l -> Result.return l
  and encode = function [ v ] -> Either.First v | l -> Either.Second l in
  Schema.map ~decode ~encode either

let one_or_many schema = one_or_many_versioned schema None
