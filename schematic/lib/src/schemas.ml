open Base

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
    with
    | _ -> false)
  | _ -> false

let any_schema = Schema.make Any

let bool_schema = Schema.make Bool

let float_schema = Schema.make Float

let ignore_schema = Schema.make Ignore

let int64_schema = Schema.make Int64

let int_schema = Schema.make Int

let list_schema schema = Schema.make (List schema)

let mapping_schema schema = Schema.make (Mapping schema)

let option_schema s = Schema.make (Nullable s)

let string_const_schema s = Schema.make (String_const s)

let string_schema = Schema.make String

let unit_schema = Schema.make Null

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
                  encoder =
                    (function
                    | First x -> Some x
                    | Second _ -> None);
                  decoder = Either.first;
                };
              Case
                {
                  name = "second";
                  schema = second;
                  encoder =
                    (function
                    | Second x -> Some x
                    | First _ -> None);
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
                  encoder =
                    (function
                    | Ok x -> Some x
                    | Error _ -> None);
                  decoder = Result.return;
                };
              Case
                {
                  name = "error";
                  schema = error_schema;
                  encoder =
                    (function
                    | Result.Error x -> Some x
                    | Result.Ok _ -> None);
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
end

module Ptime = struct
  include Ptime

  let schema = Schema.make Time

  type local = Ptime.t * Ptime.tz_offset_s

  let local_schema = Schema.make Time_local

  let date_schema = Schema.make Date

  let span_schema =
    Schema.make ~id:"duration"
      (Map
         ( (fun s -> Result.return @@ Span.of_int_s s),
           (fun span -> Option.value_exn ~here:[%here] (Span.to_int_s span)),
           Int ))
end

module Uri = struct
  include Uri

  let schema =
    Schema.make
      (Map ((fun s -> Result.return @@ Uri.of_string s), Uri.to_string, String))
end
