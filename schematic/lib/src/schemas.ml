open Base

type ignore = unit

type any = Ezjsonm.value

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
    let descriptor =
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
    Schema.make descriptor
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
