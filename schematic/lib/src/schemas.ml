open Base

type ignore = unit

type any = Ezjsonm.value

let any_schema = { Schema.id = None; descriptor = Any }

let bool_schema = { Schema.id = None; descriptor = Bool }

let float_schema = { Schema.id = None; descriptor = Float }

let ignore_schema = { Schema.id = None; descriptor = Ignore }

let int64_schema = { Schema.id = None; descriptor = Int64 }

let int_schema = { Schema.id = None; descriptor = Int }

let list_schema schema = { Schema.id = None; descriptor = List schema }

let mapping_schema schema = { Schema.id = None; descriptor = Mapping schema }

let option_schema s = { Schema.id = None; descriptor = Nullable s }

let string_const_schema s = { Schema.id = None; descriptor = String_const s }

let string_schema = { Schema.id = None; descriptor = String }

let unit_schema = { Schema.id = None; descriptor = Null }

module Either = struct
  include Base.Either

  let schema first second =
    {
      Schema.id = None;
      descriptor =
        Union
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
          };
    }
end

module Ptime = struct
  include Ptime

  let schema = { Schema.id = None; descriptor = Time }

  type local = Ptime.t * Ptime.tz_offset_s

  let local_schema = { Schema.id = None; descriptor = Time_local }

  let date_schema = { Schema.id = None; descriptor = Date }

  let span_schema =
    {
      Schema.id = Some "duration";
      descriptor =
        Map
          ( (fun s -> Result.return @@ Span.of_int_s s),
            (fun span -> Option.value_exn ~here:[%here] (Span.to_int_s span)),
            Int );
    }
end

module Uri = struct
  include Uri

  let schema =
    {
      Schema.id = None;
      descriptor =
        Map ((fun s -> Result.return @@ Uri.of_string s), Uri.to_string, String);
    }
end
