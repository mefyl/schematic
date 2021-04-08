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
          ( Span.of_int_s,
            (fun span -> Option.value_exn ~here:[%here] (Span.to_int_s span)),
            Int );
    }
end

module Uri = struct
  include Uri

  let schema =
    {
      Schema.id = None;
      descriptor = Map (Uri.of_string, Uri.to_string, String);
    }
end

include Schemas_intf

let bool_diff l r = Option.some_if (not @@ Bool.equal l r) r

let int_diff l r = Option.some_if (not @@ Int.equal l r) r

let list_diff f l r =
  let res, diff =
    Sequence.zip (Sequence.of_list l) (Sequence.of_list r)
    |> Sequence.map ~f:(fun (l, r) -> f l r)
    |> Sequence.fold ~init:([], false) ~f:(fun (res, diff) r ->
           (r :: res, diff || Option.is_some r))
  in
  if diff then
    Some res
  else
    None

let string_diff l r = Option.some_if (not @@ String.equal l r) r

let unit_diff () () = None

let bool_patch v _ = v

let int_patch v _ = v

let list_patch f p l =
  Sequence.zip (Sequence.of_list p) (Sequence.of_list l)
  |> Sequence.map ~f:(function
       | Some p, v -> f p v
       | None, v -> v)
  |> Sequence.to_list

let string_patch v _ = v

let unit_patch () () = ()

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
