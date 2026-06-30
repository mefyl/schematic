open Schema

type 'a t = 'a descriptor

(* FIXME: this should replace the [fields] type when we break compatibility. *)
module Fields = struct
  type 't t =
    | [] : unit t
    | ( :: ) : ('t, 'req) field * 'tail t -> ('req -> 'tail) t
end

let case ~decoder ~encoder ?(inline = false) ?since name schema =
  Case { decoder; encoder; inline; name; raw_name = name; schema; since }

let field ?description ?(examples = []) ?maximum ?minimum ?(omit = false)
    ~requirement ?since ?title name schema =
  {
    description;
    examples;
    maximum;
    minimum;
    name;
    omit;
    requirement;
    schema;
    since;
    title;
  }

let list schema = List schema
let map ~decode ~encode descriptor = Map { decode; encode; descriptor }
let nullable schema = Nullable schema
let union ?key cases = Union { cases; key; polymorphic = false }

let rec _fields : 'f. 'f Fields.t -> 'f Hlist.t fields =
 fun (type f) (fields : f Fields.t) : f Hlist.t fields ->
  match fields with
  | Fields.[] -> FieldEnd
  | field :: tail -> Field { field; rest = _fields tail }

let object' ~decode ~encode fields =
  Object { decode; encode; fields = _fields fields }
