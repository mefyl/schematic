open Base

module Schemas_library = struct
  type Schematic.Schema.library += V

  let v = V
end

type 'a hidden_option = 'a option [@@deriving schema]

(** A documented type for testing TypeScript JSDoc generation *)
type documented_type = int [@@deriving schema]

(** A user record with name and age fields *)
type documented_record = {
  name : string;
  age : int;
}
[@@deriving schema]

(** A type with */ in the documentation to test escaping *)
type documented_with_star_slash = string [@@deriving schema]

(** A user with documented fields *)
type user_with_field_docs = {
  name : string;  (** The user's full name *)
  age : int;  (** Age in years *)
  email : string option;  (** Optional email address *)
}
[@@deriving schema]

(** A result type with documented cases *)
type result_with_case_docs =
  | Ok of int  (** Success with a value *)
  | Error of string  (** Failure with an error message *)
[@@deriving schema]

let all_schemas =
  [
    Schematic.Schema.pack documented_type_schema;
    Schematic.Schema.pack documented_record_schema;
    Schematic.Schema.pack documented_with_star_slash_schema;
    Schematic.Schema.pack user_with_field_docs_schema;
    Schematic.Schema.pack result_with_case_docs_schema;
  ]

let () =
  let args = Sys.get_argv () in
  match args with
  | [| _; "typescript" |] ->
    Fmt.pr "@[<v>%a@]"
      (Fmt.list ~sep:(Fmt.any "@;@;") Schematic.Typescript.pp_toplevel)
      (Schematic.Typescript.of_schemas all_schemas)
  | [| _; "swift" |] ->
    Fmt.pr "@[<v>%a@]"
      (Fmt.list ~sep:(Fmt.any "@;@;") Schematic.Swift.pp)
      (Schematic.Swift.of_packed_schemas all_schemas)
  | [| _; "kotlin" |] ->
    Fmt.pr "@[<v>%a@]"
      (Fmt.list ~sep:(Fmt.any "@;@;") Schematic.Kotlin.pp)
      (Schematic.Kotlin.of_packed_schemas all_schemas)
  | _ -> Fmt.failwith "Usage: %s [typescript|swift|kotlin]" args.(0)
