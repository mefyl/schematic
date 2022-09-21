let json_of_value_callback_name = "schematic_c_json_string_of_value"
let json_to_value_callback_name = "schematic_c_json_string_to_value"

(* Globally register JSON conversion callbacks, to be invoked by our bundled C
   runtime. *)
let json_string_of_value (v : Ezjsonm.value) : string =
  Ezjsonm.value_to_string v

let json_string_to_value (s : string) : Ezjsonm.value =
  Ezjsonm.value_from_string s

let register_callbacks () =
  Stdlib.Callback.register json_of_value_callback_name json_string_of_value;
  Stdlib.Callback.register json_to_value_callback_name json_string_to_value;
  ()
