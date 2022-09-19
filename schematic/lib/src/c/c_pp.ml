let braced ~indent lhs body =
  let open Fmt in
  let opening =
    hbox @@ (const string lhs ++ const char ' ' ++ const char '{')
  in
  vbox ~indent (opening ++ cut ++ vbox body) ++ cut ++ const char '}'
