type t = int

let uid = ref (-1)
let pp = Fmt.(fmt "[%x] ")

let make () =
  let () = uid := !uid + 1 in
  !uid
