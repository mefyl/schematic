type 'a t =
  | ( :: ) : 'hd * 'tl t -> ('hd -> 'tl) t
  | [] : unit t
