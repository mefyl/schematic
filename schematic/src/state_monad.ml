module Make (State : sig
  type t
end) =
struct
  type 'a t = State.t -> 'a * State.t

  include Base.Monad.Make (struct
    type nonrec 'a t = 'a t

    let return v marks = (v, marks)

    let bind v ~f marks =
      let v, marks = v marks in
      f v marks

    let map =
      let map v ~f marks =
        let v, marks = v marks in
        (f v, marks)
      in
      `Custom map
  end)
end
