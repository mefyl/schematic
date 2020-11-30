open Base

module Bool_monadic (M : Base.Monad.S) = struct
  type nonrec t = bool M.t

  let return = M.return

  let bind = M.bind
end

module Int_monadic (M : Base.Monad.S) = struct
  type nonrec t = int M.t

  let return = M.return

  let bind v ~f = M.bind v ~f
end

module List_monadic (M : Base.Monad.S) = struct
  type nonrec 'a t = 'a M.t list M.t

  let return l = M.return (List.map ~f:M.return l)

  let bind l ~f:f_ =
    let rec f = function
      | [] -> M.return []
      | h :: t ->
        M.bind h ~f:(fun h -> M.bind (f t) ~f:(fun t -> M.return @@ (h :: t)))
    in
    M.bind l ~f:(fun l -> M.bind (f l) ~f:(fun l -> f_ l))
end

module String_monadic (M : Base.Monad.S) = struct
  type nonrec t = string M.t

  let return = M.return

  let bind = M.bind
end
