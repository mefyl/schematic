open Base

module Bool_monadic (M : Base.Monad.S) = struct
  type nonrec t = bool M.t

  let return = M.return
end

module Int_monadic (M : Base.Monad.S) = struct
  type nonrec t = int M.t

  let return = M.return
end

module List_monadic (M : Base.Monad.S) = struct
  type nonrec 'a t = 'a M.t list M.t

  let return l = M.return (List.map ~f:M.return l)
end

module String_monadic (M : Base.Monad.S) = struct
  type nonrec t = string M.t

  let return = M.return
end
