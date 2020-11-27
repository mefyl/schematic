module Bool_monadic (M : Base.Monad.S) = struct
  type nonrec t = bool M.t
end

module Int_monadic (M : Base.Monad.S) = struct
  type nonrec t = int M.t
end

module List_monadic (M : Base.Monad.S) = struct
  type nonrec 'a t = 'a M.t list M.t
end

module String_monadic (M : Base.Monad.S) = struct
  type nonrec t = string M.t
end
