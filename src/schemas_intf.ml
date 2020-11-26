module Monadic (M : Base.Monad.S) = struct
  type nonrec 'a list = 'a M.t list M.t

  type nonrec bool = bool M.t

  type nonrec int = int M.t

  type nonrec string = string M.t
end
