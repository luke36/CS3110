module type Monad = sig
  type 'a t

  val return : 'a -> 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module MaybeMonad = struct
  type 'a t = 'a option

  let return a = Some a

  let ( >>= ) a f = match a with None -> None | Some a' -> f a'
end

open MaybeMonad

let ( + ) a b = return (a + b)

let ( - ) a b = return (a - b)

let ( * ) a b = return (a * b)

let ( / ) a b = if b = 0 then None else return (a / b)

let res = return 0 >>= ( + ) 3 >>= ( * ) 1 >>= ( / ) 1
