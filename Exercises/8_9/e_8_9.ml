(* hash insert *)
(* omitted *)

(* relax bucket RI *)
(* omitted *)

(* strengthen bucket RI *)
(* omitetd *)

(* hash values *)
let _ = Hashtbl.hash ()

let _ = Hashtbl.hash false

let _ = Hashtbl.hash true

let _ = Hashtbl.hash 0

let _ = Hashtbl.hash 1

let _ = Hashtbl.hash ""

let _ = Hashtbl.hash []

let _ = Hashtbl.hash [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9 ]

let _ = Hashtbl.hash [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ]

(* hashtbl usage *)
let tab = Hashtbl.create 16

let _ =
  List.init 31 (fun n -> n)
  |> List.iter (fun n -> Hashtbl.add tab n (string_of_int n))

let _ = Hashtbl.find tab 17

let _ = try Hashtbl.find tab (-1) with Not_found -> ""

(* hashtbl stats *)

let _ = Hashtbl.stats tab

(* hashtbl bindings *)

let bindings h = Hashtbl.fold (fun k v acc -> (k, v) :: acc) h

(* hashtbl load factor *)

let load_factor h =
  let stat = Hashtbl.stats h in
  float_of_int stat.num_bindings /. float_of_int stat.num_buckets

(* functional interface *)
module CiString = struct
  type t = string

  let equal s1 s2 = String.lowercase_ascii s1 = String.lowercase_ascii s2

  let hash s = Hashtbl.hash (String.lowercase_ascii s)
end

module CiStringHashTbl = Hashtbl.Make (CiString)

let tab' = CiStringHashTbl.create 16

let _ = CiStringHashTbl.add tab' "sss" 13

(* equals and hash *)
(* omitted *)

(* bad hash *)
module Sb = struct
  type t = int

  let equal = ( = )

  let hash _ = 10
end

module SbHashtbl = Hashtbl.Make (Sb)

let tab'' = SbHashtbl.create 16

let _ = SbHashtbl.add tab'' 13 13

let _ = SbHashtbl.add tab'' 16 13

(* linear probing *)
module ProbHashtbl = struct
  exception Insert_fail

  type ('k, 'v) cell = Empty | Delete | Full of 'k * 'v

  type ('k, 'v) t = {
    hash : 'k -> int;
    mutable size : int;
    mutable n_delete : int;
    mutable capacity : int;
    mutable store : ('k, 'v) cell array;
  }

  let create hash cap =
    {
      hash;
      size = 0;
      n_delete = 0;
      capacity = cap;
      store = Array.make cap Empty;
    }

  let rec try_insert ind stop k v tbl =
    if ind <> stop then
      match tbl.store.(ind) with
      | Empty ->
          tbl.store.(ind) <- Full (k, v);
          tbl.size <- tbl.size + 1
      | Delete ->
          tbl.store.(ind) <- Full (k, v);
          tbl.size <- tbl.size + 1;
          tbl.n_delete <- tbl.n_delete - 1
      | Full _ -> try_insert ((ind + 1) mod tbl.capacity) stop k v tbl

  let rec find_aux ind stop k tbl =
    if ind == stop then None
    else
      match tbl.store.(ind) with
      | Empty -> None
      | Full (k', v) when k = k' -> Some v
      | _ -> find_aux ((ind + 1) mod tbl.capacity) stop k tbl

  let rec delete_aux ind stop k tbl =
    if ind <> stop then
      match tbl.store.(ind) with
      | Empty -> ()
      | Full (k', _) when k = k' ->
          tbl.store.(ind) <- Delete;
          tbl.size <- tbl.size - 1;
          tbl.n_delete <- tbl.n_delete + 1
      | _ -> delete_aux ((ind + 1) mod tbl.capacity) stop k tbl

  let rec insert k v tbl =
    if tbl.size + tbl.n_delete > tbl.capacity / 2 then
      resize (2 * tbl.capacity) tbl;
    let ind = tbl.hash k mod tbl.capacity in
    match tbl.store.(ind) with
    | Empty ->
        tbl.store.(ind) <- Full (k, v);
        tbl.size <- tbl.size + 1
    | Delete ->
        tbl.store.(ind) <- Full (k, v);
        tbl.size <- tbl.size + 1;
        tbl.n_delete <- tbl.n_delete - 1
    | Full _ -> try_insert ((ind + 1) mod tbl.capacity) ind k v tbl

  and resize new_cap tbl =
    let old = tbl.store in
    tbl.store <- Array.make new_cap Empty;
    tbl.capacity <- new_cap;
    tbl.size <- 0;
    tbl.n_delete <- 0;
    let help = function Full (k, v) -> insert k v tbl | _ -> () in
    old |> Array.iter help

  let delete k tbl =
    let ind = tbl.hash k mod tbl.capacity in
    match tbl.store.(ind) with
    | Empty -> ()
    | Full (k', _) when k = k' ->
        tbl.store.(ind) <- Delete;
        tbl.size <- tbl.size - 1;
        tbl.n_delete <- tbl.n_delete + 1
    | _ ->
        delete_aux ((ind + 1) mod tbl.capacity) ind k tbl;
        if tbl.size < tbl.capacity / 4 then resize (tbl.capacity / 2) tbl
end

(* functorized BST *)
module type Set = sig
  type t

  type elt

  val empty : t

  val lookup : elt -> t -> bool

  val insert : elt -> t -> t

  val elements : t -> elt list
end

module BstSet (M : Set.OrderedType) : Set with type elt = M.t = struct
  type elt = M.t

  type t = E | T of t * elt * t

  let empty = E

  let insert e tree =
    let rec ins = function
      | E -> T (E, e, E)
      | T (lt, e', gt) ->
          if e < e' then ins lt else if e > e' then ins gt else T (lt, e, gt)
    in
    ins tree

  let rec lookup e = function
    | E -> false
    | T (lt, e', rt) ->
        if e < e' then lookup e lt else if e > e' then lookup e rt else true

  let rec elements = function
    | E -> []
    | T (lt, e, rt) -> elements lt @ [ e ] @ elements rt
end

(* efficient traversal *)

type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

let rec pre_aux acc = function
  | Leaf -> acc
  | Node (l, v, r) ->
      let res1 = pre_aux acc r in
      let res2 = pre_aux res1 l in
      v :: res2

let preorder = pre_aux []

let rec in_aux acc = function
  | Leaf -> acc
  | Node (l, v, r) ->
      let res1 = in_aux acc r in
      let res2 = v :: res1 in
      in_aux res2 l

let inorder = in_aux []

let rec post_aux acc = function
  | Leaf -> acc
  | Node (l, v, r) ->
      let res1 = v :: acc in
      let res2 = post_aux res1 r in
      post_aux res2 l

let postorder = post_aux []

let t =
  Node
    ( Node (Node (Leaf, 1, Leaf), 2, Node (Leaf, 3, Leaf)),
      4,
      Node (Node (Leaf, 5, Leaf), 6, Node (Leaf, 7, Leaf)) )

(*
  t is
        4
      /   \
     2     6
    / \   / \
   1   3 5   7
*)

let () = assert (preorder t = [ 4; 2; 1; 3; 6; 5; 7 ])

let () = assert (inorder t = [ 1; 2; 3; 4; 5; 6; 7 ])

let () = assert (postorder t = [ 1; 3; 2; 5; 7; 6; 4 ])

(* RB draw complete *)
(* omitted *)

(* Rb draw insert *)
(* omitted *)

(* standard library set *)
(* omitted *)

(* pow2 *)
type 'a sequence = Cons of 'a * (unit -> 'a sequence)

let rec pow2_aux n = Cons (n, fun () -> pow2_aux (n * 2))

let pow2 = pow2_aux 1

(* more sequences *)
let even_aux n = Cons (n, fun () -> pow2_aux (n + 2))

let even = even_aux 0

let rec low_aux c =
  let nextc c = if c = 'z' then 'a' else Char.(chr (code c + 1)) in
  Cons (c, fun () -> low_aux (nextc c))

let low_rep = low_aux 'a'

let rec coin_aux () = Cons (Random.bool (), fun () -> coin_aux ())

let coin_flip = coin_aux ()

(* nth *)
let rec nth n (Cons (h, t)) = if n = 0 then h else nth (n - 1) (t ())

(* hd tl *)
(* omitted *)

(* filter *)
let rec filter f (Cons (h, t)) =
  if f h then Cons (h, fun () -> filter f (t ())) else filter f (t ())

(* interleave *)
let rec interleave (Cons (h1, t1)) (Cons (h2, t2)) =
  Cons (h1, fun () -> Cons (h2, fun () -> interleave (t1 ()) (t2 ())))

(* sift *)
let sift n = filter (fun m -> m mod n <> 0)

(* primes *)
let rec filter_prime (Cons (h, t)) =
  Cons (h, fun () -> filter_prime (sift h (t ())))

let rec from n = Cons (n, fun () -> from (n + 1))

let primes = filter_prime (from 2)

(* approximately e *)
let rec fact n = if n = 0 then 1 else n * fact (n - 1)

let e_terms x =
  let f k = (x ** float_of_int k) /. float_of_int (fact k) in
  let rec from n = Cons (f n, fun () -> from (n + 1)) in
  from 0

let total seq =
  let rec help acc (Cons (h, t)) =
    Cons (acc +. h, fun () -> help (acc +. h) (t ()))
  in
  help 0. seq

let rec within eps (Cons (h, t)) =
  let (Cons (h', t')) = t () in
  if abs_float (h -. h') < eps then h' else within eps (Cons (h', t'))

let e ?x:(arg = 1.) eps = arg |> e_terms |> total |> within eps

(* better e *)
let rec e_terms_aux x k acc =
  Cons (acc, fun () -> e_terms_aux x (k + 1) (acc /. float_of_int k *. x))

let e_terms' x = e_terms_aux x 1 1.

let rec within' eps (Cons (h, t)) =
  let (Cons (h', t')) = t () in
  let test a b =
    abs_float (a -. b) /. (((abs_float a +. abs_float b) /. 2.) +. 1.)
  in
  if test h h' < eps then h' else within eps (Cons (h', t'))

let e' ?x:(arg = 1.) eps = arg |> e_terms' |> total |> within' eps

(* diffrent sequence rep *)
type 'a sequence = Cons of (unit -> 'a * 'a sequence)

let hd (Cons s) = match s () with h, _ -> h

let tl (Cons s) = match s () with _, t -> t

let rec from n = Cons (fun () -> (n, from (n + 1)))

let nat = from 0

let rec map f (Cons s) =
  Cons (fun () -> match s () with h, t -> (f h, map f t))

(* lazy hello *)
let lazy_hello : unit Lazy.t = lazy (print_endline "hello")

(* lazy and *)
let ( &&& ) lb1 lb2 = if not (Lazy.force lb1) then false else Lazy.force lb2

(* lazy sequence *)
type 'a lazysequence = Cons of 'a * 'a lazysequence Lazy.t

let rec map f (Cons (h, t)) = Cons (f h, lazy (map f (Lazy.force t)))

let rec filter f (Cons (h, t)) =
  if f h then Cons (h, lazy (filter f (Lazy.force t)))
  else filter f (Lazy.force t)

(* promise and resolve *)

module type PROMISE = sig
  type 'a state = Pending | Resolved of 'a | Rejected of exn

  type 'a promise

  type 'a resolver

  val make : unit -> 'a promise * 'a resolver

  val return : 'a -> 'a promise

  val state : 'a promise -> 'a state

  val resolve : 'a resolver -> 'a -> unit

  val reject : 'a resolver -> exn -> unit

  val ( >>= ) : 'a promise -> ('a -> 'b promise) -> 'b promise
end

module Promise : PROMISE = struct
  type 'a state = Pending | Resolved of 'a | Rejected of exn

  type 'a handler = 'a state -> unit

  type 'a promise = {
    mutable state : 'a state;
    mutable handlers : 'a handler list;
  }

  let enqueue (handler : 'a state -> unit) (promise : 'a promise) : unit =
    promise.handlers <- handler :: promise.handlers

  type 'a resolver = 'a promise

  let write_once p s =
    if p.state = Pending then p.state <- s else invalid_arg "cannot write twice"

  let make () =
    let p = { state = Pending; handlers = [] } in
    (p, p)

  let return x = { state = Resolved x; handlers = [] }

  let state p = p.state

  let resolve_or_reject (r : 'a resolver) (st : 'a state) =
    assert (st <> Pending);
    let handlers = r.handlers in
    r.handlers <- [];
    write_once r st;
    List.iter (fun f -> f st) handlers

  let reject r x = resolve_or_reject r (Rejected x)

  let resolve r x = resolve_or_reject r (Resolved x)

  let handler (resolver : 'a resolver) : 'a handler = function
    | Pending -> failwith "handler RI violated"
    | Rejected exc -> reject resolver exc
    | Resolved x -> resolve resolver x

  let handler_of_callback (callback : 'a -> 'b promise) (resolver : 'b resolver)
      : 'a handler = function
    | Pending -> failwith "handler RI violated"
    | Rejected exc -> reject resolver exc
    | Resolved x -> (
        let promise = callback x in
        match promise.state with
        | Resolved y -> resolve resolver y
        | Rejected exc -> reject resolver exc
        | Pending -> enqueue (handler resolver) promise)

  let ( >>= ) (input_promise : 'a promise) (callback : 'a -> 'b promise) :
      'b promise =
    match input_promise.state with
    | Resolved x -> callback x
    | Rejected exc -> { state = Rejected exc; handlers = [] }
    | Pending ->
        let output_promise, output_resolver = make () in
        enqueue (handler_of_callback callback output_resolver) input_promise;
        output_promise
end

let p, r = Promise.make ()

let p' = Promise.(p >>= fun n -> Promise.return (print_int n))

let _ = Promise.resolve r 90
(* i still do not quite understand *)

(* promise and resolve lwt *)
let p, r = Lwt.wait ()

let p' = Lwt.bind p (fun n -> Lwt_io.printf "%d\n" n)

let _ = Lwt.wakeup r 90

(* timing challenge 1 *)
open Lwt.Infix

let delay (sec : float) : unit Lwt.t = Lwt_unix.sleep sec

let delay_then_print () = delay 3. >>= fun _ -> Lwt_io.print "done"

(* timing challenge 2 *)
let timing2 () =
  let _t1 = delay 1. >>= fun () -> Lwt_io.printl "1" in
  let _t2 = delay 10. >>= fun () -> Lwt_io.printl "2" in
  let _t3 = delay 20. >>= fun () -> Lwt_io.printl "3" in
  Lwt_io.printl "all done"

(* timing challenge 3 *)
let timing3 () =
  delay 1. >>= fun () ->
  Lwt_io.printl "1" >>= fun () ->
  delay 10. >>= fun () ->
  Lwt_io.printl "2" >>= fun () ->
  delay 20. >>= fun () ->
  Lwt_io.printl "3" >>= fun () -> Lwt_io.printl "all done"

(* timing challenge 4 *)
let timing4 () =
  let t1 = delay 1. >>= fun () -> Lwt_io.printl "1" in
  let t2 = delay 10. >>= fun () -> Lwt_io.printl "2" in
  let t3 = delay 20. >>= fun () -> Lwt_io.printl "3" in
  Lwt.join [ t1; t2; t3 ] >>= fun () -> Lwt_io.printl "all done"

(* file monitor *)

(* later *)

(* add opt *)
module type Monad = sig
  type 'a t

  val return : 'a -> 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module Maybe : Monad = struct
  type 'a t = 'a option

  let return x = Some x

  let ( >>= ) m f = match m with Some x -> f x | None -> None
end

open Maybe

let add (x : int Maybe.t) (y : int Maybe.t) =
  x >>= fun n ->
  y >>= fun m -> return (m + n)

(* fmap and join *)
module type ExtMonad = sig
  type 'a t

  val return : 'a -> 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

  val join : 'a t t -> 'a t
end

module Maybe : ExtMonad = struct
  type 'a t = 'a option

  let return x = Some x

  let ( >>= ) m f = match m with Some x -> f x | None -> None

  let ( >>| ) m f = match m with Some x -> Some (f x) | None -> None

  let join m = match m with Some m' -> m' | None -> None
end

(* fmap and join again *)
module Maybe : ExtMonad = struct
  type 'a t = 'a option

  let return x = Some x

  let ( >>= ) m f = match m with Some x -> f x | None -> None

  let ( >>| ) m f = m >>= fun x -> return (f x)

  let join m = m >>= fun x -> x
end

(* bind from fmap + join *)
module type FmapJoinMonad = sig
  type 'a t

  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

  val join : 'a t t -> 'a t

  val return : 'a -> 'a t
end

module type BindMonad = sig
  type 'a t

  val return : 'a -> 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module MakeMonad (M : FmapJoinMonad) : BindMonad = struct
  type 'a t = 'a M.t

  let return = M.return

  let ( >>= ) m f = M.(m >>| f |> join)
end

(* list monad *)
module type ExtMonad = sig
  type 'a t

  val return : 'a -> 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

  val join : 'a t t -> 'a t
end

module ListMonad : ExtMonad = struct
  type 'a t = 'a list

  let return x = [ x ]

  let ( >>| ) l f = List.map f l

  let join [ l ] = l

  let ( >>= ) m f = m |> List.map f |> List.concat
end
(* trivial monad laws *)
(* omitted *)
