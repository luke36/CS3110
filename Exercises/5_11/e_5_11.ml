(* complex synonym *)
module type ComplexSig = sig
  type t = float * float

  val zero : t

  val add : t -> t -> t
end

(* complex encapsulation *)
module Complex : ComplexSig = struct
  type t = float * float

  let zero = (0., 0.)

  let add (r1, i1) (r2, i2) = (r1 +. r2, i1 +. i2)
end

(* big list queue *)
module type Queue = sig
  (* An ['a t] is a queue whose elements have type ['a]. *)
  type 'a t

  (* Raised if [front] or [dequeue] is applied to the empty queue. *)
  exception Empty

  (* [empty] is the empty queue. *)
  val empty : 'a t

  (* [is_empty q] is whether [q] is empty. *)
  val is_empty : 'a t -> bool

  (* [enqueue x q] is the queue [q] with [x] added to the end. *)
  val enqueue : 'a -> 'a t -> 'a t

  (* [front q] is the element at the front of the queue. Raises [Empty]
      if [q] is empty. *)
  val front : 'a t -> 'a

  (* [dequeue q] is the queue containing all the elements of [q] except the
      front of [q]. Raises [Empty] is [q] is empty. *)
  val dequeue : 'a t -> 'a t

  (* [size q] is the number of elements in [q]. *)
  val size : 'a t -> int

  (* [to_list q] is a list containing the elements of [q] in order from
      front to back. *)
  val to_list : 'a t -> 'a list
end

module ListQueue : Queue = struct
  (* The list [x1; x2; ...; xn] represents the queue with [x1] at its front,
      followed by [x2], ..., followed by [xn]. *)
  type 'a t = 'a list

  exception Empty

  let empty = []

  let is_empty = function [] -> true | _ -> false

  let enqueue x q = q @ [ x ]

  let front = function [] -> raise Empty | x :: _ -> x

  let dequeue = function [] -> raise Empty | _ :: q -> q

  let size = List.length

  let to_list = Fun.id
end

(* Creates a ListQueue filled with [n] elements. *)
let fill_listqueue n =
  let rec loop n q =
    if n = 0 then q else loop (n - 1) (ListQueue.enqueue n q)
  in
  loop n ListQueue.empty

(* big batched queue *)
module BatchedQueue : Queue = struct
  (* [{o; i}] represents the queue [o @ List.rev i]. For example,
      [{o = [1; 2]; i = [5; 4; 3]}] represents the queue [1, 2, 3, 4, 5],
      where [1] is the front element. To avoid ambiguity about emptiness,
      whenever only one of the lists is empty, it must be [i]. For example,
      [{o = [1]; i = []}] is a legal representation, but [{o = []; i = [1]}]
      is not. This implies that if [o] is empty, [i] must also be empty. *)
  type 'a t = { o : 'a list; i : 'a list }

  exception Empty

  let empty = { o = []; i = [] }

  let is_empty = function { o = []; i = _ } -> true | _ -> false

  let enqueue x = function
    | { o = []; i = _ } -> { o = [ x ]; i = [] }
    | { o; i } -> { o; i = x :: i }

  let front = function
    | { o = []; i = _ } -> raise Empty
    | { o = h :: _; i = _ } -> h

  let dequeue = function
    | { o = []; i = _ } -> raise Empty
    | { o = [ _ ]; i } -> { o = List.rev i; i = [] }
    | { o = _ :: t; i } -> { o = t; i }

  let size { o; i } = List.(length o + length i)

  let to_list { o; i } = o @ List.rev i
end

let fill_batchedqueue n =
  let rec loop n q =
    if n = 0 then q else loop (n - 1) (BatchedQueue.enqueue n q)
  in
  loop n BatchedQueue.empty

(* queue efficiency *)
module type Map = sig
  type ('k, 'v) t
  (* [('k, 'v) t] is the type of maps that bind keys of type ['k] to
      values of type ['v]. *)

  val empty : ('k, 'v) t
  (* [empty] does not bind any keys. *)

  val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
  (* [insert k v m] is the map that binds [k] to [v], and also contains
      all the bindings of [m].  If [k] was already bound in [m], that old
      binding is superseded by the binding to [v] in the returned map. *)

  val lookup : 'k -> ('k, 'v) t -> 'v
  (* [lookup k m] is the value bound to [k] in [m]. Raises: [Not_found] if [k]
      is not bound in [m]. *)

  val bindings : ('k, 'v) t -> ('k * 'v) list
  (* [bindings m] is an association list containing the same bindings as [m].
      The keys in the list are guaranteed to be unique. *)
end

module BstMap : Map = struct
  type ('k, 'v) t = E | T of ('k, 'v) t * 'k * 'v * ('k, 'v) t

  let empty = E

  let insert key value tree =
    let rec ins = function
      | E -> T (E, key, value, E)
      | T (lt, k, _, gt) ->
          if key < k then ins lt
          else if key > k then ins gt
          else T (lt, k, value, gt)
    in
    ins tree

  let rec lookup key = function
    | E -> raise Not_found
    | T (lt, k, v, rt) ->
        if key < k then lookup key lt else if key > k then lookup key rt else v

  let rec bindings = function
    | E -> []
    | T (lt, k, v, rt) -> bindings lt @ [ (k, v) ] @ bindings rt
end

(* fraction *)
module type Fraction = sig
  (* A fraction is a rational number p/q, where q != 0.*)
  type t

  val make : int -> int -> t
  (** [make n d] is n/d. Requires d != 0. *)

  val numerator : t -> int

  val denominator : t -> int

  val to_string : t -> string

  val to_float : t -> float

  val add : t -> t -> t

  val mul : t -> t -> t
end

module IntFraction : Fraction = struct
  type t = int * int

  let make a b = if b = 0 then raise Division_by_zero else (a, b)

  let numerator (a, _) = a

  let denominator (_, b) = b

  let to_string (a, b) = string_of_int a ^ " / " ^ string_of_int b

  let to_float (a, b) = float_of_int a /. float_of_int b

  let add (a1, b1) (a2, b2) = ((a1 * b2) + (a2 * b1), b1 * b2)

  let mul (a1, b1) (a2, b2) = (a1 * a2, b1 * b2)
end

(* make char map *)
module CharMap = Map.Make (Char)

(* char ordered *)
(* ommitted here *)

(* use char map *)
let mapSb =
  CharMap.(
    empty |> add 'A' "Alpha" |> add 'E' "Echo" |> add 'S' "Sierra"
    |> add 'V' "Victor")

let eV = CharMap.find 'E' mapSb

let mapSb' = mapSb |> CharMap.remove 'A'

let bA = CharMap.mem 'A' mapSb'

let bindings = CharMap.bindings mapSb'

(* bindings *)
let b1 = CharMap.(empty |> add 'x' 0 |> add 'y' 1 |> bindings)

let b2 = CharMap.(empty |> add 'y' 1 |> add 'x' 0 |> bindings)

let b3 =
  CharMap.(
    empty |> add 'x' 2 |> add 'y' 1 |> remove 'x' |> add 'x' 0 |> bindings)

(* date order *)
type date = { month : int; day : int }

module Date = struct
  type t = date

  let compare { month = m1; day = d1 } { month = m2; day = d2 } =
    if m1 <> m2 then m1 - m2 else d1 - d2
end

(* calender *)
module DateMap = Map.Make (Date)

type calendar = string DateMap.t

let birthday : calendar =
  DateMap.(empty |> add { month = 11; day = 15 } "birthday")

(* print calendar *)
let print_calendar =
  let print { month = m; day = d } event =
    print_endline (string_of_int m ^ "." ^ string_of_int d ^ "\t" ^ event)
  in
  DateMap.iter print

(* is for *)
let is_for = CharMap.mapi (fun ch str -> String.make 1 ch ^ " is for " ^ str)

(* first after *)
let first_after (cal : calendar) date =
  snd (DateMap.find_first (fun d -> Date.compare d date > 0) cal)
(* wait, 2 functions from Map.S?? *)

(* sets *)
module CiString = struct
  type t = string

  let compare s1 s2 =
    let s1' = String.uppercase_ascii s1 in
    let s2' = String.uppercase_ascii s2 in
    String.compare s1' s2'
end

module CiStringSet = Set.Make (CiString)

(* ToString *)
module type ToString = sig
  type t

  val to_string : t -> string
end

module Print (M : ToString) = struct
  include M

  let print t = print_string (to_string t)
end

(* Print Int *)
module PrintInt = Print (Int)

(* Print String *)
module MyString = struct
  include String

  let to_string s = s
end

module PrintString = Print (MyString)

(* Print Reuse *)
(* omitted here *)

(* Print String reuse revisited *)
module StringWithPrint = struct
  include Print (MyString)
  include String
end

(* implenmentation without interface *)
(* in another file *)

(* implenmentation with interface *)
(* in another file *)

(* implementation with abstracted interface *)
(* in another file *)

(* print for date *)
(* in another file *)

(* refactor  arith *)
