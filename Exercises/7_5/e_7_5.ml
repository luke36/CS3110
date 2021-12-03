(* mutable fields *)
type grade = { name : string; mutable gpa : float }

let alice = { name = "Alice"; gpa = 3.7 }

let _ = alice.gpa <- 4.0

(* refs *)
let _ = ref true

let _ = ref [ 1; 2 ]

let _ = [ ref 1; ref 2 ]

(* inc fun *)
let inc = ref (fun x -> x + 1)

let _ = !inc 3109

(* addition assignment *)
let ( +:= ) x y = x := !x + y

(* physical equality *)
(* omitted here *)

(* norm *)
type vector = float array

let norm v =
  Array.(v |> map (fun x -> x *. x) |> Array.fold_left ( +. ) 0. |> sqrt)

(* normalize *)
let normalize vect =
  let len = norm vect in
  Array.iteri (fun i x -> vect.(i) <- x /. len) vect

(* norm loop *)
let norm' v =
  let res = ref 0. in
  for i = 0 to Array.length v do
    res := !res +. v.(i)
  done;
  !res

(* normalize loop *)
let normalize' v =
  let nor = norm' v in
  for i = 0 to Array.length v do
    v.(i) <- v.(i) /. nor
  done

(* init matrix *)
let init_matrix f x y =
  let res = Array.make_matrix x y 0 in
  for i = 0 to x do
    for j = 0 to y do
      res.(i).(j) <- f i j
    done
  done;
  res

(* doubly linked list *)

type 'a node = {
  mutable prev : 'a node option;
  mutable next : 'a node option;
  value : 'a;
}

type 'a dlist = {
  mutable first : 'a node option;
  mutable last : 'a node option;
}
