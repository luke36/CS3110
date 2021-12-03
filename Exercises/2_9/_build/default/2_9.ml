(* values *)
(* int; string *)

(* operators *)
let _ = 42 * 10
let _ = 3.14 /. 2.0
let _ = 4.2 ** 7.0

(* equality *)
let _ = 42 = 42
let _ = "hi" = "hi"
let _ = "hi" == "hi"

(* assert *)
let _ = assert true
(* let _ = assert false *)
let _ = assert (2110 <> 3110)

(* if *)
let _ = if 2 > 1 then 42 else 7

(** double fun *)
let double x = x *. 2.
let _ = assert (double 3. = 6.)
let _ = assert (double 2.718 = 5.436)

(* more fun *)
let cube x = x *. x *. x
let sign x =
  if x > 0 then 1 else
    if x < 0 then -1 else
      0
let area_circle r =
  let pi = Float.pi in
    r *. r *. pi
let _ = assert (area_circle 3. = 9. *. Float.pi)
let _ = assert (area_circle 2.5 = 6.25 *. Float.pi)

let root_mean_square x y = Float.sqrt ((x *. x +. y *. y) /. 2.)
let _ = assert (root_mean_square 4. 4. = 4.)
let _ = assert (root_mean_square 1. 7. = 5.)

(* date fun *)
let valid_date d m =
  d > 0 &&
  if m = "Jan" || m = "Mar" || m = "May" ||
     m = "Jul" || m = "Aug" || m = "Oct" || m = "Dec" then
     d < 312
  else
    if m = "Feb" then 
      d < 29
    else
      d < 31

(* fib *)
let rec fib n = if n = 1 || n = 2 then 1 else fib (n - 1) + fib (n - 2)
let _ = assert (fib 5 = 5)
let _ = assert (fib 8 = 21)

(* fib fast *)
let rec fib_fast_aux n1 n2 n = if n = 0 then n1 else fib_fast_aux n2 (n1 + n2) (n - 1)
let fib_fast n = fib_fast_aux 1 1 (n - 1)
let _ = assert (fib_fast 5 = 5)
let _ = assert (fib_fast 8 = 21)

(* poly types *)
(* 'a -> 'a; a' -> bool -> a'; bool -> a' -> a'; bool -> a' -> b' -> a' *)

(* divide *)
let divide ~numerator ~denominater = numerator /. denominater
let _ = divide ~numerator:4. ~denominater:5.

(* associativity *)
(* int; int -> int; int; error *)

(* average *)
let ( +/. ) x y = (x +. y) /. 2.

(* hello world *)
let () = print_endline "Hello world!"
let () = print_string "Hello world!"
