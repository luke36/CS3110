(* list expression *)
let _ = [ 1; 2; 3; 4; 5 ]

let _ = [ 1; 2; 3; 4; 5 ]

let _ = [ 1; 2; 3; 4 ] @ [ 5 ]

(* product *)
let rec product l = match l with [] -> 1 | h :: t -> h * product t

(* concat *)
let rec concat l = match l with [] -> "" | h :: t -> h ^ concat t

(* product test *)
(* in another file *)

(* patterns *)
let fun1 l = match l with [] -> false | h :: _ -> h = "bigred"

let fun2 l =
  match l with [ _; _ ] -> true | [ _; _; _; _ ] -> true | _ -> false

let fun3 l = match l with fst :: scd :: _ -> fst = scd | _ -> false

(* library *)
let take_5th l = if List.length l < 5 then 0 else List.nth l 4

let sort_descend l = l |> List.sort Stdlib.compare |> List.rev

(* library test *)
(* in another file *)

(* library puzzle *)
let last l = List.nth (List.rev l) 0

let any_zeroes l = List.exists (fun x -> x = 0) l

(* take drop *)
let rec take n l =
  match l with [] -> [] | h :: t -> if n = 0 then [] else h :: take (n - 1) t

let rec drop n l =
  match l with [] -> [] | h :: t -> if n = 0 then h :: t else drop (n - 1) t

(* take drop tail *)
let rec take_aux n l acc =
  if n = 0 then acc
  else match l with [] -> [] | h :: t -> take_aux (n - 1) t (h :: acc)

let take_tl n l = take_aux n l [] |> List.rev

(* unimodal *)
let rec decrease l =
  match l with
  | [] -> true
  | [ _ ] -> true
  | fst :: scd :: l -> fst >= scd && decrease (scd :: l)

let rec unimodal l =
  match l with
  | [] -> true
  | [ _ ] -> true
  | fst :: scd :: t ->
      if fst > scd then decrease (scd :: t) else unimodal (scd :: t)

(* powerset *)
let rec powerset l =
  match l with
  | [] -> [ [] ]
  | h :: t ->
      let pow = powerset t in
      pow @ List.map (fun l' -> h :: l') pow

(* print int list rec *)
let rec print_int_list = function
  | [] -> ()
  | h :: t ->
      print_endline (string_of_int h);
      print_int_list t

(* print int list iter *)
let print_int_list' = List.iter (fun x -> print_endline (string_of_int x))

(* student *)
type student = { first_name : string; last_name : string; gpa : float }

let _ = { first_name = "Luke"; last_name = "Young"; gpa = 1. }

let get_srudent_name { first_name; last_name; _ } = (first_name, last_name)

let create_student fstn lstn gpa = { first_name = fstn; last_name = lstn; gpa }

(* pokerecord *)
type poketype = Normal | Fire | Water

type pokemon = { name : string; hp : int; ptype : poketype }

let _ = { name = "charizard"; hp = 78; ptype = Fire }

let _ = { name = "squirtle"; hp = 44; ptype = Water }

(* safe hd and tl *)
let safe_hd l = match l with [] -> None | h :: _ -> Some h

let safe_tl l = match l with [] -> None | _ :: t -> Some t

let rec max_hp l =
  match l with
  | [] -> None
  | h :: t -> (
      match max_hp t with None -> Some h.hp | Some hp -> Some (max hp h.hp))

(* date before *)
type date = int * int * int

let is_before ((y1, m1, d1) : date) ((y2, m2, d2) : date) =
  y1 < y2 || (y1 = y2 && m1 < m2) || (y1 = y2 && m1 = m2 && d1 < d2)

(* earliest date *)
let rec earliest l =
  match l with
  | [] -> None
  | h :: t -> (
      match earliest t with
      | None -> Some h
      | Some dt -> if is_before h dt then Some h else Some dt)

(* assoc list *)
let insert k v lst = (k, v) :: lst

let rec lookup k = function
  | [] -> None
  | (k', v) :: t -> if k = k' then Some v else lookup k t

let assoc_list_test = [] |> insert 1 "one" |> insert 2 "two" |> insert 3 "three"

let rst2 = lookup 2 assoc_list_test

let rst4 = lookup 4 assoc_list_test

(* cards *)
type suit = Clubs | Diamonds | Hearts | Spades

type rank = int

type card = { st : suit; rk : rank }

let ace_of_clubs = { st = Clubs; rk = 1 }

let queen_of_hearts = { st = Hearts; rk = 12 }

let seven_of_spades = { st = Spades; rk = 7 }

(* matching *)
let _ = [ None ]

let _ = [ None ]

let _ = [ None ]

let _ = [ None ]
(* impossible *)

(* quadrant *)
type quad = I | II | III | IV

type sign = Neg | Zero | Pos

let sign (x : int) : sign = if x > 0 then Pos else if x < 0 then Neg else Zero

let quadrant (x, y) =
  match (sign x, sign y) with
  | Pos, Pos -> Some I
  | Pos, Neg -> Some II
  | Neg, Neg -> Some III
  | Neg, Pos -> Some IV
  | _ -> None

let _ = quadrant (1, 2)

type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

(* depth *)
let rec depth tr =
  match tr with
  | Leaf -> 0
  | Node (_, left, right) -> 1 + max (depth left) (depth right)

(* shape *)
let rec shape tr1 tr2 =
  match (tr1, tr2) with
  | Leaf, Leaf -> true
  | Node _, Leaf -> false
  | Leaf, Node _ -> false
  | Node (_, l1, r1), Node (_, l2, r2) -> shape l1 l2 && shape r1 r2

(* list max exn *)
let rec list_max l =
  match l with
  | [] -> raise (Failure "list_max")
  | h :: t -> ( match list_max t with exception Failure _ -> h | m -> max h m)

(* list max exn string *)
let rec list_max_string l =
  match l with
  | [] -> "empty"
  | h :: t -> (
      match list_max_string t with
      | "empty" -> string_of_int h
      | m -> string_of_int (max (int_of_string m) h))

(* list max exn ounit *)
(* in another file *)

(* is_bst *)
let rec tagged_tree tr =
  match tr with
  | Leaf -> Leaf
  | Node ((k, _), left, right) -> (
      let newl = tagged_tree left in
      let newr = tagged_tree right in
      match (newl, newr) with
      | Leaf, Leaf -> Node ((k, k, k), Leaf, Leaf)
      | Node ((min, max, _), _, _), Leaf -> Node ((min, max, k), newl, newr)
      | Leaf, Node ((min, max, _), _, _) -> Node ((min, max, k), newl, newr)
      | Node ((minl, maxl, _), _, _), Node ((minr, maxr, _), _, _) ->
          Node ((min minl minr, max maxl maxr, k), newl, newr))

let rec check_tagged_tree tr =
  match tr with
  | Leaf -> true
  | Node ((_, _, k), l, r) -> (
      check_tagged_tree l && check_tagged_tree r
      &&
      match (l, r) with
      | Leaf, Leaf -> true
      | Node ((_, maxl, _), _, _), Leaf -> maxl <= k
      | Leaf, Node ((minr, _, _), _, _) -> k <= minr
      | Node ((_, maxl, _), _, _), Node ((minr, _, _), _, _) ->
          maxl <= k && k <= minr)

let is_bst tr = tr |> tagged_tree |> check_tagged_tree

(* quadrant poly *)
let sign x = if x > 0 then `Pos else if x < 0 then `Neg else `Zero

let quadrant x y =
  match (sign x, sign y) with
  | `Pos, `Pos -> Some `I
  | `Neg, `Pos -> Some `II
  | `Neg, `Neg -> Some `III
  | `Pos, `Neg -> Some `IV
  | _ -> None
