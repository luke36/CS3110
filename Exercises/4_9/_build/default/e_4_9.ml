(* twice, no arguements *)
let double x = 2 * x

let square x = x * x

let twice f x = f (f x)

let quad = twice double

let fourth = twice square
(* currying *)

(* mystery operator 1 *)
let ( $ ) f x = f x
(* change the priotiry of evaluation. but why? how is the priority of
   a symbol defined by a custom decided? *)

(* mystery operator 2 *)
let ( @@ ) f g x = x |> g |> f
(* equals to combine. but, what i do not know is, how is a symbol
   defined by the custom interpreted if there are more than 2
   parameters? *)

(* repeat *)
let rec repeat f n x = if n = 0 then x else f (repeat f (n - 1) x)

(* product *)
let product_left = List.fold_left ( *. ) 1.0

let product_right = List.fold_right ~f:( *. ) ~accu:1.0

(* terse product *)
(* up here *)

(* sum_cube_odd *)
let rec ( -- ) i j = if i > j then [] else i :: (i + 1 -- j)

let sum_cube_odd n =
  0 -- n
  |> List.filter (fun x -> x = 0)
  |> List.map (fun x -> x * x * x)
  |> List.fold_left ( * ) 1

(* sum_cube_odd *)
(* up here *)

(* exists *)
let rec exists_rec p l =
  match l with [] -> false | h :: t -> p h || exists_rec p t

let exists_fold p = List.fold_left (fun acc x -> p x || acc) false

let exists_lib = List.exists

(* account balance *)
let balance_left b = List.fold_left ( - ) b

let balance_right b l = List.fold_right (fun x accu -> accu - x) l b

let rec balance_rec b l =
  match l with [] -> b | h :: t -> balance_rec (b - h) t

(* library uncurried *)
let uncurried_append (l1, l2) = List.append l1 l2

let uncurried_compare (c1, c2) = Char.compare c1 c2

let uncurried_max (a, b) = Stdlib.max a b

(* map composition *)
let map_composition f g = List.map (fun x -> f (g x))

(* more list fun *)
let fun_string_3 = List.filter (fun s -> String.length s > 3)

let fun_add_1 = List.map (( + ) 1)

let fun_str_sep strs sep =
  match strs with
  | [] -> ""
  | h :: t -> h ^ List.fold_left (fun accu x -> accu ^ sep ^ x) "" t

(* association list keys *)
let unique_keys dic =
  dic |> List.map (fun (key, _) -> key) |> List.sort_uniq (fun _ _ -> -1)

(* valid matrix *)
let rec is_valid_matrix (m : int list list) =
  match m with
  | [] -> false
  | h :: t -> (
      match h with
      | [] -> false
      | _ -> (
          match t with
          | [] -> true
          | h' :: _ -> List.length h = List.length h' && is_valid_matrix t))

(* row vector add *)
let add_row_vectors = List.map2 ( + )

(* matrix add *)
let add_matrices = List.map2 add_row_vectors

(* matrix multiply *)
let multiply_matrices m1 m2 =
  let number_multi_row_vector n v = List.map (fun x -> x * n) v in
  let get_a_row r m =
    List.fold_left2
      (fun acc n r -> add_row_vectors acc (number_multi_row_vector n r))
      (List.init (List.length (List.hd m)) (fun _ -> 0))
      r m
  in
  List.map (fun r -> get_a_row r m2) m1
