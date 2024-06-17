let alkaline_earth_metals = [
  ("beryllium", 4);
  ("magnesium", 12);
  ("calcium", 20);
  ("strontium", 38);
  ("barium", 56);
  ("radium", 88)
]

let rec print_tuples = function
  | [] -> ()
  | (a, b) :: t ->
      Printf.printf "(%s, %i); " a b;
      print_tuples t

let rec highest_atomic_number_acc acc = function
  | [] -> acc
  | h :: t ->
    match h with (_, x) -> if x > acc
      then highest_atomic_number_acc x t
      else highest_atomic_number_acc acc t

let highest_atomic_number = highest_atomic_number_acc 0

let () = highest_atomic_number alkaline_earth_metals |> string_of_int |> print_endline

(* ------------------------------------------------------------------------------------ *)

let rec insert_by_weight x = function
  | [] -> [x]
  | (name, weight) :: t ->
    match x with (x_name, x_weight) -> if x_weight <= weight
    then x :: (name, weight) :: t
    else (name, weight) :: insert_by_weight x t

let rec sort_by_weight = function
  | [] -> []
  | h :: t -> insert_by_weight h (sort_by_weight t)

let () = sort_by_weight alkaline_earth_metals |> print_tuples
let () = print_newline ()

(* ------------------------------------------------------------------------------------ *)

let noble_gasses = [
  ("helium", 2);
  ("neon", 10);
  ("argon", 18);
  ("krypton", 36);
  ("xenon", 54);
  ("radon", 86)
]

let rec merge_lists l = function
  | [] -> l
  | h :: t -> h :: merge_lists l t

let rec insert_by_name x = function
  | [] -> [x]
  | (name, weight) :: t ->
    match x with (x_name, x_weight) -> if x_name <= name
    then x :: (name, weight) :: t
    else (name, weight) :: insert_by_name x t

let rec sort_by_name = function
  | [] -> []
  | h :: t -> insert_by_name h (sort_by_name t)

let () = merge_lists alkaline_earth_metals noble_gasses |> sort_by_name |> print_tuples