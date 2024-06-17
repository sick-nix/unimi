type matrix = int list list

let rec print_list = function
  | [] -> ()
  | h :: t ->
    Printf.printf "%d\t" h;
    print_list t

let rec print_matrix = function
  | [] -> ()
  | h :: t -> 
    print_list h;
    print_string "\n";
    print_matrix t

let rec nth_acc (acc: int) (i: int) = function
  | [] -> None
  | h :: t when i = acc -> Some h
  | h :: t -> nth_acc (acc + 1) i t

let nth i l = nth_acc 0 i l
let int_of_nth i l = match nth i l with
  | None -> 0
  | Some v -> v
let list_of_nth i l = match nth i l with
  | None -> []
  | Some v -> v
let matrix_nth r c l = list_of_nth r l |> int_of_nth c

let rec count_acc acc = function
| [] -> acc
| h :: t -> count_acc (acc + 1) t

let count l = count_acc 0 l

let get_nrows l = count l

let rec get_ncolumns (l: matrix) = match nth 0 l with
  | None -> 0
  | Some v -> count v

let rec make_list_acc r c nrows ncolumns (p : int -> int -> int -> int -> int) = match c with
  | x when x = ncolumns -> []
  | x -> p r c nrows ncolumns :: make_list_acc r (c + 1) nrows ncolumns p

let rec make_list r nrows ncolumns p = make_list_acc r 0 nrows ncolumns p

let rec make_matrix_acc r nrows ncolumns p = match r with
| x when x = nrows -> []
| x -> make_list r nrows ncolumns p :: make_matrix_acc (r + 1) nrows ncolumns p

let make_matrix nrows ncolumns p = make_matrix_acc 0 nrows ncolumns p



(* 1: A function zeroes to construct a matrix of size n×m filled with zeros. *)
let zeroes nrows ncolumns = make_matrix nrows ncolumns (fun r' c' nrows' ncolumns' -> 0)

let zeroes_matrix = zeroes 3 4
let () = print_matrix zeroes_matrix
let () = print_string "\n\n"



(* 2: A function identity to construct the identity matrix
  (the one with all 0s but the 1s on the diagonal) of given size. *)
let identity nrows ncolumns = match nrows with
  | x when x = ncolumns -> Some (make_matrix nrows ncolumns (fun r' c' nrows' ncolumns' -> if r' = c' then 1 else 0))
  | x -> None
let identity_matrix = identity 3 3
let () = match identity_matrix with
  | Some v -> print_matrix v
  | None -> print_string "The identity matrix has to have same number of rows and columns"
let () = print_string "\n\n"



(* 3: A function init to construct a square matrix of a given size n filled with the first n×n integers. *)
let init_nsquare n m = make_matrix n m (fun r' c' nrows' ncolumns' -> r' * ncolumns' + c' + 1)
let init n = init_nsquare n n
let mt = init 4
let () = print_matrix mt
let () = print_string "\n\n"



(* 4: A function transpose that transposes a generic matrix independently of its size and content. *)
let transpose (l: matrix) =
  let ncolumns = get_nrows l in
  let nrows = get_ncolumns l in
  make_matrix nrows ncolumns (fun r' c' nrows' ncolumns' -> matrix_nth c' r' l)

let mt2 = init_nsquare 3 2
let () = print_matrix mt2
let () = print_string "\n"
let tp = transpose mt2
let () = print_matrix tp
let () = print_string "\n\n"



(* 5: The basics operators + and * that adds and multiplies two matrices non necessarily squared. *)
let ( + ) l1 l2 = 
  let nrows1 = get_nrows l1 in
  let ncolumns1 = get_ncolumns l1 in
  let nrows2 = get_nrows l2 in
  let ncolumns2 = get_ncolumns l2 in
  if nrows1 = nrows2 && ncolumns1 = ncolumns2
    then Some (make_matrix nrows1 ncolumns1 (fun r' c' nrows' ncolumns' -> matrix_nth r' c' l1 + matrix_nth r' c' l2))
    else None

let m1 = init_nsquare 3 2
let m2 = init_nsquare 3 2
let sum = match (m1 + m2) with
  | None -> []
  | Some v -> v
let () = print_matrix sum


let ( * ) l1 l2 = []