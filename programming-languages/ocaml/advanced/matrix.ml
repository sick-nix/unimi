(* 
You have to write a module Matrix to represent and manipulate integer matrices.
The module should be parametric and will provide some operations.
Override the appropriate operators and raise the appropriate exceptions.

We first define these operations, and then give a skeleton of the Matrix module
  showing the signatures for all methods and constructors you must implement.

The operations
Let aᵢⱼ denotes the i,j-th element of matrix A, located at row i, column j.
Using this notation, the matrix operations listed above may be defined precisely as follows:
  matrix equivalence A ≃ B where A in Ζᵐˣⁿ, B in Zp×q when m = p and n = q and bᵢⱼ = aᵢⱼ for i = 1,...,m j = 1,...,n;
  matrix copy B = A where A, B in Ζᵐˣⁿ (bᵢⱼ = aᵢⱼ for i = 1,...,m j = 1,...,n);
  matrix addition C = A + B where A, B, C in Ζᵐˣⁿ (cᵢⱼ = aᵢⱼ + bᵢⱼ for i = 1,...,m j = 1,...,n);
  scalar-matrix multiplication B = aA where A, B in Ζᵐˣⁿ, a in Z (bᵢⱼ = a·aᵢⱼ for i = 1,...,m j = 1,...,n);
  matrix-matrix multiplication C = A·B where A in Zᵐˣᵖ, B in Zᵖˣⁿp×n,
    C in Ζᵐˣⁿ (cᵢⱼ = Σₖ=1, ..., p aᵢₖ·bₖⱼ for i = 1,...,m j = 1,...,n);
  matrix transposition B = AT where A in Ζᵐˣⁿ, B in Zⁿˣᵐ (bⱼᵢ = aᵢⱼ for i = 1,...,m j = 1,...,n);
  matrix norm (matrix 1-norm) a = ‖A‖ where a in Z, A in Ζᵐˣⁿ (a = maxⱼ Σᵢ | aᵢⱼ | for i = 1,...,m j = 1, ..., n).
  Note that in each case we state the proper matrix dimensions for the operation to be valid.
  For example, when multiplying two matrices A and B, the number of columns of A must match the number of rows of B. 
*)

module type NumericType = sig
  type t
  val to_string: t -> string
  val zero: t
  val add: t -> t -> t
  val mul: t -> t -> t
  val abs: t -> t
  val max: t -> t -> t
end


module Matrix = functor (Elt: NumericType) -> struct
  type t = Elt.t
  type matrix = t list list

  let nth r c (l: matrix)  =
    let rec nth_opt (acc: int) (i: int) = function
    | [] -> None
    | h :: t when i = acc -> Some h
    | h :: t -> nth_opt (acc + 1) i t
    in let nth_list i l = nth_opt 0 i l
    in let val_of_nth i l = match nth_list i l with
    | None -> Elt.zero
    | Some v -> v
    in let list_of_nth i l = match nth_list i l with
    | None -> []
    | Some v -> v
    in list_of_nth r l |> val_of_nth c

  let get_nrows l = List.length l

  let rec get_ncolumns l = match List.nth_opt l 0 with
  | None -> 0
  | Some v -> List.length v

  let rec print (l: matrix) =
    let rec print_list = function
    | [] -> ()
    | h :: t ->
      Elt.to_string h |> print_string;
      print_string "\t";
      print_list t
    in let rec print_matrix_inner = function
    | [] -> ()
    | h :: t ->
      print_list h;
      print_string "\n";
      print_matrix_inner t
    in print_matrix_inner l

  let make nrows ncolumns (p: int -> int -> int -> int -> t) =
    let rec make_list_acc r c nrows ncolumns p = match c with
      | x when x = ncolumns -> []
      | x -> p r c nrows ncolumns :: make_list_acc r (c + 1) nrows ncolumns p
    in let rec make_list r nrows ncolumns p = make_list_acc r 0 nrows ncolumns p
    in let rec make_matrix_acc r nrows ncolumns p = match r with
    | x when x = nrows -> []
    | x -> make_list r nrows ncolumns p :: make_matrix_acc (r + 1) nrows ncolumns p
    in make_matrix_acc 0 nrows ncolumns p

  let empty nrows ncolumns = make nrows ncolumns (fun r' c' nrows' ncolumns' -> Elt.zero)

  let equals (l1: matrix) (l2: matrix) = l1 = l2

  let transpose l =
    let ncolumns = get_nrows l
    in let nrows = get_ncolumns l
    in make nrows ncolumns (fun r' c' nrows' ncolumns' -> nth c' r' l)

  let copy l =
    let ncolumns = get_nrows l
    in let nrows = get_ncolumns l
    in make nrows ncolumns (fun r' c' nrows' ncolumns' -> nth r' c' l)

  let add l1 l2 =
    let nrows1 = get_nrows l1 in
    let ncolumns1 = get_ncolumns l1 in
    let nrows2 = get_nrows l2 in
    let ncolumns2 = get_ncolumns l2 in
    match nrows1 = nrows2 && ncolumns1 = ncolumns2 with
    | false -> None
    | true -> Some (make nrows1 ncolumns1 (fun r' c' nrows' ncolumns' -> Elt.add (nth r' c' l1) (nth r' c' l2)))

  let smul n l =
    let ncolumns = get_nrows l
    in let nrows = get_ncolumns l
    in make nrows ncolumns (fun r' c' nrows' ncolumns' -> Elt.mul (nth r' c' l) n)

  let mmul l1 l2 =
    let m = get_nrows l1 in
    let n = get_ncolumns l1 in
    let n' = get_nrows l2 in
    let p = get_ncolumns l2 in
    match n = n' with
    | false -> None
    | true ->
      let sum_ij i j =
        let rec sum_ij_acc k acc = match k = n with
        | true -> acc
        | false -> sum_ij_acc (k+1) (Elt.add (nth i k l1) (nth k j l2) |> Elt.add acc)
        in sum_ij_acc 0 Elt.zero
      in Some (make m p (fun r' c' nrows' ncolumns' -> sum_ij r' c'))

  let norm l =
    let tr = transpose l
    in let rec get_max l' max = match l' with
    | [] -> max
    | h :: t ->
      let m = List.map (fun n -> Elt.abs n) h |> List.fold_left Elt.add Elt.zero
      in get_max t (Elt.max max m)
    in get_max tr Elt.zero   
end

module IntMatrix = Matrix(Int)


(* Tests *)
let empty = IntMatrix.empty 4 4
let () = IntMatrix.print empty; print_string "\n"

let m1 = IntMatrix.make 4 4 (fun r c nrows ncolumns -> r * ncolumns + c + 1)
let () = IntMatrix.print m1; print_string "\n"

let t1 = IntMatrix.transpose m1
let () = IntMatrix.print t1; print_string "\n"

let s1 = match IntMatrix.add m1 t1 with
| None -> []
| Some v -> v
let () = IntMatrix.print s1; print_string "\n"

let c1 = IntMatrix.copy m1
let () = match IntMatrix.equals m1 c1 with
| false -> print_string "Not equals\n\n"
| true -> print_string "Equals\n\n"

let sm1 = IntMatrix.smul 3 m1
let () = IntMatrix.print sm1; print_string "\n"

let l1 = IntMatrix.make 5 4 (fun r c nrows ncolumns -> r * ncolumns + c + 1)
let l2 = IntMatrix.make 4 5 (fun r c nrows ncolumns -> r * ncolumns + c + 1)
let mm1 = match IntMatrix.mmul l1 l2 with
| None -> []
| Some v -> v
let () = IntMatrix.print mm1; print_string "\n"

let l3 = [[5;-4;2]; [-1;2;3]; [-2;1;0]]
let n1 = IntMatrix.norm l3
let () = print_int n1; print_string "\n"

let l4 = [[1;-7]; [-2;-3]]
let n2 = IntMatrix.norm l4
let () = print_int n2; print_string "\n"