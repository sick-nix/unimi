(* 
sin(x) can be approximate by the Taylor's series.
Similarly you can approximate all the trigonometric and transcendent functions
  (look at: http://en.wikipedia.org/wiki/Taylor_series).
Let's write a module to implement sin x n by using the Taylor's series
  (where n is the level of approximation, i.e., 1 only one item, 2 two items, 3 three items and so on).
  Do the same with cosine, tangent, logarithm and so on.
Let's compare your functions with those implemented in the pervasive module at the growing of the approximation level.
*)

let deg_to_rad x =
  180. /. x |> Float.div Float.pi

let fact x = 
  let rec fact_acc x' acc = match x' with
    | 1 -> acc
    | n -> fact_acc (x' - 1) (acc * n)
  in
    fact_acc x 1

let pow x n =
  let rec pow_acc n' acc = match n' with
    | 0 -> acc
    | n'' -> pow_acc (n'' - 1) (acc *. x)
  in
    pow_acc n 1.

let round2 n = Float.round (n *. 100.) /. 100.

let sin approx x =
  let rec sin_acc n acc = match n with
    | -1 -> acc
    | y ->
      let num = pow (-1.) y in
      let additional = 2*y+1 in
      let den = fact additional in
      let other = pow (round2 x) additional in
      sin_acc (y - 1) (acc +. (num /. float_of_int(den) *. other))
    in sin_acc approx 0. |> round2

let () = deg_to_rad 180. |> sin 8 |> print_float
let () = print_string "\n"

let cos approx x =
  let rec cos_acc n acc = match n with
    | 0 -> 1. +. acc
    | y ->
      let num = pow (-1.) y in
      let additional = 2*y in
      let den = fact additional in
      let other = pow (round2 x) additional in
      cos_acc (y - 1) (acc +. (num /. float_of_int(den) *. other))
    in cos_acc approx 0. |> round2

let () = deg_to_rad 180. |> cos 8 |> print_float
let () = print_string "\n"

let exp approx x =
  let rec exp_acc n acc = match n with
    | 0 -> 1. +. acc
    | y ->
      let num = Float.pow x (float_of_int(y)) in
      let den = fact y in
      exp_acc (y - 1) (acc +. (num /. float_of_int(den)))
    in exp_acc approx 0. |> round2

let () = exp 8 2. |> print_float
let () = print_string "\n"

let ln n x = ()